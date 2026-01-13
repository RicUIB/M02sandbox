###########################################
#### Intento de desagregación del PIB #####

library(tidyverse)
library(readxl)
library(zoo)
library(sf)
library(spdep)
library(pls)
library(Matrix)
library(car)
library(cluster)
library(gt)  
rm(list=ls())
############################################
setwd("C:/Users/UIB/Downloads/PIB")

PIB_Total <- read_excel("PIB_Total.xlsx")
PIB_largo <- PIB_Total %>%
  pivot_longer(
    cols = matches("^\\d{4}|\\d{4} \\(.*\\)"),  # columnas tipo "2000" o "2022 (P)"
    names_to = "anio",
    values_to = "PIB"
  ) %>%
  mutate(
    anio = str_extract(anio, "^\\d{4}") %>% as.integer()
  )


PIB_largo <- PIB_largo %>% 
  dplyr::filter(Provincia=="Total") %>% 
  dplyr::select(ComunidadAutonoma, CCAA, anio, PIB)


#############################
### COnsumo en Euros 


ValorConsumo <- read_delim("ValorConsumo.csv", 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                           trim_ws = TRUE) %>% 
  dplyr::select(CCAA, Anio, ConsumoEuros)

PIB_largo <- PIB_largo %>% 
  left_join(ValorConsumo, by=c("CCAA"="CCAA", "anio"="Anio"))


################# IPI


IPI <- read_delim("IndicedeProduccionIndustrial.csv", 
                  delim = ";", escape_double = FALSE, col_types = cols(Total = col_number()), 
                  locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE)
IPI <- IPI %>% 
  dplyr::mutate(anio = as.numeric(substr(Periodo,1,4)),
                IPI = Total) %>% 
  dplyr::filter(`Índice y tasas`=="Índice", anio>2001) %>% 
  dplyr::group_by(CodigoCCAA, anio) %>% 
  dplyr::summarise(IPI=mean(IPI, na.rm=TRUE))

PIB_largo <- PIB_largo %>% 
  left_join(IPI, by=c("CCAA"="CodigoCCAA", "anio"="anio"))



Poblacion <- read_delim("Poblacion.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)  %>% 
  dplyr::mutate(CCAA = substr(`Comunidades y Ciudades Autonomas`,1,2),
                anio=Periodo) %>% 
  dplyr::filter(!is.na(CCAA)) %>% 
  pivot_wider(
    names_from = Sexo,
    values_from = Total
  ) %>% 
  dplyr::mutate(PobHombres = Hombres,
                PobMujeres = Mujeres,
                PobTotal = Total) %>% 
  dplyr::select(CCAA, anio, PobHombres, PobMujeres, PobTotal)


pob_nested <- Poblacion %>%
  group_by(CCAA) %>%
  nest()

# Paso 2: modelo lineal simple por CCAA y predicción
pob_forecast <- pob_nested %>%
  mutate(
    model = map(data, ~ lm(PobTotal ~ anio, data = .x)),
    pred_data = map(data, ~ tibble(anio = 2022:2023)),
    pred = map2(model, pred_data, ~ mutate(.y, PobTotal = predict(.x, newdata = .y)))
  ) %>%
  select(CCAA, pred) %>%
  unnest(pred)

# Paso 3: unir con los datos originales
Poblacion <- bind_rows(Poblacion, pob_forecast) %>%
  arrange(CCAA, anio)
  

PIB_largo <- PIB_largo %>% 
  left_join(Poblacion, by=c("CCAA"="CCAA", "anio"="anio"))





CEnergetico <- read_delim("COnsumoEnergetico.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)


# Paso 1: extraer código si existe al principio
CEnergetico <- CEnergetico %>%
  mutate(
    CCAA = str_extract(`Comunidades y Ciudades Autónomas`, "^\\d{2}")
  )

# Paso 2: crear tabla de correspondencia nombre -> código usando las filas que sí tienen código
tabla_codigos <- CEnergetico %>%
  filter(!is.na(CCAA)) %>%
  mutate(nombre_limpio = str_remove(`Comunidades y Ciudades Autónomas`, "^\\d{2} ") %>% str_trim()) %>%
  select(CCAA, nombre_limpio) %>%
  distinct()

# Paso 3: unir con la tabla original para rellenar los códigos faltantes
CEnergetico <- CEnergetico %>%
  mutate(nombre_limpio = str_remove(`Comunidades y Ciudades Autónomas`, "^\\d{2} ") %>% str_trim()) %>%
  left_join(tabla_codigos, by = "nombre_limpio", suffix = c("", ".y")) %>%
  mutate(
    CCAA = coalesce(CCAA, CCAA.y)
  ) %>%
  select(-nombre_limpio, -CCAA.y) %>%
  filter(!is.na(CCAA)) %>%  # Eliminar filas como "Total Nacional"
  select(CCAA, `Producto consumido`, Período, Total) %>%
  rename(
    producto = `Producto consumido`,
    anio = Período,
    valor = Total
  ) %>%
  pivot_wider(
    names_from = producto,
    values_from = valor
  ) %>%
  arrange(CCAA, anio)


# Paso 1: pasar de wide a long
CEnergetico_long <- CEnergetico %>%
  pivot_longer(
    cols = -c(CCAA, anio),
    names_to = "producto",
    values_to = "valor"
  )

# Paso 2: completar todas las combinaciones CCAA × producto × año
CEnergetico_expandido <- CEnergetico_long %>%
  complete(
    CCAA,
    producto,
    anio = full_seq(anio, 1)  # todos los años posibles
  ) %>%
  arrange(CCAA, producto, anio)

# Paso 3: interpolar los valores faltantes
CEnergetico_imputado <- CEnergetico_expandido %>%
  group_by(CCAA, producto) %>%
  mutate(
    valor = zoo::na.approx(valor, anio, na.rm = FALSE),
    valor = zoo::na.locf(valor, na.rm = FALSE),         # completar hacia adelante
    valor = zoo::na.locf(valor, fromLast = TRUE, na.rm = FALSE)  # hacia atrás
  ) %>%
  ungroup()

# Paso 4: volver a formato wide
CEnergetico_final <- CEnergetico_imputado %>%
  pivot_wider(
    names_from = producto,
    values_from = valor
  ) %>%
  arrange(CCAA, anio) %>% 
  dplyr::mutate(Total = `Total consumos energéticos`)


PIB_largo <- PIB_largo %>% 
  left_join(CEnergetico_final, by=c("CCAA", "anio"))


#### Numero de sociedades

NumeroSociedades <- read_delim("NumeroSociedades.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  dplyr::mutate(anio=as.numeric(substr(Periodo,1,4)),
                CCAA = substr(`Comunidades y Ciudades Autónomas`,1,2),
                Tipo = `Número de sociedades y capital (en miles de euros)`) %>%
  dplyr::filter(`Forma jurídica`=="Total",
                !is.na(as.numeric(CCAA)), is.na(Provincias)) %>% 
  mutate(TipoSimplificado = case_when(
    grepl("desembolsado", Tipo, ignore.case = TRUE) ~ "EurosDesembolsado",
    grepl("suscrito", Tipo, ignore.case = TRUE) ~ "EurosSuscritos",
    grepl("Número", Tipo, ignore.case = TRUE) ~ "NumeroSociedades")) %>% 
  dplyr::select(CCAA, anio, TipoSimplificado, Total) %>% 
  pivot_wider(names_from = TipoSimplificado,
              values_from = Total)

PIB_largo <- PIB_largo %>% 
  left_join(NumeroSociedades, by=c("CCAA", "anio"))

#### Indice de precios



IPC <- read_delim("IPC.csv", delim = ";", 
                  escape_double = FALSE, locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE)%>% 
  dplyr::mutate(anio=as.numeric(substr(Periodo,1,4)),
                CCAA = substr(`Comunidades y Ciudades Autónomas`,1,2)) %>%
  dplyr::filter(`Grupos ECOICOP`=="Índice general", `Tipo de dato`=="Índice",
                !is.na(as.numeric(CCAA))) %>% 
  group_by(CCAA,anio) %>% 
  summarise(IPCMedio = mean(Total, na.rm=TRUE)) 


PIB_largo <- PIB_largo %>% 
  left_join(IPC, by=c("CCAA", "anio"))



ActivosAbsolutos <- read_delim("ActivosAbsolutos.csv", 
                               delim = ";", escape_double = FALSE, 
                               col_types = cols(Total = col_number()), 
                               trim_ws = TRUE) %>% 
  dplyr::mutate(anio=as.numeric(substr(Periodo,1,4))) %>% 
  dplyr::filter(Edad=="Total",CodigoCCAA != "To" ) %>% 
  group_by(Sexo, CodigoCCAA,anio) %>% 
  summarise(ActAbs = mean(Total, na.rm=TRUE)) %>% 
  pivot_wider(
    names_from = Sexo,
    values_from = ActAbs
  ) %>% 
  dplyr::mutate(ActivoHombres = Hombres,
                ActivosMujeres = Mujeres) %>% 
  dplyr::select(CodigoCCAA, anio, ActivoHombres, ActivosMujeres)
  

PIB_largo <- PIB_largo %>% 
  left_join(ActivosAbsolutos, by=c("CCAA"="CodigoCCAA", "anio"="anio"))



TAActividad <- read_delim("TasadeActividad.csv", 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                          trim_ws = TRUE) %>% 
  dplyr::mutate(anio=as.numeric(substr(Periodo,1,4))) %>% 
  dplyr::filter(Edad=="Total",CodigoCCAA != "To" ) %>% 
  group_by(Sexo, CodigoCCAA,anio) %>% 
  summarise(ActAbs = mean(Total, na.rm=TRUE)) %>% 
  pivot_wider(
    names_from = Sexo,
    values_from = ActAbs
  )%>% 
  dplyr::mutate(TasaHombres = Hombres,
                TasaMujeres = Mujeres) %>% 
  dplyr::select(CodigoCCAA, anio, TasaHombres, TasaMujeres)



PIB_largo <- PIB_largo %>% 
  left_join(TAActividad, by=c("CCAA"="CodigoCCAA", "anio"="anio"))


PIB_model <- PIB_largo %>% 
  dplyr::filter(anio>2001, !is.na(CCAA)) %>% 
  mutate(across(where(is.numeric)& !any_of(c("PIB", "anio")),
                ~ scale(.)[, 1]))
modelo = lm(PIB/1E7~PobTotal+ConsumoEuros+IPI+Total+Gas+Biocombustibles+
              Electricidad+ActivoHombres+
              ActivosMujeres+TasaHombres+TasaMujeres+IPCMedio+
              NumeroSociedades+EurosSuscritos+EurosDesembolsado, data=PIB_model)
summary(modelo)
anova(modelo)


### Construir la matrix de distancias
ccaa_sf <- st_read("Espana/Comunidades_Autonomas_ETRS89_30N.shp")  # asegúrate que contiene las 17 CCAA

# Revisar nombres únicos
ccaa_sf <- ccaa_sf %>% dplyr::filter(!Codigo%in%c("18", "19"))
nb_q <- poly2nb(ccaa_sf, queen = TRUE)
nombres <- ccaa_sf$Codigo  # o la columna de nombre real
which(nombres == "04")        # i_baleares
which(nombres == "09")              # i_catalunya
which(nombres == "10")  # i_valencia
which(nombres == "05")              # i_canarias
which(nombres == "13")  # i_madrid

# Supón:
i_baleares <- which(nombres == "04")
i_catalunya <- which(nombres == "09")
i_valencia <- which(nombres == "10")
i_canarias <- which(nombres == "05")
i_madrid <- which(nombres == "13")

# Añadir vecinos a la lista nb_q
nb_q[[i_baleares]] <- unique(c(nb_q[[i_baleares]], i_catalunya, i_valencia))
nb_q[[i_catalunya]] <- unique(c(nb_q[[i_catalunya]], i_baleares))
nb_q[[i_valencia]] <- unique(c(nb_q[[i_valencia]], i_baleares))

nb_q[[i_canarias]] <- unique(c(nb_q[[i_canarias]], i_madrid))
nb_q[[i_madrid]] <- unique(c(nb_q[[i_madrid]], i_canarias))

W <- nb2mat(nb_q, style = "W", zero.policy = TRUE)  # estilo "W" = filas normalizadas
corrplot::corrplot(W, is.corr = FALSE)


### Otra matrix

datos <- read.delim("datos.txt", header = TRUE, sep = "\t") %>% 
  dplyr::filter(!CCAA%in%c("Ceuta", "Melilla"))
datos$CCAA <- c("01", "02", "03", "06","07", "08", "05", "09",
                "11", "12", "04","14", "13", "15", "16", "17",
                "10")
datos <- datos %>%
  dplyr::arrange(CCAA) %>% 
  mutate(CCAA = as.factor(CCAA)) %>%
  group_by(CCAA)

rownames(datos) <- datos$CCAA
gower_dist <- daisy(datos, metric = "gower")
gower_matrix <- as.matrix(gower_dist)
W3 <- ifelse(gower_matrix<median(as.vector(gower_matrix)),1,0)
diag(W3) <- 0
W3 <- W3/rowSums(W3)
gower_matrix <- 1/as.matrix(gower_dist)
diag(gower_matrix) <- 0
W2<- gower_matrix/rowSums(gower_matrix)


corrplot::corrplot(W2, is.corr = FALSE)
###################################
### Las funciones de estimacion ####


arima110_covariance <- function(phi1, sigma2, TT) {
  # Construir matriz de autocovarianzas de la serie diferenciada w_t = Δu_t ~ AR(1)
  Gamma_w <- matrix(0, TT, TT)
  for (i in 1:TT) {
    for (j in 1:TT) {
      lag <- abs(i - j)
      Gamma_w[i, j] <- sigma2 * phi1^lag / (1 - phi1^2)
    }
  }
  return(Gamma_w)
}

estimate_spatial_arima <- function(Z, W, Y_a, max_iter = 100,
                                   tol = 1e-6, rhoInt =c(-0.999, 0.999), 
                                   anchorage = NULL, d=NULL ) {
  
  n <- nrow(W)
  TT <- length(Y_a)
  k <- ncol(Z)
  
  # Define C = I_T \otimes 1_n^T
  C <- kronecker(Diagonal(TT), matrix(1, nrow = 1, ncol = n))
  
  # Initialize parameters
  beta <- matrix(0, nrow = k, ncol = 1)
  rho <- 0
  
  # Step 2: ARIMA(1,1,2) to Y_a
  #arima_fit <- Arima(Y_a, order = c(1, 1, 0))
  phi_1 <- 0.01
  sigma2 <- 1
  
  # Start iteration
  for (iter in 1:max_iter) {
    # Step 3: estimate beta given phi_1, sigma2, rho
    I_n <- Diagonal(n)
    
    
    # Step 4: optimize rho
    loglik_rho <- function(rho_val) {
      rho1 = rho_val[1]
      phi1 = rho_val[2]
      sigma1 = rho_val[3]
      Sigma_U <- arima110_covariance(phi1, sigma1, TT)
      A_inv <- kronecker(Diagonal(TT), solve(I_n - rho1 * W))
      mu <- C %*% A_inv %*% Z
      Sigma_Y <- C %*% A_inv %*% kronecker(Sigma_U, Diagonal(n)) %*% t(A_inv) %*% t(C)
      # GLS estimation of beta
      beta_new <- solve(t(mu) %*% solve(Sigma_Y) %*% mu) %*% t(mu) %*% solve(Sigma_Y) %*% Y_a
      mu_rho <- C %*% A_inv %*% Z %*% beta_new
      res <- Y_a - mu_rho
      dd = -0.5 * log(det(Sigma_Y)) - 0.5 * t(res) %*% solve(Sigma_Y) %*% res
      
      dd = max(dd[1,1], -1e10)
      return(-dd)
    }
    
    rhoNew1 <- optim(c(0,0,1),loglik_rho, lower = c(rhoInt[1], -0.999,0.01), 
                     upper=c(rhoInt[2], 0.999, 10E4), method = "L-BFGS-B")
    
    #rhoNew1 <- optim(c(0,0,1),loglik_rho, lower = c(-0.299, -0.999,0.01), 
    #                upper=c(0.299, 0.999, 10E4), method = "L-BFGS-B")
    rho_new= rhoNew1$par[1]
    phi_1_new= rhoNew1$par[2]
    sigma2_new= rhoNew1$par[3]
    
    # Check convergence
    if (abs(rho_new - rho) < tol && 
        abs(phi_1_new - phi_1) < tol &&
        abs(sigma2_new - sigma2) < tol) {
      break
    }
    
    # Update values
    rho <- rho_new
    phi_1 <- phi_1_new
    sigma2 <- sigma2_new
  }
  
  # Step 7: Estimate disaggregated Y
  A_hat_inv <- solve(kronecker(Diagonal(TT), I_n - rho * W))
  Sigma_U <- arima110_covariance(phi_1, sigma2, TT)
  mu <- C %*% A_hat_inv %*% Z
  Sigma_Y <- C %*% A_hat_inv %*% kronecker(Sigma_U, Diagonal(n)) %*% t(A_hat_inv) %*% t(C)
  
  beta <- solve(t(mu) %*% solve(Sigma_Y) %*% mu) %*% t(mu) %*% solve(Sigma_Y) %*% Y_a
  # Step 5: update phi_1 and sigma2 from residuals
  
  B_hat <- A_hat_inv %*% kronecker(Sigma_U,I_n ) %*% t(A_hat_inv)
  Y_hat0 <- A_hat_inv %*% Z %*% beta
  Y_hat <- A_hat_inv %*% Z %*% beta+ B_hat %*% t(C) %*% solve(C %*% B_hat %*% t(C)) %*% (Y_a - C %*% A_hat_inv %*% Z %*% beta)
  Y_hat_H <- Y_hat0
  if(!is.null(anchorage)){
    K <- rbind(C, anchorage)
    K_B_Kt_inv <- solve(K%*% B_hat %*% t(K))
    y_obs <- c(Y_a, d)
    correction <- K %*% Y_hat0
    Y_hat_H <- Y_hat0 + B_hat %*% t(K) %*% K_B_Kt_inv %*% (y_obs - correction)
  }
  
  return(list(
    beta = beta,
    rho = rho,
    phi_1 = phi_1,
    sigma2 = sigma2,
    Y_hat = Y_hat,
    Y_hat0 = Y_hat0,
    Y_hat_H = Y_hat_H
  ))
}


p1 <- PIB_largo %>% 
  dplyr::filter(!is.na(CCAA)) %>% 
  ggplot(aes(x=anio, y=PIB, colour=CCAA))+
  geom_line()+
  scale_y_log10()

p1



p2 <- PIB_largo %>% 
  dplyr::filter(!is.na(CCAA)) %>% 
  ggplot(aes(x=anio, y=ConsumoEuros, colour=CCAA))+
  geom_line()+
  scale_y_log10()

p2








PIB_model <- PIB_largo %>% 
  dplyr::filter(anio>2001, !is.na(CCAA)) %>%
  dplyr::arrange(anio, CCAA) %>% 
  mutate(across(where(is.numeric)& !any_of(c("PIB", "anio")),
                ~ scale(.)[, 1]))


Zprop <- PIB_model %>% 
  dplyr::select(ConsumoEuros, Total,
                NumeroSociedades,EurosDesembolsado,
                Gas, Electricidad)

cor(Zprop)
PCA <- princomp(Zprop)
PCA$sdev
ZPCA <- PCA$scores[,1:3]

modelo = lm(PIB_model$PIB~ZPCA)



summary(modelo)
anova(modelo)


Z <-model.matrix(modelo)
#Z <-model.matrix(modelo0)

Ya <- PIB_model %>% 
  group_by(anio) %>% 
  summarise(PIB = sum(PIB))
kk=1E7
Ya <- Ya$PIB/kk
res = estimate_spatial_arima(Z=Z, W = W2, Y_a = Ya, max_iter = 10, tol = 1E-4)
res$rho
res$beta
res$sigma2
res$phi_1


PIB_model$Ydesa <- as.numeric(res$Y_hat)*kk

PIB_model$YAncla <- as.numeric(res$Y_hat_H)*kk 



PIB_long <- PIB_model %>%
  pivot_longer(cols = c(PIB, Ydesa, YAncla),
               #pivot_longer(cols = c(PIB, Ydesa),
               names_to = "variable",
               values_to = "valor")

# Graficamos: una faceta por comunidad, tres líneas por comunidad
ggplot(PIB_long, aes(x = anio, y = valor/1E7, color = variable)) +
  geom_line(size = 1) +
  facet_wrap(~ ComunidadAutonoma, scales = "free_y") +
  theme_minimal(base_size = 10) +
  labs(x = "Año", y = NULL, color = "Variable") +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0, NA))


Metrics::mape(PIB_model$PIB, PIB_model$YAncla)
Metrics::rmse(PIB_model$PIB, PIB_model$YAncla)/mean(PIB_model$PIB)

#### Funcion mal, pero funciona
### Campo de pruebas
### Vamos con anclaje

Ya <- PIB_model %>% 
  group_by(anio) %>% 
  summarise(PIB = sum(PIB))
kk=1E7
Ya <- Ya$PIB/kk

t0 <- 3        # Año que quieres anclar (posición en el vector de tiempo)
n <- 17        # Número total de regiones
TT <- length(Ya)  # Asegúrate de esto: longitud total dividido por regiones
regiones_anchorar <- c("01", "09", "13")  # Comunidades a fijar

# Vector con valores conocidos para esas regiones en año t0
d <- PIB_model %>%
  filter(anio == unique(PIB_model$anio)[t0],
         CCAA %in% regiones_anchorar) %>%
  arrange(CCAA) %>%
  pull(PIB)

# Posiciones de esas regiones en el orden global (asumiendo que CCAA está ordenado como en Y)
pos_reg <- PIB_model %>%
  filter(anio == unique(PIB_model$anio)[t0],
         CCAA %in% regiones_anchorar) %>%
  arrange(CCAA) %>%
  mutate(row = as.integer(CCAA)) %>%
  pull(row)

# Posiciones globales en el vector Y:
idx_t0 <- (t0 - 1) * n + pos_reg

# H: matriz con 1s en esas posiciones
H <- matrix(0, nrow = length(pos_reg), ncol = n * TT)
H[cbind(1:length(pos_reg), idx_t0)] <- 1


res = estimate_spatial_arima(Z=Z, W = W2, Y_a = Ya, max_iter = 10, tol = 1E-4,
                             anchorage = H, d=d/kk)
res$rho
res$beta
res$sigma2
res$phi_1


PIB_model$Ydesa <- as.numeric(res$Y_hat)*kk

PIB_model$YAncla <- as.numeric(res$Y_hat_H)*kk 



PIB_long <- PIB_model %>%
  pivot_longer(cols = c(PIB, Ydesa, YAncla),
               #pivot_longer(cols = c(PIB, Ydesa),
               names_to = "variable",
               values_to = "valor")

# Graficamos: una faceta por comunidad, tres líneas por comunidad
ggplot(PIB_long, aes(x = anio, y = valor/1E7, color = variable)) +
  geom_line(size = 1) +
  facet_wrap(~ ComunidadAutonoma, scales = "free_y") +
  theme_minimal(base_size = 10) +
  labs(x = "Año", y = NULL, color = "Variable") +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0, NA))



Metrics::mape(PIB_model$PIB, PIB_model$YAncla)
Metrics::rmse(PIB_model$PIB, PIB_model$YAncla)

PIB_model %>% 
  dplyr::group_by(CCAA, ComunidadAutonoma) %>% 
  summarise(MAPE=100*round(Metrics::mape(PIB,YAncla),2),
            R2=100*round(cor(PIB,YAncla)^2,2),
            PIBMedio = mean(PIB/1E6)) %>% 
  arrange(desc(PIBMedio))


########## Anclar

anclajes <- data.frame(
  CCAA = c("01", "01", "09", "13", "01", "09", "13","13"),
  anio = c(2002, 2005, 2005, 2005, 2019, 2019, 2019, 2022))


# Obtener posiciones absolutas en Y (vectorizado por tiempo)
anclajes <- anclajes %>%
  left_join(
    PIB_model %>% 
      distinct(CCAA) %>%
      mutate(pos_region = row_number()),
    by = "CCAA"
  ) %>%
  mutate(pos_Y = (anio - 2001) * n + pos_region)

# Extraer valores conocidos d
d <- PIB_model %>%
  semi_join(anclajes, by = c("CCAA", "anio")) %>%
  arrange(match(paste(CCAA, anio), paste(anclajes$CCAA, anclajes$anio))) %>%
  pull(PIB)

# Construir matriz H
H <- matrix(0, nrow = nrow(anclajes), ncol = n * TT)
H[cbind(1:nrow(anclajes), anclajes$pos_Y)] <- 1



res = estimate_spatial_arima(Z=Z, W = W2, Y_a = Ya, max_iter = 10, tol = 1E-4,
                             anchorage = H, d=d/kk)
res$rho
res$beta
res$sigma2
res$phi_1


PIB_model$Ydesa <- as.numeric(res$Y_hat)*kk

PIB_model$Ydesa2 <- as.numeric(res$Y_hat0)*kk 



PIB_long <- PIB_model %>%
  pivot_longer(cols = c(PIB, Ydesa, Ydesa2),
               #pivot_longer(cols = c(PIB, Ydesa),
               names_to = "variable",
               values_to = "valor")

# Graficamos: una faceta por comunidad, tres líneas por comunidad
ggplot(PIB_long, aes(x = anio, y = valor/1E7, color = variable)) +
  geom_line(size = 1) +
  facet_wrap(~ ComunidadAutonoma, scales = "free_y") +
  theme_minimal(base_size = 10) +
  labs(x = "Año", y = NULL, color = "Variable") +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0, NA))


Metrics::mape(PIB_model$PIB, PIB_model$Ydesa)
Metrics::rmse(PIB_model$PIB, PIB_model$Ydesa)
