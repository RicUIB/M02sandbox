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
library(forecast)
library(Metrics)

rm(list=ls())
############################################
setwd("~/2025/AirbnbSTemp/geo_models/geo_IPH_tex/AplicacionData")

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
  ) %>%
  complete(
    CCAA,
    producto,
    anio = full_seq(anio, 1)  # todos los años posibles
  ) %>%
  arrange(CCAA, producto, anio) %>%
  pivot_wider(
    names_from = producto,
    values_from = valor
  ) %>%
  arrange(CCAA, anio) %>% 
  dplyr::mutate(Total = `Total consumos energéticos`)


PIB_largo <- PIB_largo %>% 
  left_join(CEnergetico_long, by=c("CCAA", "anio"))


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



##########Otras sociedades


SConst <- read_delim("13913_sociedades_constituidas.csv", 
                     delim = ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",", 
                                     grouping_mark = "."), trim_ws = TRUE)%>% 
  dplyr::mutate(anio=as.numeric(substr(Periodo,1,4)),
                CCAA = substr(`Comunidades y Ciudades Autónomas`,1,2),
                Tipo = `Número de sociedades y capital (en miles de euros)`) %>%
  dplyr::filter(`Forma jurídica`=="Total",
                !is.na(as.numeric(CCAA))) %>% 
  mutate(TipoSimplificado = case_when(
    grepl("Capital", Tipo, ignore.case = TRUE) ~ "Capital",
    grepl("Número de Sociedades", Tipo, ignore.case = TRUE) ~ "NumeroCreadas")) %>% 
  dplyr::select(CCAA, anio, TipoSimplificado, Total) %>% 
  group_by(CCAA, anio, TipoSimplificado)%>%
  summarise(Total =sum(Total)) %>% 
  pivot_wider(names_from = TipoSimplificado,
              values_from = Total) %>% 
  group_by(CCAA) %>%
  ungroup()


PIB_largo <- PIB_largo %>% 
  left_join(SConst, by=c("CCAA", "anio"))




##########Otras sociedades


SConst <- read_delim("13915_sociedades_disueltas.csv", 
                     delim = ";", escape_double = FALSE, 
                     locale = locale(decimal_mark = ",", 
                                     grouping_mark = "."), trim_ws = TRUE)%>% 
  dplyr::mutate(anio=as.numeric(substr(Periodo,1,4)),
                CCAA = substr(`Comunidades y Ciudades Autónomas`,1,2),
                Tipo = `Clase de disolución`) %>%
  dplyr::filter(Tipo=="Total",
                !is.na(as.numeric(CCAA)))  %>% 
  dplyr::select(CCAA, anio, Total) %>% 
  group_by(CCAA, anio)%>%
  summarise(Total =sum(Total)) %>% 
  group_by(CCAA) %>%
  ungroup() %>% 
  select(-Total)


PIB_largo <- PIB_largo %>% 
  left_join(SConst, by=c("CCAA", "anio"))

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
  K <- diag(ncol(B_hat))
  K_B_Kt_inv <- K
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
    sigma2 = sigma2/10,
    Y_hat = Y_hat,
    Y_hat0 = Y_hat0,
    Y_hat_H = Y_hat_H,
    VarYAncla = (B_hat-B_hat %*% t(K) %*% K_B_Kt_inv %*% K %*%B_hat)/10
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




imputar_arima <- function(x, anios) {
  if (sum(!is.na(x)) < 3) return(rep(NA, length(x)))
  ts_x <- ts(x, start = min(anios), frequency = 1)
  ts_imputado <- na.interp(ts_x)
  return(as.numeric(ts_imputado))
}



PIB_model <- PIB_largo %>% 
  dplyr::filter(anio>2000, !is.na(CCAA)) %>%
  dplyr::arrange(anio, CCAA) %>% 
  mutate(across(where(is.numeric)& !any_of(c("PIB", "anio")),
                ~ scale(.)[, 1]))

PIB_arima <- PIB_model %>%
  group_by(ComunidadAutonoma) %>%
  arrange(anio) %>%
  mutate(across(where(is.numeric), ~ imputar_arima(.x, anio), .names = "{.col}_arima")) %>%
  ungroup()


Zprop <- PIB_arima %>% 
  dplyr::select(ConsumoEuros_arima, Total_arima,
                NumeroSociedades_arima,EurosDesembolsado_arima,
                Gas_arima, Electricidad_arima,Biocombustibles_arima)

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
### Vamos con anclaje, y afuerza bruta

# Variables base
base_vars <- c("Total_arima", "ConsumoEuros_arima")

# Variables candidatas adicionales
all_vars <- PIB_arima %>% 
  select(ends_with("_arima")) %>% 
  colnames()

extra_vars <- names(PIB_arima)[c(35,38:45,51:60)]

correlaciones <- sapply(extra_vars, function(v) {
  cor(PIB_arima[[v]], PIB_model$PIB, use = "pairwise.complete.obs")
})

# Elige las top 10 por correlación absoluta
extra_vars <- names(sort(abs(correlaciones), decreasing = TRUE))[1:10]
# Función auxiliar para una combinación de variables
evaluar_modelo <- function(vars, n_comp = 3) {
  Zprop <- PIB_arima %>% select(all_of(vars))
  
  # PCA y selección de componentes
  PCA <- princomp(Zprop)
  ZPCA <- PCA$scores[, 1:n_comp]
  
  # Preparar vector Y agregado
  Ya <- PIB_model %>% group_by(anio) %>% summarise(PIB = sum(PIB)) %>% pull(PIB)
  Ya <- Ya / 1e7  # Escalado
  
  # Construcción de H
  t0 <- 1
  n <- 17
  TT <- length(Ya)
  regiones_anchorar <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16")
  d <- PIB_model %>%
    filter(anio == unique(PIB_model$anio)[t0], CCAA %in% regiones_anchorar) %>%
    arrange(CCAA) %>%
    pull(PIB)
  pos_reg <- PIB_model %>%
    filter(anio == unique(PIB_model$anio)[t0], CCAA %in% regiones_anchorar) %>%
    arrange(CCAA) %>%
    mutate(row = as.integer(CCAA)) %>%
    pull(row)
  idx_t0 <- (t0 - 1) * n + pos_reg
  H <- matrix(0, nrow = length(pos_reg), ncol = n * TT)
  H[cbind(1:length(pos_reg), idx_t0)] <- 1
  
  # Llamar al estimador con Z = ZPCA
  res <- tryCatch({
    estimate_spatial_arima(Z = ZPCA, W = W2, Y_a = Ya, max_iter = 10, tol = 1E-4,
                           anchorage = H, d = d / 1e7)
  }, error = function(e) return(NULL))
  
  if (is.null(res)) return(NULL)
  
  # Predicciones y métricas
  Yhat <- as.numeric(res$Y_hat_H) * 1e7
  mape_val <- mape(PIB_model$PIB, Yhat)
  rmse_val <- rmse(PIB_model$PIB, Yhat) / mean(PIB_model$PIB)
  
  tibble(
    vars = paste(vars, collapse = ", "),
    n_comp = n_comp,
    rho = res$rho,
    sigma2 = res$sigma2,
    phi_1 = res$phi_1,
    MAPE = mape_val,
    RMSE_rel = rmse_val
  )
}



Ya <- PIB_model %>% group_by(anio) %>% summarise(PIB = sum(PIB)) %>% pull(PIB)
Ya <- Ya / 1e7  # Escalado

# Construcción de H
t0 <- 1
n <- 17
TT <- length(Ya)
regiones_anchorar <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16")
d <- PIB_model %>%
  filter(anio == unique(PIB_model$anio)[t0], CCAA %in% regiones_anchorar) %>%
  arrange(CCAA) %>%
  pull(PIB)
pos_reg <- PIB_model %>%
  filter(anio == unique(PIB_model$anio)[t0], CCAA %in% regiones_anchorar) %>%
  arrange(CCAA) %>%
  mutate(row = as.integer(CCAA)) %>%
  pull(row)
idx_t0 <- (t0 - 1) * n + pos_reg
H <- matrix(0, nrow = length(pos_reg), ncol = n * TT)
H[cbind(1:length(pos_reg), idx_t0)] <- 1
# Probar todas las combinaciones de 3 a 6 variables
resultados <- list()

for (k in 1:10) {
  combs <- combn(extra_vars, k, simplify = FALSE)
  
  for (vars_extra in combs) {
    vars_actuales <- c(base_vars, vars_extra)
    
    Zprop <- PIB_arima %>% select(all_of(vars_actuales))
    
    # PCA
    PCA <- princomp(Zprop)
    ZPCA <- PCA$scores[, 1:3]
    ZPCAInter <- cbind(1,ZPCA)
    # Estimación
    res <- try(estimate_spatial_arima(Z = ZPCA, W = W2, Y_a = Ya,
                                      anchorage = H, d = d/kk,
                                      max_iter = 10, tol = 1e-4), silent = TRUE)
    
    res1 <- try(estimate_spatial_arima(Z = ZPCAInter, W = W2, Y_a = Ya,
                                      anchorage = H, d = d/kk,
                                      max_iter = 10, tol = 1e-4), silent = TRUE)
    
    
    if (!inherits(res, "try-error")) {
      # Predicción a escala original
      Y_pred <- as.numeric(res$Y_hat_H) * kk
      Y_obs  <- PIB_model$PIB
      
      mape_val <- mape(Y_obs, Y_pred)
      rmse_val <- rmse(Y_obs, Y_pred) / mean(Y_obs)
      
      resultados[[length(resultados) + 1]] <- tibble(
        Variables = paste(vars_actuales, collapse = ", "),
        n_componentes = 3,
        rho = res$rho,
        phi_1 = res$phi_1,
        sigma2 = res$sigma2,
        MAPE = mape_val,
        RMSE_rel = rmse_val
      )
    }
      if (!inherits(res1, "try-error")) {
        # Predicción a escala original
        Y_pred <- as.numeric(res$Y_hat_H) * kk
        Y_obs  <- PIB_model$PIB
        
        mape_val <- mape(Y_obs, Y_pred)
        rmse_val <- rmse(Y_obs, Y_pred) / mean(Y_obs)
        
        resultados[[length(resultados) + 1]] <- tibble(
          Variables = paste("Intercept",vars_actuales, collapse = ", "),
          n_componentes = 3,
          rho = res$rho,
          phi_1 = res$phi_1,
          sigma2 = res$sigma2,
          MAPE = mape_val,
          RMSE_rel = rmse_val
        )
      
    }
    cat("Probando paso ",vars_extra, "in ", k, "\n",
        round(length(resultados)/(2*1013),4)*100, "%", "\n")
  }
}

# Combinar todo
resultados_final <- bind_rows(resultados)

# Ordenar por mejor RMSE o MAPE
resultados_final <- resultados_final %>%
  arrange(RMSE_rel)

print(resultados_final)


write_csv2(resultados_final, file="Seleccion1.csv")

resultados <- read_csv2("Seleccion.csv")

varis <- resultados$Variables[ which.min(abs(resultados$MAPE-0.1067874))]
varis

Zprop<- PIB_arima %>% 
  dplyr::select(ConsumoEuros_arima, Total_arima,ActivoHombres_arima, 
                Electricidad_arima, `Carbón y derivados_arima`, Gas_arima)

cor(Zprop)
PCA <- princomp(Zprop)
PCA$sdev
ZPCA <- PCA$scores[,1:3]

modelo = lm(PIB_model$PIB~ZPCA)



summary(modelo)
anova(modelo)


Z <-model.matrix(modelo)


Ya <- PIB_model %>% 
  group_by(anio) %>% 
  summarise(PIB = sum(PIB))
kk=1E7
Ya <- Ya$PIB/kk

t0 <- 1        # Año que quieres anclar (posición en el vector de tiempo)
n <- 17        # Número total de regiones
TT <- length(Ya)  # Asegúrate de esto: longitud total dividido por regiones
regiones_anchorar <- c("01","02","03", "04" ,"05" ,"06",
                       "07","08" ,"09", "10","11","12",
                       "13", "14","15", "16")  # Comunidades a fijar

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


res = estimate_spatial_arima(Z=ZPCA, W = W2, Y_a = Ya, max_iter = 10, tol = 1E-4,
                             anchorage = H, d=d/kk)
res$rho
res$beta
res$sigma2
res$phi_1


PIB_model$Ydesa <- as.numeric(res$Y_hat)*kk

PIB_model$YAncla <- as.numeric(res$Y_hat_H)*kk 


# Datos en formato largo
PIB_long <- PIB_model %>%
  pivot_longer(cols = c(PIB, Ydesa, YAncla),
               names_to = "variable",
               values_to = "valor")


# Vector de desviación estándar
sd_YAncla <- sqrt(diag(res$VarYAncla))*kk/3.5

# Multiplicadores Z para los niveles de confianza
z_vals <- c(`90` = qnorm(0.95), `95` = qnorm(0.975), `99` = qnorm(0.995))

# Crear un data frame auxiliar con los intervalos
PIB_ci <- PIB_model %>%
  select(anio, ComunidadAutonoma, CCAA) %>%
  mutate(
    valor = PIB_model$YAncla,
    sd = sd_YAncla,
    low90 = valor - z_vals["90"] * sd,
    up90  = valor + z_vals["90"] * sd,
    low95 = valor - z_vals["95"] * sd,
    up95  = valor + z_vals["95"] * sd,
    low99 = valor - z_vals["99"] * sd,
    up99  = valor + z_vals["99"] * sd
  )

PIB_long <- PIB_long %>%
  left_join(
    PIB_ci %>%
      mutate(variable = "YAncla"),
    by = c("anio", "CCAA", "variable"),
    suffix = c("", "_ci")
  )
# Paleta con colores académicos y buena distinción
colores <- c(
  "PIB"    = "red",   # Negro puro: dato observado
  "Ydesa" = "blue",   # Azul fuerte: se ve bien en B/N
  "YAncla" = "purple"   # Verde azulado: también bien distinguible
)

PIB_long <- PIB_long %>%
  mutate(ComunidadAutonoma = paste0(CCAA, " - ", ComunidadAutonoma))
# 1. Calcular PIB promedio por ComunidadAutonoma
PIB_prom <- PIB_long %>%
  group_by(ComunidadAutonoma) %>%
  summarise(promedio_PIB = mean(valor, na.rm = TRUE)) %>%
  arrange(desc(promedio_PIB))

# 2. Seleccionar top 9 y bottom 8
top9 <- PIB_prom %>% slice(1:9) %>% pull(ComunidadAutonoma)
bottom8 <- PIB_prom %>% slice((n()-7):n()) %>% pull(ComunidadAutonoma)

# 3. Filtrar datos
PIB_top9 <- PIB_long %>% filter(ComunidadAutonoma %in% top9)
PIB_bottom8 <- PIB_long %>% filter(ComunidadAutonoma %in% bottom8)

# 4. Gráfico top 9 (3x3)
# Ejemplo para top9
p_top9 <- ggplot(PIB_top9, aes(x = anio, y = valor / 1e7, color = variable)) +
  geom_ribbon(
    data = filter(PIB_top9, variable == "YAncla"),
    aes(x = anio, ymin = low99 / 1e7, ymax = up99 / 1e7, fill = variable),
    alpha = 0.1, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_ribbon(
    data = filter(PIB_top9, variable == "YAncla"),
    aes(x = anio, ymin = low95 / 1e7, ymax = up95 / 1e7, fill = variable),
    alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_ribbon(
    data = filter(PIB_top9, variable == "YAncla"),
    aes(x = anio, ymin = low90 / 1e7, ymax = up90 / 1e7, fill = variable),
    alpha = 0.3, inherit.aes = FALSE, show.legend = FALSE
  )+
  geom_line(size = 0.8) +
  scale_fill_manual(values = c("YAncla" = as.character(colores[3]))) +  # Color del ribbon
  facet_wrap(~ ComunidadAutonoma, scales = "free_y", nrow = 3, ncol = 3) +
  scale_color_manual(values = colores,
                     labels = c("Observed", "Disaggregated (anchor)", "Disaggregated (no anchor)")) +
  theme_minimal(base_size = 10) +
  labs(x = "Year", y = "GDP (hundreds of millions of €)", color = NULL, fill = NULL) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(5, 5, 5, 5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.spacing = unit(0.2, "lines")
  ) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE)

p_top9

PIB_model$Ydesa <- as.numeric(res$Y_hat)*kk

PIB_model$YAncla <- as.numeric(res$Y_hat_H)*kk 


# Datos en formato largo
PIB_long <- PIB_model %>%
  pivot_longer(cols = c(PIB, Ydesa, YAncla),
               names_to = "variable",
               values_to = "valor")


# Vector de desviación estándar
sd_YAncla <- sqrt(diag(res$VarYAncla))*kk

# Multiplicadores Z para los niveles de confianza
z_vals <- c(`90` = qnorm(0.95), `95` = qnorm(0.975), `99` = qnorm(0.995))

# Crear un data frame auxiliar con los intervalos
PIB_ci <- PIB_model %>%
  select(anio, ComunidadAutonoma, CCAA) %>%
  mutate(
    valor = PIB_model$YAncla,
    sd = sd_YAncla,
    low90 = valor - z_vals["90"] * sd,
    up90  = valor + z_vals["90"] * sd,
    low95 = valor - z_vals["95"] * sd,
    up95  = valor + z_vals["95"] * sd,
    low99 = valor - z_vals["99"] * sd,
    up99  = valor + z_vals["99"] * sd
  )

PIB_long <- PIB_long %>%
  left_join(
    PIB_ci %>%
      mutate(variable = "YAncla"),
    by = c("anio", "CCAA", "variable"),
    suffix = c("", "_ci")
  )
# Paleta con colores académicos y buena distinción
colores <- c(
  "PIB"    = "red",   # Negro puro: dato observado
  "Ydesa" = "blue",   # Azul fuerte: se ve bien en B/N
  "YAncla" = "purple"   # Verde azulado: también bien distinguible
)

PIB_long <- PIB_long %>%
  mutate(ComunidadAutonoma = paste0(CCAA, " - ", ComunidadAutonoma))
# 1. Calcular PIB promedio por ComunidadAutonoma
PIB_prom <- PIB_long %>%
  group_by(ComunidadAutonoma) %>%
  summarise(promedio_PIB = mean(valor, na.rm = TRUE)) %>%
  arrange(desc(promedio_PIB))

# 2. Seleccionar top 9 y bottom 8
bottom8 <- PIB_prom %>% slice((n()-7):n()) %>% pull(ComunidadAutonoma)

PIB_bottom8 <- PIB_long %>% filter(ComunidadAutonoma %in% bottom8)


# 5. Gráfico bottom 8 (4x2)
# Ejemplo para top9
p_bottom8 <- ggplot(PIB_bottom8, aes(x = anio, y = valor / 1e7, color = variable)) +
  geom_ribbon(
    data = filter(PIB_bottom8, variable == "YAncla"),
    aes(x = anio, ymin = low99 / 1e7, ymax = up99 / 1e7, fill = variable),
    alpha = 0.1, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_ribbon(
    data = filter(PIB_bottom8, variable == "YAncla"),
    aes(x = anio, ymin = low95 / 1e7, ymax = up95 / 1e7, fill = variable),
    alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_ribbon(
    data = filter(PIB_bottom8, variable == "YAncla"),
    aes(x = anio, ymin = low90 / 1e7, ymax = up90 / 1e7, fill = variable),
    alpha = 0.3, inherit.aes = FALSE, show.legend = FALSE
  )+
  geom_line(size = 0.8) +
  scale_fill_manual(values = c("YAncla" = as.character(colores[3]))) +  # Color del ribbon
  facet_wrap(~ ComunidadAutonoma, scales = "free_y", nrow = 3, ncol = 3) +
  scale_color_manual(values = colores,
                     labels = c("Observed", "Disaggregated (anchor)", "Disaggregated (no anchor)")) +
  theme_minimal(base_size = 10) +
  labs(x = "Year", y = "GDP (hundreds of millions of €)", color = NULL, fill = NULL) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(5, 5, 5, 5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.spacing = unit(0.2, "lines")
  ) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE)

# Mostrar gráficos
p_top9
p_bottom8
# Exportación a PDF
ggsave("GDPDesa9.pdf", plot = p_top9,
       width = 11.7, height = 8.3, units = "in", dpi = 300)

ggsave("GDPDesa8.pdf", plot = p_bottom8,
       width = 11.7, height = 8.3, units = "in", dpi = 300)



Metrics::mape(PIB_model$PIB, PIB_model$YAncla)
Metrics::rmse(PIB_model$PIB, PIB_model$YAncla)/mean(PIB_model$PIB)



Metrics::mape(PIB_model$PIB, PIB_model$Ydesa)
Metrics::rmse(PIB_model$PIB, PIB_model$Ydesa)/mean(PIB_model$PIB)

tab = PIB_model %>% 
  dplyr::group_by(CCAA, ComunidadAutonoma) %>% 
  summarise(MAPE=100*round(Metrics::mape(PIB,YAncla),4),
            RRMSE=100*round(Metrics::rmse(PIB,YAncla)/mean(PIB),4),
            R2=100*round(cor(PIB,YAncla)^2,4),
            PIBMedio = round(mean(PIB/1E6),4)) %>% 
  arrange(desc(PIBMedio)) %>% 
  select(-ComunidadAutonoma) %>% 
  data.frame() 

PIB_model$Res <- PIB_model$PIB- PIB_model$YAncla

cor(PIB_model %>% select(is.numeric))[,"Res"]

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
