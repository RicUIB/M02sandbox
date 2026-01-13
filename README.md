


Ojetivos prioritarios: data2,data3,data4,

Unified: Municipio, ID INE (4 columnas), Fechas formato YYYY-MM-DD 

### Comentarios
El raw to tidy de data2 no está ubicado en su carpeta
### Datos

| ID | Datos | Fuente | Estado | Link |
|----------|---------------------------------|----------|----------|----------|
| data1 | Alojamientos Turisticos por municipio | CAIB | unified | [Link](https://www.caib.es/sites/estadistiquesdelturisme/ca/allotjaments_turastics/) |
| data2 | IPH diario por isla | IBESTAT | raw | [Link](https://ibestat.es/edatos/apps/statistical-visualizer/visualizer/collection.html?resourceType=collection&agencyId=IBESTAT&resourceId=000011A_000001) |
| data3 | Turismo interno y receptor por municipio de destino | INE | raw | [Link](https://www.ine.es/experimental/turismo_moviles/experimental_turismo_moviles.htm) |
| data4 | Población censada por municipio por sexo | INE | raw | [Link](https://www.ine.es/pob_xls/pobmun.zip) |
| \- | Movimientos turísticos en frontera | FRONTUR | wip | [Link](https://ibestat.es/estadistica/economia/turismo/flujo-de-turistas-frontur/) |
| data5 | Ocupación de Calvià, Capdepera, Palma y St Llorenç | INE | raw | [Link](https://ine.es/jaxiT3/Tabla.htm?t=2076) |
| data7 | Turistas con destino principal las Baleares | IBESTAT | wip | [Link](https://ibestat.es/edatos/apps/statistical-visualizer/visualizer/data.html?resourceType=dataset&agencyId=IBESTAT&resourceId=000058A_000002&version=~latest#visualization/table) |
| data6 | Mapas y Cartografía | CNIG | tidy | [Link](https://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=CAANE) |

-raw: solo está la tabla descargada (en raw_data)

-tidy: se ha dado formato de csv/RData y los datos están organizados (en tidy_data)

-unified: datos tidy con formato común de municipio, ID, año, formato snake_tail en inglés (en unified_data)

<!---

### Datos disponibles

| **Ubicación** | **Datos** | **Comentarios** | **Link** |
|-----------------|--------------------|--------------------|-----------------|
| - | (IBESTAT) IPH | - |[Link](https://ibestat.es/edatos/apps/statistical-visualizer/visualizer/collection.html?resourceType=collection&agencyId=IBESTAT&resourceId=000011A_000001) |


| \\Adrian\\INE_municipios | (INE) data Población municipio sexos y año. | Copia del generado por Ricardo | [Link](https://www.ine.es/pob_xls/pobmun.zip) |


| \\Adrian\\IBESTAT | (IBESTAT) Turistas con destino principal las Illes Balears por lugar de residencia. | Conversión de XSLX a RData | [Link](https://ibestat.es/edatos/apps/statistical-visualizer/visualizer/data.html?resourceType=dataset&agencyId=IBESTAT&resourceId=000058A_000002&version=~latest#visualization/table) |


| WIP | (FRONTUR) Movimientos turísticos en Frontera | \- | [Link](https://ibestat.es/estadistica/economia/turismo/flujo-de-turistas-frontur/) |


| \\Adrian\\INE | (INE) movilidad a partir de la telefonía móvil | Conversión de XSLX a RData. Tres RData: emisor, receptor, interno | [Link](https://www.ine.es/experimental/turismo_moviles/experimental_turismo_moviles.htm) |


| \\Adrian\\CAIB | (CAIB) Alojamientos turísticos por municipio | Solo totales por municipio | [Link](https://www.caib.es/sites/estadistiquesdelturisme/ca/allotjaments_turastics/) |


| \- | (INE) Ocupación turística de Calvià, Capdepera, Palma y Sant Llorenç del Cardassar | \- | [Link](https://ine.es/jaxiT3/Tabla.htm?t=2076) |


| \- | \- | \- | \- |




La plataforma [Inside Airbnb](http://insideairbnb.com/) recopila y pone a disposición información detallada sobre apartamentos y residencias vacacionales en alquiler en diversas ubicaciones alrededor del mundo. Este sitio constituye una herramienta valiosa para analizar la oferta de Airbnb en distintas regiones, desde provincias y departamentos hasta condados y barrios.

Los datos disponibles son de acceso libre bajo una [licencia Open Source](https://www.redhat.com/es/topics/open-source/what-is-open-source), lo que permite su uso y análisis siempre que se respeten las condiciones especificadas en la [licencia del proyecto](http://insideairbnb.com/about.html). Si quieres sumergirte en el mundo de los datos y explorar sus posibilidades, puedes comenzar desde la [página principal de Inside Airbnb](http://insideairbnb.com/get-the-data.html) o consultar los recursos específicos que se detallan a continuación.

#### Recursos de Datos Disponibles

- **[Obtener datos](http://insideairbnb.com/get-the-data.html):** Enlace directo para descargar los datos.
- **[Diccionario de Datos](https://docs.google.com/spreadsheets/d/1iWCNJcSutYqpULSQHlNyGInUvHg2BoUGoNRIGa6Szc4/edit#gid=982310896):** Documentación detallada sobre las variables disponibles.
- **[Políticas de Datos](http://insideairbnb.com/data-policies.html):** Información sobre la disponibilidad de datos en relación con la misión del proyecto y las directrices de la comunidad.
- **[Solicitud de Datos](http://insideairbnb.com/data-requests.html):** Herramienta para acceder a datos archivados o de nuevas regiones (ten en cuenta que este servicio es de pago para datos de más de un año de antigüedad).

#### ¡Atención!
Si deseas consultar datos históricos que excedan un año, deberás recurrir al servicio de solicitud de datos, el cual implica un costo.

---

### Acceso y Descarga de los Datos

A través del enlace **[Get the data](http://insideairbnb.com/get-the-data.html)** puedes descargar diferentes archivos para cada ciudad, descritos en la siguiente tabla:

| **Nombre del Archivo**       | **Descripción**                                                                                      |  
|-------------------------------|------------------------------------------------------------------------------------------------------|  
| **listings.csv.gz**           | Datos detallados de las propiedades en la ciudad seleccionada.                                       |  
| **calendar.csv.gz**           | Datos detallados sobre la disponibilidad y precios de las propiedades en la ciudad.                 |  
| **reviews.csv.gz**            | Datos detallados de las reseñas relacionadas con las propiedades en la ciudad.                      |  
| **listings.csv**              | Información resumida y métricas generales, ideal para visualizaciones.                              |  
| **reviews.csv**               | Datos resumidos de reseñas, útiles para análisis temporales vinculados a cada propiedad.            |  
| **neighbourhoods.csv**        | Listado de vecindarios utilizados como filtro geográfico, basado en datos de la ciudad o fuentes GIS. |  
| **neighbourhoods.geojson**    | Archivo GeoJSON con las delimitaciones de los vecindarios de la ciudad.                             |  

¡Explorar estos datos es una gran oportunidad para visualizar y analizar las dinámicas del mercado de alquiler vacacional en distintas regiones del mundo! 🎯


-->
