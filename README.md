# Muestreo de Humedad - Proyecto de Rasterización

Este proyecto toma datos de humedad de un archivo CSV y genera un mapa rasterizado utilizando técnicas de interpolación (IDW) en R.

## Estructura del Proyecto

- `datos_humedad.csv`: Archivo con datos de humedad y coordenadas.
- `perimetro_LS3/`: Carpeta que contiene el shapefile del perímetro del lote.
- `script/muestreo.R`: Script en R que realiza el proceso de rasterización.

## Requisitos

- R (versión 4.0 o superior)
- Paquetes: `sf`, `raster`, `ggplot2`, `gstat`, `leaflet`

## Instrucciones

1. Clona el repositorio:
   ```bash
   git clone https://github.com/tu_usuario/muestreo_humedad.git
