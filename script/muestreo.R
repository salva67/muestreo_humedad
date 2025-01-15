# Cargar librerías
library(lwgeom)
library(terra)
library(sf)
library(dplyr)
library(psych)
library(e1071)
library(smoothr)
library(units)
library(ggplot2)
library(future)
library(future.apply)
library(leaflet)
library(htmlwidgets)
library(raster)
library(gridExtra)





###################################################################################

rasterizar_idw <- function(input_csv, field, 
                           geom_col = "geometry", 
                           crs_original = 4326, crs_destino = 32722, 
                           resolucion = 10, idp = 2, nmax = 12, 
                           lote_perimetro = NULL, 
                           lote_name = "lote", 
                           output_dir = NULL, plot_result = TRUE, save_pdf = TRUE) {
  library(dplyr)
  library(ggplot2)
  library(raster)
  library(sf)
  library(gstat)
  library(gridExtra)
  library(rasterVis)
  
  cat("\n--- INICIO DEL PROCESO ---\n")
  
  # Leer CSV y convertir geometrías
  cat("\nLeyendo el archivo CSV...\n")
  data <- read.csv(input_csv, stringsAsFactors = FALSE)
  data <- st_as_sf(data, wkt = geom_col, crs = crs_original)
  data_reprojected <- st_transform(data, crs = crs_destino)
  
  # Leer el perímetro del lote
  if (!is.null(lote_perimetro)) {
    lote <- st_read(lote_perimetro)
    lote <- st_transform(lote, crs = crs_destino)
  } else {
    lote <- st_as_sfc(st_bbox(data_reprojected))
  }
  
  # Crear grilla base
  cat("\nCreando grilla base para rasterizar...\n")
  raster_base <- raster(extent(lote), res = resolucion, crs = st_crs(lote)$proj4string)
  raster_grid <- as(raster_base, "SpatialPixelsDataFrame")
  proj4string(raster_grid) <- st_crs(crs_destino)$proj4string  # Asegurar CRS
  
  # Crear directorio de salida
  dir_resultados <- file.path(output_dir, lote_name)
  if (!dir.exists(dir_resultados)) dir.create(dir_resultados, recursive = TRUE)
  
  # Interpolación IDW
  cat("\nRealizando interpolación IDW...\n")
  idw_result <- gstat::idw(as.formula(paste(field, "~ 1")), 
                           as(data_reprojected, "Spatial"), 
                           newdata = raster_grid, 
                           idp = idp, 
                           nmax = nmax)
  
  raster_idw <- raster(idw_result)
  proj4string(raster_idw) <- st_crs(crs_destino)$proj4string  # Asignar CRS
  
  # Aplicar máscara y recortar
  cat("\nAplicando máscara y recortando con el perímetro del lote...\n")
  raster_mask <- mask(raster_idw, lote)
  raster_recortado <- crop(raster_mask, extent(lote))
  
  # Guardar raster
  raster_path <- file.path(dir_resultados, paste0("raster_", lote_name, "_", field, ".tif"))
  cat("\nGuardando el raster recortado en:", raster_path, "\n")
  writeRaster(raster_recortado, filename = raster_path, format = "GTiff", overwrite = TRUE)
  
  # Categorizar por umbrales
  cat("\nCategorizando valores por umbrales...\n")
  umbrales <- c(minValue(raster_recortado), quantile(raster_recortado[], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), maxValue(raster_recortado))
  
  raster_categorico <- raster_recortado
  values(raster_categorico) <- cut(
    values(raster_recortado),
    breaks = umbrales,
    include.lowest = TRUE,
    labels = c("Baja", "Media-Baja", "Media-Alta", "Alta")
  )
  
  cat("Categorías asignadas correctamente.\n")
  # Convertir a data.frame
  df_raster <- as.data.frame(rasterToPoints(raster_recortado), xy = TRUE)
  colnames(df_raster)[3] <- "Humedad"
  
  df_categorico <- df_raster %>%
    mutate(Categoria = cut(Humedad, breaks = umbrales, include.lowest = TRUE, labels = c("Baja", "Media-Baja", "Media-Alta", "Alta")))
  
  # Generar gráficos
  cat("\nGenerando gráficos...\n")
  # Mapa rasterizado
  p1 <- ggplot(df_raster, aes(x = x, y = y, fill = Humedad)) +
    geom_tile() +
    scale_fill_gradient(low = "red", high = "blue") +
    coord_fixed() +
    labs(title = paste("Mapa Rasterizado -", field), fill = "Humedad") +
    theme_minimal()
  
  # Mapa categorizado
  p2 <- ggplot(df_categorico, aes(x = x, y = y, fill = Categoria)) +
    geom_tile() +
    scale_fill_manual(values = c("red", "orange", "yellow", "blue")) +
    coord_fixed() +
    labs(title = paste("Mapa Categorizado por Umbrales -", field), fill = "Categoría") +
    theme_minimal()
  
  # Exportar a PDF
  if (save_pdf) {
    pdf_path <- file.path(dir_resultados, paste0("reporte_", lote_name, "_", field, ".pdf"))
    cat("\nGuardando gráficos en PDF en:", pdf_path, "\n")
    pdf(pdf_path, width = 8, height = 12)
    grid.arrange(p1, p2, ncol = 1)
    dev.off()
  }
  
  cat("\n--- PROCESO COMPLETADO ---\n")
  
  return(list(
    raster_original = raster_idw,
    raster_recortado = raster_recortado,
    raster_categorico = raster_categorico
  ))
}


resultados <- rasterizar_idw(
  input_csv = "C:/Users/sanicosi/Desktop/geospatial/bicho_bolita/datos_humedad.csv",
  field = "Humedad_10cm",
  geom_col = "geometry",
  crs_original = 4326,
  crs_destino = 32722,
  resolucion = 5,
  idp = 5,
  nmax = 50,
  lote_perimetro = "C:/Users/sanicosi/Desktop/geospatial/bicho_bolita/perimetro_LS3/POLYGON.shp",
  output_dir = "C:/Users/sanicosi/Desktop/geospatial/salida",
  lote_name = "LS3",
  plot_result = TRUE,
  save_pdf = TRUE
)
