
download_temp <- tempdir()
library(magrittr)
ENG_url <- "https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Regions_December_2019_Boundaries_EN_BUC/MapServer/0/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson"
UK_url <- "https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Countries_December_2019_Boundaries_UK_BUC/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson"
download.file(ENG_url, file.path(download_temp, "ENG.geojson"))
download.file(UK_url, file.path(download_temp, "UK.geojson"))
ENG_data <- sf::st_read(file.path(download_temp, "ENG.geojson")) %>%
  dplyr::mutate(ctry_reg = rgn19nm) %>%
  dplyr::select(-objectid, -rgn19cd, -rgn19nm)
UK_data <- sf::st_read(file.path(download_temp, "UK.geojson")) %>%
  dplyr::filter(ctry19cd != "E92000001") %>%
  dplyr::mutate(ctry_reg = ctry19nm) %>%
  dplyr::select(-objectid, -ctry19cd, -ctry19nm, -ctry19nmw)
UK_country_region_sf <- rbind(UK_data, ENG_data)

UK_country_region_sf <- dplyr::mutate(
  UK_country_region_sf,
  ctry_reg_abb = sapply(regmatches(ctry_reg, gregexpr("[A-Z]", ctry_reg)), paste, collapse = ""))


usethis::use_data(UK_country_region_sf, overwrite = TRUE)
rm(download_temp)



