
#' Simple feature map of UK countries and English regions
#'
#' A dataset containing the ultra generalised clip boundaries of UK countries
#' and regions of England as of 2019
#'
#' @format An sf object with two polygon name variables:
#' \describe{
#'   \item{ctry_reg}{name of the country or region (in the case of England)}
#'   \item{ctry_reg_abb}{three-letter abbreviation of the country/region name}
#' }
#' @details Source: Office for National Statistics licensed under the
#' Open Government Licence v.3.0
#' Contains OS data (c) Crown copyright and database right [2020]
#' @source UK countries: \url{https://geoportal.statistics.gov.uk/datasets/countries-december-2019-boundaries-uk-buc}
#' English regions: \url{https://geoportal.statistics.gov.uk/datasets/regions-december-2019-boundaries-en-buc}
"UK_country_region_sf"
