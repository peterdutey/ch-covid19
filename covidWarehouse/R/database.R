



get_db_conn <- function() {
  getOption("fshc_warehouse")
}

#' Create \code{reference_homes} table
#'
#' @param conn a database connection
#' @param overwrite whether to overwrite the table (default is TRUE)
#' @return the name of the table if data was successfully loaded
#' @importFrom dplyr tbl
#' @export
create_table_reference_homes <- function(conn = get_db_conn(), overwrite = TRUE){

  homes <- xlsx::read.xlsx(
    paste0(getOption("FSHC_EXTRACTS_DIRECTORY"), "reference_2020-04-22.xlsx"),
    sheetName = "Homes", stringsAsFactors = F)
  names(homes) <- gsub("[.]+", "_", tolower(names(homes)))

  dbplyr::db_copy_to(con = conn,
                     table = "reference_homes",
                     values = homes,
                     overwrite = overwrite,
                     temporary = FALSE,
                     indexes = list(
                       "home_code",
                       "postcode"
                     ))

}


#' Title
#'
#' @param conn a database connection
#' @param postcodes a character vector of postcodes. If `NULL` (the default),
#' data will be fetched from the `reference_homes` table.
#' @param overwrite whether to overwrite the `reference_geography` table.
#' Default is `TRUE`.
#' @importFrom magrittr %>%
#' @return
#' @export
#' @importFrom dplyr tbl
create_table_reference_geography <- function(conn = get_db_conn(),
                                       postcodes = NULL,
                                       overwrite = TRUE){
  if(is.null(postcodes)) {
    postcodes <- tbl(get_db_conn(), "reference_homes")
    postcodes <- dplyr::collect(postcodes)$postcode
    postcodes <- unique(postcodes)
  }

  geo_reference <- list()

  for(i in 0:trunc(length(postcodes)/100)) {
    geo_reference[[i+1]] <- PostcodesioR::bulk_postcode_lookup(
      list(postcodes = postcodes[(i*100):min(i*100+99,
                                             length(postcodes))])
      )
  }

  geo_reference <- unlist(geo_reference, recursive = F)
  geo_reference <- lapply(geo_reference, function(X) {
    output <- X$result[!sapply(X$result, is.null)]
    output.main <- dplyr::as_tibble(output[grep("codes", names(output), invert = T)])
    if("codes" %in% names(output)){
      output.codes <- output$codes[!sapply(output$codes, is.null)]
      names(output.codes) <- paste0(names(output.codes), "_code")
    } else {
      output.codes <- NULL
    }
    return(dplyr::bind_cols(output.main, output.codes))
  })
  geo_reference <- dplyr::bind_rows(geo_reference)

  geo_reference <- geo_reference[, c(
    grep("_code$", colnames(geo_reference), invert = TRUE),
    grep("_code$", colnames(geo_reference))
  )]
  geo_reference
  dbplyr::db_copy_to(con = conn,
                     table = "reference_geography",
                     values = geo_reference,
                     overwrite = overwrite,
                     temporary = FALSE,
                     indexes = list(
                       "postcode"
                     ))

}
