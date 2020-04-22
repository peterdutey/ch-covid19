



get_db_conn <- function() {
  getOption("fshc_warehouse")
}

#' Create \code{reference_homes} table
#'
#' @param conn a database connection
#' @param source
#' @return
#' @export
create_table_reference_homes <- function(conn = get_db_conn()){

  xlsx::read.xlsx(paste0(getOption("FSHC_EXTRACTS_DIRECTORY"), "reference_2020-04-22.xlsx"),
                  sheetName = "Homes")

}

create_geo_reference_table <- function(conn = get_db_conn(), postcodes){
  PostcodesioR::bulk_postcode_lookup(postcodes)
}
