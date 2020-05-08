



get_db_conn <- function() {
  getOption("fshc_warehouse")
}

#' Create or overwrite the \code{reference_homes} table
#'
#' @param conn a database connection
#' @param overwrite whether to overwrite the table (default is TRUE)
#' @return the name of the table if data was successfully loaded
#' @importFrom dplyr tbl
#' @export
create_table_reference_homes <- function(homes = reference_homes,
                                         conn = get_db_conn(), overwrite = TRUE){

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


#' Create or overwrite \code{reference_geography} table
#'
#' @param conn a database connection
#' @param data a data frame, set to  `reference_geography` by default.
#' @param overwrite whether to overwrite the `reference_geography` table.
#' Default is `TRUE`.
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise select filter
#' @return
#' @export
#' @importFrom dplyr tbl
create_table_reference_geography <- function(conn = get_db_conn(),
                                       data = reference_geography,
                                       overwrite = TRUE){

  dbplyr::db_copy_to(con = conn,
                     table = "reference_geography",
                     values = data,
                     overwrite = overwrite,
                     temporary = FALSE,
                     indexes = list(
                       "postcode"
                     ))

}

#' Extract most recent flat files into R data objects
#'
#' @description Saves the most recent data into directory `getOption("fshc_files")`
#' as files `"residents.rda"`, `"incident.rda"`, `"occupancy.rda"`, `"beds.rda"`
#' @param reference_data a boolean indicating whether the reference data (one-off extracts)
#' should be extracted. The default is FALSE to save time.
#' @return nothing
#' @export
#' @name extract_data
extract_incidents <- function() {

  if(!file.exists(file.path(getOption("fshc_files"), "reference_homes.rda"))) {
    extract_reference_homes()
  }
  load(file.path(getOption("fshc_files"), "reference_homes.rda"))

  if(!file.exists(file.path(getOption("fshc_files"), "residents.rda"))) {
    extract_residents()
  }
  load(file.path(getOption("fshc_files"), "residents.rda"))

  incidents_filepath <- list.files(getOption("FSHC_EXTRACTS_DIRECTORY"), full.names = T)
  incidents_filepath <- sort(grep("Datix", incidents_filepath, value = T), decreasing = T)[1]
  incidents_src <- read.csv(incidents_filepath,
                            stringsAsFactors = F,na.strings = "")
  names(incidents_src) <- tolower(names(incidents_src))
  names(incidents_src) <- gsub("[.]+", "_", names(incidents_src))
  names(incidents_src) <- gsub("(_$)|(^_)", "", names(incidents_src))

  incidents <- incidents_src
  for(i in grep("(_date)|(date_of)", names(incidents))){
    incidents[,i] <- lubridate::dmy(incidents[,i])
  }
  incidents[which(incidents$age < 5), c("age", "date_of_birth")] <- NA

  home_codes <- dplyr::distinct(reference_homes,
                                home_code,
                                home_name_datix)
  home_codes <- dplyr::rename(home_codes, home_name = home_name_datix)
  incidents <- dplyr::rename(incidents, home_name = home)

  incidents <- dplyr::left_join(incidents, home_codes, by = "home_name")
  incidents$staff_indicator <- !incidents$resident_encryptedid %in% residents$encrypted_id

  incidents <- incidents %>%
    dplyr::group_by(resident_encryptedid) %>%
    dplyr::mutate(incident_rank = if_else(
      is.na(resident_encryptedid) | staff_indicator,
      NA_integer_,
      dplyr::dense_rank(incident_date)
    )) %>%
    dplyr::ungroup()

  type_codes <- data.frame(list(
    infection_covid_19_type_code = 1:5,
    infection_covid_19_type = c(
      'Symptoms, but no definite clinical diagnosis of COVID-19',
      'Confirmed formal clinical diagnosis of COVID-19',
      'No symptoms, but close contact with infected or isolating person',
      'No symptoms, but team member in high risk group (pregnant, >70yrs, etc.)',
      'No symptoms, but team member is a carer for someone in high risk group')
  ))
  incidents <- merge(incidents, type_codes, all.x = TRUE)

  save(incidents, file = file.path(getOption("fshc_files"), "incidents.rda"))

}

#' @rdname extract_data
extract_residents <- function() {

  residents_filepath <- list.files(getOption("FSHC_EXTRACTS_DIRECTORY"), full.names = T)
  residents_filepath <- sort(grep("Residents_as_at", residents_filepath, value = T), decreasing = T)[1]
  residents_src <- read.csv(residents_filepath,
                            stringsAsFactors = F,na.strings = "")
  names(residents_src) <- tolower(names(residents_src))
  names(residents_src) <- gsub("[.]+", "_", names(residents_src))
  names(residents_src) <- gsub("(_$)|(^_)", "", names(residents_src))

  residents <- residents_src
  for(i in grep("(_date)|(date_of)", names(residents))){
    residents[,i] <- lubridate::dmy(residents[,i])
  }

  residents$last_admission_date <- dplyr::if_else(
    is.na(residents$last_admission_date),
    residents$original_admission_date,
    residents$last_admission_date
  )

  names(residents)[grep("status_", names(residents))] <- "status"
  residents$status <- trimws(residents$status)

  save(residents, file = file.path(getOption("fshc_files"), "residents.rda"))
}

#' @rdname extract_data
extract_occupancy <- function() {

  occupancy_filepath <- list.files(getOption("FSHC_EXTRACTS_DIRECTORY"), full.names = T)
  occupancy_filepath <- grep(".csv$", occupancy_filepath, value = T)
  occupancy_filepath <- sort(grep("[wW]eekly [Oo]cc", occupancy_filepath, value = T), decreasing = T)[1]
  occupancy_src <- read.csv(occupancy_filepath,
                            stringsAsFactors = F,na.strings = "")
  names(occupancy_src) <- tolower(names(occupancy_src))
  names(occupancy_src) <- gsub("[.]+", "_", names(occupancy_src))

  beds <- dplyr::select(occupancy_src, home_code, dplyr::starts_with("beds_"))
  beds <- reshape(beds, direction = "long", idvar = "home_code",
            varying = 2:ncol(beds), v.names = "beds", sep = "_",
            timevar = "year",
            times = gsub("beds_", "", names(beds)[2:ncol(beds)]))
  row.names(beds) <- NULL

  occupancy <- dplyr::select(occupancy_src, home_code, dplyr::starts_with("x"))
  occupancy <- reshape(occupancy, direction = "long", idvar = "home_code",
                  varying = 2:ncol(occupancy), v.names = "occupancy", sep = "",
                  timevar = "week_ending",
                  times = lubridate::dmy(gsub("x", "", names(occupancy)[2:ncol(occupancy)])))
  row.names(occupancy) <- NULL

  save(beds, file = file.path(getOption("fshc_files"), "beds.rda"))
  save(occupancy, file = file.path(getOption("fshc_files"), "occupancy.rda"))
}

#' @rdname extract_data
#' @import PostcodesioR
extract_reference_homes <- function() {
  reference_homes <- xlsx::read.xlsx(
    paste0(getOption("FSHC_EXTRACTS_DIRECTORY"), "reference_2020-04-22.xlsx"),
    sheetName = "Homes", stringsAsFactors = F)
  names(reference_homes) <- gsub("[.]+", "_", tolower(names(reference_homes)))
  save(reference_homes, file = file.path(getOption("fshc_files"), "reference_homes.rda"))

  postcodes <- unique(reference_homes$postcode)

  reference_geography <- list()

  for(i in 0:trunc(length(postcodes)/100)) {
    reference_geography[[i+1]] <- PostcodesioR::bulk_postcode_lookup(
      list(postcodes = postcodes[(i*100):min(i*100+99,
                                             length(postcodes))])
    )
  }

  reference_geography <- unlist(reference_geography, recursive = F)
  reference_geography <- lapply(reference_geography, function(X) {
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
  reference_geography <- dplyr::bind_rows(reference_geography)

  reference_geography <- reference_geography[, c(
    grep("_code$", colnames(reference_geography), invert = TRUE),
    grep("_code$", colnames(reference_geography))
  )]

  save(reference_geography, file = file.path(getOption("fshc_files"), "reference_geography.rda"))
}

#' @rdname extract_data
extract_data <- function(reference_data = FALSE) {
  if(reference_data){
    extract_reference_homes()
  }
  extract_occupancy()
  extract_residents()
  extract_incidents()
}



#' Load data extracts into the global environment
#'
#' @description Loads data extracts stored in directory `getOption("fshc_files")`:
#' `"residents.rda"`, `"incident.rda"`, `"occupancy.rda"`, `"beds.rda"`,
#' `"reference_homes.rda"`, `"reference_geography.rda"`
#' @return nothing
#' @export
load_data <- function() {
  for(file in list(
    "residents.rda",
    "incidents.rda",
    "beds.rda",
    "occupancy.rda",
    "reference_geography.rda",
    "reference_homes.rda"
  )) {
    load(file.path(getOption("fshc_files"), file), envir = .GlobalEnv)
  }
}


