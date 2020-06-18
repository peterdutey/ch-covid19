



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


check_homes <- function(home_codes, call = NULL) {

  if(!file.exists(file.path(getOption("fshc_files"), "reference_homes.rda"))) {
    extract_reference_homes()
  }
  load(file.path(getOption("fshc_files"), "reference_homes.rda"))

  unknown_home <- na.omit(unique(home_codes))
  unknown_home <- unknown_home[which(!unknown_home %in% reference_homes$home_code)]

  if(length(unknown_home)>0) {
    warning(call, " home unknown to `reference homes`\n",
            paste(unknown_home, collapse = ", "))
  }

  missing_home <- reference_homes$home_code[
    which(!reference_homes$home_code %in% home_codes)
    ]
  if(length(missing_home)>0) {
    warning(
      "The following homes are missing",
      if(!is.null(call)) paste0(" in ", call),
      "\n", paste(missing_home, collapse = ", "))
  }

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

  incidents_filepath <- list.files(file.path(getOption("FSHC_EXTRACTS_DIRECTORY"), "incidents"), full.names = T)
  incidents_filepath <- sort(incidents_filepath, decreasing = T)[1]
  incidents_src <- read.csv(incidents_filepath,
                            stringsAsFactors = F,na.strings = "")
  incidents_src <- clean_variable_names(incidents_src)
  # removing Jersey
  incidents_src <- dplyr::filter(incidents_src,
                                 ! tolower(trimws(home)) %in% c("la haule",
                                                                "silver springs"))

  incidents <- incidents_src
  for(i in grep("(_date)|(date_of)", names(incidents))){
    incidents[,i] <- gsub("/2002$", "/2020", incidents[,i])
    incidents[,i] <- lubridate::dmy(incidents[,i])
  }
  incidents <- dplyr::rename(incidents, resident_id = resident_encryptedid)

  stopifnot(!any(is.na(incidents$incident_date)))

  incidents[which(incidents$age < 5), c("age", "date_of_birth")] <- NA

  incidents$resident_id <- dplyr::if_else(
    is.na(incidents$resident_id),
    paste0("MISSING_", incidents$id),
    incidents$resident_id
  )


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
  # remove any accidental staff remaining
  incidents <- dplyr::filter(incidents, infection_covid_19_type_code %in% 1:2)

  home_codes <- dplyr::distinct(reference_homes,
                                home_code,
                                home_name_datix)
  home_codes <- dplyr::rename(home_codes, home_name = home_name_datix)
  incidents <- dplyr::rename(incidents, home_name = home)
  incidents <- dplyr::mutate(
    incidents,
    home_name = dplyr::case_when(
      home_name == "Brackenbed View" ~ "Pellon Manor",
      home_name == "Gilmerton NCC" ~ "Gilmerton",
      TRUE ~ home_name
    ))
  incidents <- dplyr::left_join(incidents, home_codes, by = "home_name")

  if(any(is.na(incidents$home_code))) {
    warning("incident home not known to `reference_homes`")
  }

  if(sum(is.na(incidents$home_name))) {
    warning(
      "home is missing in ",
      as.character(sum(is.na(incidents$home_name))),
      " incident reports")
  }

  # symptoms

  if(exists("infection_covid_19_symptoms", incidents)){
  ref_symptoms <- reference_symptoms()
  incidents$infection_covid_19_symptoms <- gsub("[â°]", "", tolower(
    incidents$infection_covid_19_symptoms
  ))

  unique_symptoms <- incidents$infection_covid_19_symptoms %>%
    na.omit %>%
    unique %>%
    strsplit(", ") %>% unlist %>% unique

  if(!all(unique_symptoms %in% ref_symptoms$symptom_description)){
    stop("Undefined symptoms found in incidents dataset")
  }

  for(i in 1:(nrow(ref_symptoms) - 1)) {
    incidents[[paste0("s_", ref_symptoms$symptom_name[i])]] <-
      as.integer(
        grepl(ref_symptoms$symptom_description[i],
              incidents$infection_covid_19_symptoms, fixed=TRUE)
      )
  }

  incidents$s_asymptomatic <- as.integer(incidents$infection_covid_19_symptoms == "no symptoms")
  } else {
    warnings("not symptoms data")
  }

  # Adjudicate positive cases
  incidents <- incidents %>%
    dplyr::mutate(
      infection_result_code =  dplyr::case_when(
        is.na(infection_result) ~ NA_integer_,
        grepl("neg", tolower(infection_result)) ~ 0L,
        grepl("(pos)|(poistive)|(covid)", tolower(infection_result)) ~ 1L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::mutate(
      covid_tested = as.integer(!is.na(infection_result_code) |
                                  !is.na(infection_covid_19_test_date) |
                                  !infection_confirmed %in% c("Not tested", NA)),
      covid_symptomatic = #1
      dplyr::case_when(
        s_asymptomatic == 1 ~ 0L,
        TRUE ~ 1L
      )
      ) %>%
    dplyr::mutate(
      covid_test_result = dplyr::case_when(
        !is.na(infection_result_code) ~ infection_result_code,
        infection_confirmed == "Yes - positive test result: confirmed case" ~ 1L,
        infection_confirmed == "No - negative test result" ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::mutate(
      covid_confirmed =  dplyr::case_when(
        !is.na(covid_test_result) ~ as.integer(covid_test_result),
        infection_covid_19_type_code == 2 ~ 1L,
        TRUE ~ 0L)
    ) %>%
    dplyr::group_by(resident_id) %>%
    dplyr::mutate(incident_rank = if_else(
      is.na(resident_id),
      NA_integer_,
      dplyr::dense_rank(incident_date)
    )) %>%
    dplyr::mutate(
      covid_first_symptomatic = min_non_missing(
        if_else(covid_symptomatic == 1,
                incident_date,
                as.Date(NA))),
      covid_first_confirmed = min_non_missing(
        if_else(covid_confirmed == 1,
                incident_date,
                as.Date(NA))),
      covid_first_positive = min_non_missing(
        if_else(covid_test_result == 1,
                incident_date,
                as.Date(NA))),
      covid_first_tested = min_non_missing(
        if_else(covid_tested == 1,
                incident_date,
                as.Date(NA)))
    ) %>%
    dplyr::mutate(
      covid_ever_symptomatic = if_else(
        is.na(covid_first_symptomatic),
        0L,
        as.integer(incident_date >= covid_first_symptomatic)),
      covid_ever_confirmed = if_else(
        is.na(covid_first_confirmed),
        0L,
        as.integer(incident_date >= covid_first_confirmed)),
      covid_ever_positive = if_else(
        is.na(covid_first_positive),
        0L,
        as.integer(incident_date >= covid_first_positive)
      ),
      covid_ever_tested = if_else(
        is.na(covid_first_tested),
        0L,
        as.integer(incident_date >= covid_first_tested)
      )) %>%
    dplyr::ungroup()

  save(incidents, file = file.path(getOption("fshc_files"), "incidents.rda"))
  usethis::ui_done("incidents.rda")
}

#' @rdname extract_data
extract_residents <- function() {

  residents_filepath <- list.files(file.path(getOption("FSHC_EXTRACTS_DIRECTORY"), "residents"), full.names = T)
  residents_filepath <- sort(residents_filepath, decreasing = T)[1]
  residents_src <- read.csv(residents_filepath,
                            stringsAsFactors = F,na.strings = "")
  residents_src <- clean_variable_names(residents_src)
  # Removing Jersey
  residents_src <- dplyr::rename(residents_src, home_code = home_id)
  residents_src <- dplyr::filter(residents_src, !home_code %in% c("LAH", "SIL"))

  residents <- residents_src
  for(i in grep("(_date)|(date_of)|(dob)", names(residents))){
    residents[,i] <- lubridate::dmy(residents[,i])
  }

  residents$last_admission_date <- dplyr::if_else(
    is.na(residents$last_admission_date),
    residents$original_admission_date,
    residents$last_admission_date
  )

  home_types <- dplyr::tibble(
    funding_type = c('Nursing General', 'Residential General',
                     'Nursing Dementia', 'Residential Dementia',
                     'Younger Person', 'Nursing - EMI',
                     'Nursing - Elderly', 'Residential - Elderly',
                     'Nursing General Private', 'Residential General Private',
                     'Nursing Dementia Private', 'Residential Dementia Private',
                     'Young Chronic Sick/YPD'),
    home_type = c('Nursing', 'Residential',
                  'Nursing', 'Residential',
                  'Younger Person', 'Nursing',
                  'Nursing', 'Residential',
                  'Nursing', 'Residential',
                  'Nursing', 'Residential',
                  'Young Chronic Sick/YPD'),
    user_type = c("General", "General",
                  "Dementia", "Dementia",
                  "Young", "Elderly",
                  "Elderly", "Elderly",
                  "General", "General",
                  "Dementia", "Dementia",
                  "Young")
  )

  names(residents)[grep("status_", names(residents))] <- "status"
  residents$status <- trimws(residents$status)
  residents$admission_type <- trimws(residents$admission_type)
  residents$funding_type <- trimws(residents$funding_type)
  residents <- merge(residents, home_types, all.x = TRUE)

  residents <- dplyr::distinct(residents) %>%
    dplyr::rename(resident_id = encrypted_id)

  check_homes(residents$home_code, call = "`residents`")

  if(sum(is.na(residents$home_code))) {
    warning(
      "home is missing in ",
      as.character(sum(is.na(residents$home_code))),
      " residents records")
  }

  residents <- dplyr::filter(residents,
                             !funding_type %in% c("Young Chronic Sick/YPD", "Younger Person"))

  save(residents, file = file.path(getOption("fshc_files"), "residents.rda"))
  usethis::ui_done("residents.rda")
}

#' @rdname extract_data
extract_occupancy <- function() {

  if(!file.exists(file.path(getOption("fshc_files"), "reference_homes.rda"))) {
    extract_reference_homes()
  }

  occupancy_filepath <- list.files(file.path(getOption("FSHC_EXTRACTS_DIRECTORY"), "occupancy"), full.names = T)
  occupancy_filepath <- sort(occupancy_filepath, decreasing = T)[1]
  occupancy_src <- read.csv(occupancy_filepath, stringsAsFactors = F, na.strings = "")
  occupancy_src <- clean_variable_names(occupancy_src)
  # Removing Jersey
  occupancy_src <- dplyr::filter(occupancy_src, !home_code %in% c("LAH", "SIL"))

  cbeds_filepath <- list.files(file.path(getOption("FSHC_EXTRACTS_DIRECTORY"), "beds"), full.names = T)
  cbeds_filepath <- sort(cbeds_filepath, decreasing = T)[1]
  cbeds <- read.csv(cbeds_filepath, stringsAsFactors = F)
  cbeds <- clean_variable_names(cbeds)
  cbeds$year <- "2020"

  beds <- dplyr::select(occupancy_src, home_code, dplyr::starts_with("beds_"))
  beds <- reshape(beds, direction = "long", idvar = "home_code",
            varying = 2:ncol(beds), v.names = "beds", sep = "_",
            timevar = "year",
            times = gsub("beds_", "", names(beds)[2:ncol(beds)])) %>%
    merge(cbeds, all.x = T) %>%
    dplyr::mutate(contract_beds = tidyr::replace_na(contract_beds, 0))
  row.names(beds) <- NULL

  occupancy <- dplyr::select(occupancy_src, home_code, dplyr::starts_with("x"))
  # Multiply occupancy by 7 to get the total resident-days
  occupancy <- reshape(occupancy, direction = "long", idvar = "home_code",
                  varying = 2:ncol(occupancy), v.names = "occupancy", sep = "",
                  timevar = "week_ending",
                  times = lubridate::dmy(gsub("x", "", names(occupancy)[2:ncol(occupancy)]))) %>%
    dplyr::mutate(occupancy = dplyr::na_if(occupancy, -1)) %>%
    dplyr::group_by(home_code) %>%
    dplyr::mutate(last_occupancy = dplyr::lag(occupancy, order_by = week_ending)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(resident_days = (occupancy + last_occupancy) * 7 / 2)
  row.names(occupancy) <- NULL

  check_homes(beds$home_code, call = "`beds`")
  check_homes(occupancy$home_code, call = "`occupancy`")

  save(beds, file = file.path(getOption("fshc_files"), "beds.rda"))
  usethis::ui_done("beds.rda")
  save(occupancy, file = file.path(getOption("fshc_files"), "occupancy.rda"))
  usethis::ui_done("occupancy.rda")
}

#' @rdname extract_data
#' @import PostcodesioR
extract_reference_homes <- function() {

  homes_filepath <- list.files(file.path(getOption("FSHC_EXTRACTS_DIRECTORY"), "homes"), full.names = T)
  homes_filepath <- sort(homes_filepath, decreasing = T)[1]
  reference_homes <- read.csv(homes_filepath, stringsAsFactors = F, na.strings = "")
  reference_homes <- clean_variable_names(reference_homes)
  reference_homes <- unique(reference_homes)
  reference_homes[reference_homes$postcode=="BA13 3JD", "postcode"] <- "BA13 3JH"
  # Remove Jersey
  reference_homes <- dplyr::filter(reference_homes, !home_code %in% c("LAH", "SIL"))
  stopifnot(!any(duplicated(reference_homes$home_code)))

  reference_homes <- dplyr::mutate_at(
    reference_homes,
    dplyr::vars(nursing_dementia, nursing_general,
                residential_dementia, residential_general, younger_person),
    function(X){
      dplyr::case_when(X == "Y" ~ 1L,
                       X == "N" ~ 0L,
                       TRUE ~ NA_integer_)
    }
  )


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

  reference_geography <- dplyr::mutate(
    reference_geography,
    region_UK = if_else(country == "England", region, country)
  )

  save(reference_geography, file = file.path(getOption("fshc_files"), "reference_geography.rda"))
  usethis::ui_done("reference_geography.rda")
}

#' @rdname extract_data
extract_tallies <- function() {

  if(!file.exists(file.path(getOption("fshc_files"), "reference_homes.rda"))) {
    extract_reference_homes()
  }

  files <- list.files(file.path(getOption("FSHC_EXTRACTS_DIRECTORY"), "tallies"), full.names = T)
  new_filepath <- sort(grep("tally_new_cases.*csv$", files, value = T), decreasing = T)[1]
  total_filepath <- sort(grep("tally_total_cases.*csv$", files, value = T), decreasing = T)[1]

  new_cases <- read.csv(new_filepath, stringsAsFactors = F)
  total_cases <- read.csv(total_filepath, stringsAsFactors = F)

  names(new_cases) <- tolower(names(new_cases))
  new_cases$date <- lubridate::dmy(new_cases$date)
  new_cases$time <- lubridate::dmy_hm(new_cases$time)
  new_cases <- dplyr::filter(new_cases, !home_code %in% c("LAH", "SIL"))

  names(total_cases) <- tolower(names(total_cases))
  total_cases$date <- lubridate::dmy(total_cases$date)
  total_cases$time <- lubridate::dmy_hm(total_cases$time)
  total_cases <- dplyr::filter(total_cases, !home_code %in% c("LAH", "SIL"))

  # Merge Gilmerton NCC within 565	Gilmerton
  # Rid of Holybourne Day Centre (closed)
  total_cases$home_name[total_cases$home_code == "GIL"] <- "Gilmerton"
  total_cases$home_code[total_cases$home_code == "GIL"] <- "565"
  total_cases <- dplyr::filter(total_cases, home_code != "240")
  new_cases$home_name[new_cases$home_code == "GIL"] <- "Gilmerton"
  new_cases$home_code[new_cases$home_code == "GIL"] <- "565"
  new_cases <- dplyr::filter(new_cases, home_code != "240")

  check_homes(unique(total_cases$home_code))
  check_homes(unique(new_cases$home_code))

  save(total_cases, file = file.path(getOption("fshc_files"), "total_cases.rda"))
  usethis::ui_done("total_cases.rda")
  save(new_cases, file = file.path(getOption("fshc_files"), "new_cases.rda"))
  usethis::ui_done("new_cases.rda")
}

#' @rdname extract_data
extract_timelines <- function(){

  if(!file.exists(file.path(getOption("fshc_files"), "residents.rda"))) {
    extract_residents()
  }
  load(file.path(getOption("fshc_files"), "residents.rda"))

  if(!file.exists(file.path(getOption("fshc_files"), "incidents.rda"))) {
    extract_incidents()
  }
  load(file.path(getOption("fshc_files"), "incidents.rda"))

  timelines <- resident_timelines(
    residents_data = residents,
    incidents_data = incidents,
    time_span = get_time_span(data = incidents)
  )
  save(timelines, file = file.path(getOption("fshc_files"), "timelines.rda"))
  usethis::ui_done("timelines.rda")
}

#' @rdname extract_data
extract_data <- function(reference_data = FALSE) {
  if(reference_data){
    extract_reference_homes()
  }
  extract_occupancy()
  extract_residents()
  extract_incidents()
  extract_tallies()
  extract_timelines()
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
    "reference_homes.rda",
    "new_cases.rda",
    "total_cases.rda",
    "timelines.rda"
  )) {
    load(file.path(getOption("fshc_files"), file), envir = .GlobalEnv)
  }
}


reference_symptoms <- function(){
  dplyr::tibble(
    symptom_name = c(
      "cough",
      "sore_throat",
      "nasal",
      "wheezing",
      "short_breath",
      "hoarse",
      "fever",
      "smell",
      "other",
      "asymptomatic"
    ),
    symptom_description = c(
      "cough - new & persistent (with or without sputum)",
      "sore throat",
      "nasal congestion or discharge",
      "wheezing",
      "shortness of breath",
      "hoarseness",
      "fever (temp 37.8c or above)",
      "loss of sense of taste or smell",
      "other symptoms not listed",
      "no symptoms"
    ))
}


clean_variable_names <- function(x){
  names(x) <- tolower(names(x))
  names(x) <- gsub("[.]+", "_", names(x))
  names(x) <- gsub("(_$)|(^_)", "", names(x))
  x
}
