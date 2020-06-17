
compute_age <- function(dob, date){
  as.integer(trunc(lubridate::time_length(
    lubridate::interval(dob, date)
    , "year")))
}

cut_age_denary <- function(age) {
  # as.character(
    cut(
      x = age,
      breaks = c(seq(30, 130, 10), 150),
      include.lowest = FALSE,
      right = FALSE,
      labels = c(paste0(
        seq(30, 129, 10), "-",
        seq(39, 129, 10), " years"),
        "130+ years")
    )#)
}


cut_age_quinary <- function(age) {
  as.character(
    cut(
      x = age,
      breaks = c(seq(30, 135, 5), 150),
      include.lowest = FALSE,
      right = FALSE,
      labels = c(paste0(
        seq(30, 130, 5), "-",
        seq(34, 134, 5), " years"),
        "135+ years")
    ))
}

cut_age_pentadecimal <- function(age) {
  as.character(
    cut(
      x = age,
      breaks = c(seq(30, 135, 15), 150),
      include.lowest = FALSE,
      right = FALSE,
      labels = c(paste0(
        seq(30, 129, 15), "-",
        seq(44, 135, 15), " years"),
        "135+ years")
    ))
  }

tbrounding <- function(x, places = 1){
  format(round(x, places), nsmall = places)
}


pvrounding <- function(x){
  if(x < 0.001){
    "$p$ < 0.001"
  } else {
    paste("$p$ =", tbrounding(x, 3))
  }
}

formatbm <- function(x){
  format(x, big.mark = ",")
}


#' Minimum non missing value
#'
#' @description Unlike \code{\link{min}()}, this function returns NA
#' if there is no non-missing element in `x`
#' @param x a vector
#' @return a vector
#' @export
min_non_missing <- function(x) {
  date_input <- is(x, "Date")
  x <- na.omit(x)
  if(length(x)==0){
    if(date_input){
      return(as.Date(NA))
    } else {
      return(NA)
    }
  } else {
    return(min(x))
  }
}


#' Set timespan of measurement
#'
#' @param data
#'
#' @return a list with two `Date` objects named `"start"` and `"end"`
#' @export
#' @importFrom lubridate %m+% days
get_time_span <- function(data = incidents) {
  dplyr::summarise(data,
            start = get_monday_date(min(incident_date)),
            end = get_monday_date(max(incident_date)) %m+% days(6)) %>%
    as.list

}

#' Build weekly time series table
#'
#' @param timespan a list with a week start date and a week end date.
#' Default is \code{\link{get_time_span}()}.#'
#' @return a data frame with variables `week_number`, `year`, `week_starting`,
#' and `week_ending`
#' @export
#' @importFrom lubridate %m+% days
get_time_series <- function(timespan = get_time_span()) {
  data.frame(list(
    week_starting = seq(timespan$start, timespan$end %m-% days(6), 7)
  )) %>%
    dplyr::mutate(week_number = lubridate::isoweek(week_starting),
           year = lubridate::year(week_starting),
           week_ending = week_starting %m+% days(6)) %>%
    dplyr::transmute(week_number, year, week_starting, week_ending)

}


#' Get the week starting date
#'
#' @param date a `Date` vector
#'
#' @return a `Date` vector corresponding to the preceding Monday
#' @export
get_monday_date <- function(date) {
  if_else(wday(date, week_start = 1) == 1,
          date,
          date %m-% days(wday(date, week_start = 2)))
}


resident_timelines <- function(residents_data = residents,
                               incidents_data = incidents,
                               time_span = get_time_span()) {
  # This function generates individual resident timelines on the basis of FSHC records
  # of last admission and discharge dates. This is a mere approximation in the absence
  # of actual records of all admissions and discharges
  progress <- dplyr::progress_estimated(length(residents_data$resident_id))

  incidents_data <- dplyr::group_by(incidents_data, resident_id) %>%
    dplyr::summarise(
      covid_first_positive = min_non_missing(covid_first_positive),
      covid_death = min_non_missing(infection_covid_19_date_of_death)
    )

  output <- dplyr::group_by(residents_data, resident_id) %>%
    tidyr::nest() %>%
    dplyr::left_join(incidents_data)

  progress <- dplyr::progress_estimated(length(residents_data$resident_id))

  output <- output %>%
    dplyr::mutate(
      timeline = purrr::pmap(list(resident_id,
                                  data,
                                  covid_first_positive,
                                  covid_death),
                             function(resident_id,
                                      data,
                                      covid_first_positive,
                                      covid_death){

      progress$tick()$print()

      # Eliminate records of multiple admissions - this is a processing error from FSHC,
      # probably pseudonymisation problem
      main_admissions <- dplyr::distinct(data) %>%
        dplyr::mutate(admission_type2 = as.integer(admission_type != "Respite")) %>%
        dplyr::arrange(desc(admission_type2), desc(last_discharge_date), desc(last_admission_date), desc(last_admission_date)) %>%
        .[1,]

      timeline <- data.frame(list(
        resident_id = resident_id,
        date = seq(time_span$start, time_span$end, by = 1),
        date_end = time_span$end),
        stringsAsFactors = FALSE)

      timeline <- merge(
        timeline,
        main_admissions,
        all = TRUE)

      other_admissions <- dplyr::filter(data, admission_type == "Respite") %>%
        # if missing, there are still marked as in home
        # also, in every case where last discharge < last admission, their status is marked as in home.
        dplyr::mutate(last_discharge_date = dplyr::case_when(
          is.na(last_discharge_date) ~ time_span$end,
          last_discharge_date < last_admission_date ~ time_span$end,
          TRUE ~ last_discharge_date
        ))

      other_admissions_days <- lubridate::ymd(
        unique(
          unlist(purrr::map2(
            other_admissions$last_admission_date,
            other_admissions$last_discharge_date,
            function(x, y){
              as.character(seq(x, y, by = 1L))
            }))
        )
      )

      timeline[timeline$date %in% other_admissions_days, "rday"] <- 1


      timeline <- resident_days_approx_indicator(timeline) %>%
        dplyr::mutate(
          rday = dplyr::case_when(
            date > covid_death ~ 0L,
            TRUE ~ rday
          ),
          susceptible = 1L
        ) %>%
        dplyr::mutate(
          susceptible = dplyr::case_when(
            rday == 0 ~ 0L,
            date > covid_first_positive ~ 0L,
            TRUE ~ susceptible
          ))

      timeline
    }))

  output
}

resident_days_approx <- function(timelines_data = timelines, time_span = get_time_span()) {
  # This function merges individual resident timelines and computes an approximation
  # of daily occupancy for each care home

  timeline <- dplyr::bind_rows(timelines_data$timeline)

  duplic <- duplicated(timeline[,c("resident_id", "date")])

  if(any(duplic)){
    duplic <- dplyr::filter(timeline, resident_id %in% timeline$resident_id[which(duplic)])
    print(duplic)
    warning("Duplicated resident-days - please verify resident_days_approx()")
  }

  occup <- timeline %>%
    dplyr::group_by(home_code, date, date_end) %>%
    dplyr::summarise(
      approx_resident_days = sum(rday, na.rm = T)
    )

  occup
}

#' Approximate presence in care home
#'
#' @param data a data frame containing varibles `date`, `original_admission_date`,
#' `last_admission_date`, `last_discharge_date`, and `status`
#'
#' @return a data.frame with an rday variable indicating whether the resident
#' was in or out of the care me on the date contained in variable `date`
#' @importFrom dplyr mutate if_else between
resident_days_approx_indicator <- function(data){
  dplyr::mutate(data, rday = if_else(
    is.na(last_discharge_date),
    if_else(date >= last_admission_date, 1L, 0L),
    if_else(last_discharge_date > last_admission_date |
              (last_discharge_date == last_admission_date & status != "In Home"),
            # orig adm       last adm         last disch        now
            # |--  --  --  --|----------------|                |
            as.integer(data.table::between(date, original_admission_date, last_discharge_date)),
            # orig adm       last disch       last adm          now
            # |--  --  --  --|                |----------------|
            as.integer(
              data.table::between(date, original_admission_date, last_discharge_date) |
                date >= last_admission_date
            ))
  ))
}


#' Rates of COVID tests
#'
#' @return
#' @export
#' @importFrom lubridate %m+% %m-% days wday
table_weekly_rate_tests <- function() {
  tests <- incidents %>%
    dplyr::transmute(
      home_code,
      covid_tested,
      covid_symptomatic,
      covid_test_result,
      covid_confirmed,
      covid_first_confirmed,
      covid_ever_confirmed,
      incident_rank,
      infection_covid_19_type_code,
      incident_date,
      # if the test date is more than 7 days away from incident date, use the incident date
      date_test = if_else(
        is.na(infection_covid_19_test_date) | abs(as.numeric(infection_covid_19_test_date - incident_date, unit = "days")) > 7,
          incident_date,
          infection_covid_19_test_date),
      location = infection_covid_19_test_location,
      # if the test result date is more than 7 days away from incident date, assume it's 2 days after test date
      date_result = if_else(
        as.numeric(infection_covid_19_test_result_date - infection_covid_19_test_date, unit = "days") > 7,
        if_else(
          abs(as.numeric(infection_covid_19_test_date - incident_date, unit = "days")) > 7,
          incident_date,
          infection_covid_19_test_date)  %m+%  days(2),
        infection_covid_19_test_result_date),
      ) %>%
    mutate(week_starting =  get_monday_date(date_test)) %>%
    dplyr::group_by(home_code, week_starting) %>%
    dplyr::summarise(
      first_cases = sum(incident_rank %in% c(NA, 1), na.rm = T),
      total_cases = sum(covid_symptomatic, na.rm = T),
      suspected_cases = sum(covid_confirmed, na.rm = T),
      tests_performed = sum(covid_tested, na.rm = T),
      tests_performed_home = sum(covid_tested == 1 & location %in% c("Care home", NA)),
      tests_performed_hospital_or_other = sum(covid_tested == 1 & !(location %in% c("Care home", NA))),
      tests_reported = sum(covid_tested & !is.na(covid_test_result)),
      tests_positive = sum(covid_test_result, na.rm = T)
    )

  timespan <- list(
    start = get_monday_date(
      min(c(incidents$incident_date,
            incidents$infection_covid_19_test_date),
          na.rm = T)),
    end = get_monday_date(min(
      Sys.Date(),
      max(c(incidents$incident_date,
            incidents$infection_covid_19_test_result_date), na.rm = T)))
     %m+% days(6)
  )

  # roll forward occupancy when missing
  output <- merge(
    get_time_series(timespan = timespan),
    reference_homes[, c("home_code", "home_name_clickview")]) %>%
    dplyr::left_join(occupancy, by = c("home_code", "week_ending")) %>%
    dplyr::group_by(home_code) %>%
    dplyr::mutate(occupancy_imputed = if_else(
      is.na(occupancy),
      dplyr::last(na.omit(occupancy), order_by = week_starting),
      occupancy
    ),
    resident_days_imputed = if_else(
      is.na(resident_days),
      dplyr::last(na.omit(resident_days), order_by = week_starting),
      resident_days
    ) ) %>%
    dplyr::ungroup()

  output <- dplyr::left_join(output, tests, by = c("week_starting", "home_code")) %>%
    dplyr::group_by(home_code) %>%
    dplyr::arrange(week_starting) %>%
    dplyr::mutate(
      cases_cumulative = cumsum(if_else(is.na(first_cases), 0L, first_cases))) %>%
    dplyr::ungroup()

  output
}



#' Deduplicated cases and events
#'
#' @return
#' @export
#' @importFrom lubridate %m+% %m-% days wday
weekly_deduplicated_cases <- function() {

  restable <- dplyr::transmute(
    residents,
    resident_id,
    res_gender = dplyr::case_when(
      gender == "F" ~ "Female",
      gender == "M" ~ "Male",
      TRUE ~ NA_character_
    ),
    res_dob = lubridate::dmy(dob)
  ) %>% dplyr::distinct()

  cases <- dplyr::left_join(incidents, restable, by = "resident_id") %>%
    dplyr::mutate(
      gender = if_else(is.na(gender), res_gender, gender),
      age = if_else(!is.na(date_of_birth), compute_age(date_of_birth, incident_date), age),
    )

  cases_gender <- dplyr::select(cases, resident_id, gender) %>%
    na.omit() %>%
    dplyr::group_by(resident_id) %>%
    dplyr::sample_n(size = 1)

  cases_age <- dplyr::select(cases, resident_id, age) %>%
    na.omit() %>%
    dplyr::group_by(resident_id) %>%
    dplyr::sample_n(size = 1)

  cases <- cases %>%
    dplyr::select(-age, -gender) %>%
    dplyr::left_join(cases_age, by = ) %>%
    dplyr::left_join(cases_gender) %>%
    dplyr::transmute(
      resident_id,
      home_code,
      gender,
      age_group = cut_age_pentadecimal(age),
      covid_symptomatic,
      covid_tested,
      covid_test_result,
      covid_confirmed,
      covid_first_symptomatic,
      covid_ever_symptomatic,
      covid_first_confirmed,
      covid_first_positive,
      covid_first_tested,
      infection_covid_19_date_of_hospital_admission,
      infection_covid_19_date_of_death,
      infection_covid_19_type_code,
      incident_date,
      week_starting = get_monday_date(incident_date))

  cases
}

table_weekly_cases_age_sex <- function() {

  cases <- weekly_deduplicated_cases() %>%
    dplyr::group_by(week_starting, age_group, gender) %>%
    dplyr::summarise(
      first_symptomatic = sum(covid_first_symptomatic == incident_date, na.rm = T),
      first_suspected = sum(covid_first_confirmed == incident_date, na.rm = T),
      first_confirmed = sum(covid_first_positive == incident_date, na.rm = T),
      hospitalised = sum(!is.na(infection_covid_19_date_of_hospital_admission)),
      death = sum(!is.na(infection_covid_19_date_of_death))
    )

  timespan <- list(
    start = get_monday_date(
      min(c(incidents$incident_date,
            incidents$infection_covid_19_test_date),
          na.rm = T)),
    end = get_monday_date(min(
      Sys.Date(),
      max(c(incidents$incident_date,
            incidents$infection_covid_19_test_result_date), na.rm = T)))
    %m+% days(6)
  )

  output <- get_time_series(timespan = timespan) #%>%

  output <- dplyr::left_join(output, cases, by = c("week_starting")) %>%
    dplyr::arrange(week_starting) %>%
    dplyr::mutate(
      age_group = if_else(is.na(age_group), "Unknown", age_group),
      gender = if_else(is.na(gender), "Unknown", gender),
      total_symptomatic = cumsum(if_else(is.na(first_symptomatic), 0L, first_symptomatic))
    )

  output
}

table_weekly_cases_home <- function() {

  mdcounts <- new_cases %>%
    dplyr::mutate(week_starting = get_monday_date(date)) %>%
    dplyr::group_by(week_starting, home_code) %>%
    dplyr::summarise(
      tally_first_symptomatic = sum(new_symptomatic_home, na.rm = T)
      # removed 2020/06/12 - seems to
      # fit the data better without hospital cases
      # + sum(new_symptomatic_hospital, na.rm = T)
      ,
      tally_confirmed = sum(new_confirmed_home, na.rm = T)
      # removed 2020/06/12 - seems to
      # fit the data better without hospital cases
      # + sum(new_confirmed_hospital, na.rm = T)
      ,
      tally_deaths = sum(new_deaths_home, na.rm = T) # + sum(new_deaths_hospital, na.rm = T)
    )

  cases <- weekly_deduplicated_cases() %>%
    dplyr::group_by(home_code, week_starting) %>%
    dplyr::summarise(
      first_symptomatic = sum(covid_first_symptomatic == incident_date, na.rm = T),
      first_suspected = sum(covid_first_confirmed == incident_date, na.rm = T),
      first_confirmed = sum(covid_first_positive == incident_date, na.rm = T),
      first_test = sum(covid_first_tested == incident_date, na.rm = T),
      hospitalised = sum(!is.na(infection_covid_19_date_of_hospital_admission)),
      death = sum(!is.na(infection_covid_19_date_of_death))
    )

  timespan <- list(
    start = get_monday_date(
      min(c(incidents$incident_date,
            incidents$infection_covid_19_test_date),
          na.rm = T)),
    end = get_monday_date(min(
      Sys.Date(),
      max(c(incidents$incident_date,
            incidents$infection_covid_19_test_result_date), na.rm = T)))
    %m+% days(6)
  )

  beds2020 <- dplyr::filter(beds, year == "2020") %>%
    dplyr::transmute(home_code,
                     beds_2020 = beds)

  output <- merge(
    get_time_series(timespan = timespan),
    dplyr::select(reference_homes, home_code, home_name_datix, home_name_clickview),
    all = T
    ) %>%
    dplyr::left_join(occupancy) %>%
    dplyr::group_by(home_code) %>%
    dplyr::mutate(occupancy_imputed = if_else(
      is.na(occupancy),
      dplyr::last(na.omit(occupancy), order_by = week_starting),
      occupancy
    ),
    resident_days_imputed = if_else(
      is.na(resident_days),
      dplyr::last(na.omit(resident_days), order_by = week_starting),
      resident_days
    ) ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(beds2020)

  output <- dplyr::left_join(output, cases, by = c("week_starting", "home_code")) %>%
    dplyr::left_join(mdcounts, by = c("week_starting", "home_code"))

  output
}


export_tables <- function() {

  write.csv(table_weekly_rate_tests(),
            file = file.path(getOption("fshc_files"), "rate_tests.csv"),
            na = "", row.names = FALSE)

  write.csv(table_weekly_cases_age_sex(),
            file = file.path(getOption("fshc_files"), "case_counts_week_age_sex.csv"),
            na = "", row.names = FALSE)

  write.csv(table_weekly_cases_home(),
            file = file.path(getOption("fshc_files"), "case_counts_week_care_home.csv"),
            na = "", row.names = FALSE)

}
