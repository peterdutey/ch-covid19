## code to prepare `mock_regional_data` dataset goes here

mock_regional_data <- read.csv("data-raw/mock_regional_data.csv", stringsAsFactors = F)
usethis::use_data(mock_regional_data)
