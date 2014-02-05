#'
#' global.R
#' 
#' load required libraries & setup, well, global vars
#' 

library(shiny)
library(data.table)
library(stringr)

#' 25 worst passwords (global)

worst.pass <- c("password", "123456", "12345678", "qwerty", "abc123", 
                "monkey", "1234567", "letmein", "trustno1", "dragon", 
                "baseball", "111111", "iloveyou", "master", "sunshine", 
                "ashley", "bailey", "passw0rd", "shadow", "123123", 
                "654321", "superman", "qazwsx", "michael", "football")

#' shorthand globals for dows/months/yrs

weekdays.full <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
weekdays.abbrev <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")

months.full <- tolower(month.name)
months.abbrev <- tolower(month.abb)

yrs <- as.character(1975:2030)
