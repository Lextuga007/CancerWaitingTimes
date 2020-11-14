library(readxl)
library(janitor)
library(tidyverse)
library(stringi)
library(lubridate)
library(NHSRdatasets)
library(httr)
library(rvest)


# Download data -----------------------------------------------------------

# Create folders if do not exist. 

if(!dir.exists("data")) {
  dir.create("data", recursive = T)
}

# Download file
download.file("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Cancer-Waiting-Times-Commissioner-Time-Series-Apr-2011-Sep-2020-with-Revisions-2.xlsx",
                     destfile = "data/cancerWaitingTimes.xlsx",
                     method = "wininet",
                     mode = "wb")                 

# Extract all worksheets to individual csv -------------------------------------------------------------

files_list <- list.files(path = "data",
                         pattern = "*.xlsx",
                         full.names = TRUE)


read_then_csv <- function(sheet, path) {
  pathbase <- path %>%
    basename() %>%
    tools::file_path_sans_ext()
  path %>%
    read_excel(sheet = sheet) %>%
    write_csv(paste0("data/", pathbase, "-", sheet, ".csv"))
}


for(j in 1:length(files_list)){
  
  path <- paste0(files_list[j])
  
  path %>%
    excel_sheets() %>%
    set_names() %>%
    map(read_then_csv, path = path)
}


