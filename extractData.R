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


# Reload csvs Two Week Wait tabs-------------------------------------

files_list_sheets <- list.files(path = "data",
                                pattern = "Two Week Wait",
                                full.names = TRUE
)

for(i in files_list_sheets) {
  
  x <- read_csv((i), col_types = cols(.default = col_character()))
  
  assign(i, x)
}


# Format data ------------------------------------------------------------

weekWait <- `data/cancerWaitingTimes-Two Week Wait.csv` %>% 
  clean_names() %>% 
  remove_empty() %>% 
  rename(x1 = two_week_wait_from_gp_urgent_referral_to_first_consultant_appointment) %>% 
  mutate(x1 = case_when(is.na(x1) ~ 'dates',
                        TRUE ~ x1),
         x2 = case_when(is.na(x2) ~ 'blank',
                        TRUE ~x2)) %>% 
  filter(x1 != 'Operational Standard = 93%') 

# switch rows for dates to below header -----------------------------------

weekWaitDate <- weekWait %>% 
  slice(1) %>% 
  pivot_longer(cols = -c(x1, x2),
               names_to = "categories",
               values_to = "values") %>% 
  fill(values, .direction = "down") %>% 
  mutate(ExcelSerialDate = case_when(stri_length(values) == 5 ~ excel_numeric_to_date(as.numeric(values), date_system = "modern")),
         ExcelSerialDate = as.character(ExcelSerialDate),
         values = case_when(!is.na(ExcelSerialDate) ~ ExcelSerialDate,
                            TRUE ~ values)) %>% 
  select(-ExcelSerialDate) %>% 
  pivot_wider(names_from = categories,
              values_from = values) 


weekWaitComplete <- weekWaitDate %>% 
  union_all(weekWaitAll %>% 
              filter(x1 != 'dates'))

