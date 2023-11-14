library(tidyverse)
library(readxl)
library(unpivotr)
library(openxlsx)
library(janitor)

datasheets <- 'dss-income-support-recipient-monthly-time-series-september-2023.xlsx'


data_head <- read_excel(datasheets, sheet = 'Parenting Payment Single', skip = 1,
                                            n_max = 4, 
                                            col_names = FALSE)

for (i in 2:ncol(data_head)) {
  prev <- data_head[, i-1]
  this <- data_head[, i]
  missing <- is.na(this)
  this[missing, ] <- prev[missing, ]
  data_head[, i] <- this
}

new_names <- data_head %>%
  summarise(across(.fns = paste, collapse = ".")) %>%
  unlist() %>% unname()

new_names

dss <- read_excel(datasheets, 
                  sheet = 'Parenting Payment Single', skip = 5, col_names = new_names)# %>%
  clean_names()

colnames(dss)<-gsub(".NA","",colnames(dss))
