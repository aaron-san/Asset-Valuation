
library(dplyr)
library(arrow)


# Load ratios
ratios_file <- 
    list.files("C:/Users/user/Desktop/Aaron/R/Projects/Fundamentals-Data/data/cleaned data", pattern = "ratios_final ", 
               full.names = TRUE) %>% max()
ratios <- read_feather(ratios_file)

choices_tickers <- ratios %>% distinct(ticker) %>% pull()
choices_field <- ratios %>% select(where(is.numeric)) %>% colnames()
