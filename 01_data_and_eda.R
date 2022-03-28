options(scipen = 999)

library(tidyverse)
library(skimr)
library(DataExplorer)


# Read sample data
data_tbl <- read_csv("sample.csv") 

# 01 - EDA ----

data_tbl %>% skim()

data_tbl %>% head()

data_tbl %>% glimpse()

data_tbl %>% plot_missing()

data_tbl %>% DataExplorer::create_report()


# * 1.1 Chraracter Columns ----
data_tbl %>% count(CustomerStatus) # almost all Active

data_tbl %>% count(CustomerTypeGroup) # almost all Individual 

data_tbl %>% count(CustomerSegment) # almost all Individual

data_tbl %>% count(SalesChannel) # mostly unknown

data_tbl %>% count(Gender) # 46 out of 60k NA

data_tbl %>% count(MaritalStatus) # mostly NA

data_tbl %>% count(Nationality) %>% arrange(-n)

data_tbl %>% count(Occupation) %>% arrange(-n) %>% view()

# Raw data set EDA ----
data_tbl %>% 
    
    # columns to remove 
    select(- c(Cycle, CustomerStatus, SalesChannel, Gender, MaritalStatus)) %>% 
    
    # changing chr to fct
    mutate(across(c(CustomerSegment, Nationality, Occupation), .fns = ~ as_factor(.)))

data_missing_imputed_tbl <- data_tbl %>% 
    set_missing(value = list(0, "unknown"))

data_missing_imputed_tbl %>% glimpse()

data_missing_imputed_tbl %>% plot_missing()

data_missing_imputed_tbl %>% DataExplorer::create_report()
