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

term <- "moum"

data_tbl %>%
    names() %>%
    as_tibble() %>% 
    filter(str_to_lower(value) %>% str_detect(term))

data_tbl %>% 
    select(contains(term)) %>% 
    #select(ChargeAmount:PostpaidBalance) %>% 
    #drop_na() %>% 
    slice(1:20)

# * Columns: Missing Values ----

names <- apply(X = is.na(data_tbl), MARGIN = 2, FUN = mean) %>% names() %>% as_tibble() 

column_na_prop_tbl <- names %>% 
    bind_cols(
        apply(X = is.na(data_tbl), MARGIN = 2, FUN = mean) %>% as_tibble()        
    ) %>% rename(column = "value...1", na_prop = "value...2")

column_na_prop_tbl %>% arrange(na_prop) %>% view()

# Columns NA proportion (203) 
column_na_prop_tbl 

# Complete columns (27)
column_na_prop_tbl %>% 
    filter(na_prop == 0)

# Columns with < 10 % missing observations (78)
column_na_prop_tbl %>% 
    filter(na_prop < 0.1)

# Columns with all observations missing (2)
column_na_prop_tbl %>% 
    filter(na_prop == 1)

# * Columns: Type ----

# ** Character (9) ----
char_column_tbl <- data_tbl %>% 
    select(where(is.character))

char_column_tbl

# ** Logical (5) ----
logical_column_tbl <- data_tbl %>% 
    select(where(is.logical))

logical_column_tbl
# columns TotalNETopUpCardAmount and GlobalVouchers can be removed as they do not contain any values

# ** Numeric (183) ---- 
data_tbl %>% 
    select(where(is.numeric))
