options(scipen = 999)

library(tidyverse)
library(skimr)
library(DataExplorer)


# Read sample data
data_tbl <- read_csv("sample.csv") 

# * EDA ----

data_tbl %>% skim()

data_tbl %>% head()

data_tbl %>% glimpse()

data_tbl %>% plot_missing()

data_tbl %>% DataExplorer::create_report()


# * Chraracter Columns ----
data_tbl %>% count(CustomerStatus) # almost all Active

data_tbl %>% count(CustomerTypeGroup) # almost all Individual 

data_tbl %>% count(CustomerSegment) # almost all Individual

data_tbl %>% count(SalesChannel) # mostly unknown

data_tbl %>% count(Gender) # 46 out of 60k NA

data_tbl %>% count(MaritalStatus) # mostly NA

data_tbl %>% count(Nationality) %>% arrange(-n)

data_tbl %>% count(Occupation) %>% arrange(-n) %>% view()

# * NA's imputation ----

data_missing_imputed_tbl <- data_tbl %>% 
    set_missing(value = list(0, "unknown"))

data_missing_imputed_tbl %>% glimpse()

data_missing_imputed_tbl %>% plot_missing()

data_missing_imputed_tbl %>% DataExplorer::create_report()

# * Column groups - string based

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

# * Columns: Missing Values % ----

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


# # Tidying the dataset (1 customer - 1 row)
# data_for_summarization_tbl <- data_tbl %>% 
#     
#     # Select columns for summarization
#     select(SubscriberID, campaign_1:campaign_16, NumVoicePrepaid:RechargeForPostpaid,
#            ActualMoUM_PRev:PromiseToPay, NumberOfDowngrades:ARPU) %>% 
#     
#     # Replace missing values
#     set_missing(value = list(0, "unknown"))
# 
# data_for_summarization_tbl %>% plot_missing()
# data_for_summarization_tbl %>% create_report()
# 
# names <- apply(X = is.na(data_for_summarization_tbl), MARGIN = 2, FUN = mean) %>% names() %>% as_tibble() 
# 
# column_na_prop_tbl <- names %>% 
#     bind_cols(
#         apply(X = is.na(data_for_summarization_tbl), MARGIN = 2, FUN = mean) %>% as_tibble()        
#     ) %>% rename(column = "value...1", na_prop = "value...2")
# 
# column_na_prop_tbl %>% arrange(na_prop) %>% view()
# 
# columns_to_check <- column_na_prop_tbl %>%
#     filter(na_prop > 0) %>%
#     pull(1)
# 
# data_tbl %>% 
#     select(columns_to_check) %>% 
#     filter(!is.na(GlobalVouchers))
#     plot_missing()

data_tbl %>% distinct(SubscriberID) # There are 50,331 unique customers in the dataset

data_tbl %>%
    mutate(SubscriberID = as.factor(SubscriberID)) %>% 
    count(SubscriberID) %>% 
    arrange(-n) %>% 
    filter(n > 1) # there are 8,630 customers with 2+ snapshots (rows) in the dataset
# => so there are 41,701 customers (50,331 - 8,630) with 1 snapshot 

one_snapshot_customers_tbl <-  data_tbl %>%
    #mutate(SubscriberID = as.factor(SubscriberID)) %>% 
    count(SubscriberID) %>% 
    arrange(-n) %>% 
    filter(n == 1) %>% 
    select(-n)

data_single_snap_customers_tbl <- data_tbl %>% 
    inner_join(one_snapshot_customers_tbl)

data_multi_snap_customers_tbl <- setdiff(data_tbl, data_one_snapshot_customers_tbl)

# * Snapshots ----

data_tbl %>% 
    filter(SubscriberID %in% c(15586621683439, 21285609711439)) %>% 
    view()

data_single_snap_customers_tbl %>% 
    slice(20:40) %>% 
    view()

data_tbl %>%
    mutate(SubscriberID = as.factor(SubscriberID)) %>% 
    count(SubscriberID) %>% 
    arrange(-n) %>% 
    filter(n == 2)

data_tbl %>% 
    filter(SubscriberID %in% c(15586621683439, 21285609711439, 975234957439, 20336183439, 68876657439, 79767733439)) %>%
    arrange(SubscriberID) %>% 
    view

data_tbl %>% head()

# The dataset contains duplicate rows; if we create unique ID created from SubscriberID, SnapshotDate, and all 16 campaing response columns
# we should be able to de-duplicate the dataset

deduplication_data_tbl <- data_tbl %>% 
    mutate(unique_id = str_c(SubscriberID, "_", SnapshotDate, "_", campaign_1, campaign_2, campaign_3, campaign_4, campaign_5, campaign_6,
                             campaign_7, campaign_8, campaign_9, campaign_10, campaign_11, campaign_12, campaign_13, campaign_14,
                             campaign_15, campaign_16)) %>% 
    select(unique_id, everything()) 
    # filter(SubscriberID %in% c(15586621683439, 21285609711439, 975234957439, 20336183439, 68876657439, 79767733439)) %>% 
    # view()

# Manual Deduplication
deduplication_data_tbl %>% 
    group_by(unique_id) %>% 
    mutate(counter = row_number()) %>% 
    ungroup() %>% 
    select(unique_id, counter, everything()) %>% 
    filter(counter == 1) %>% 
    count()

# Auto Deduplication
deduplicated_data_tbl <- data_tbl %>% 
    select(-...1) %>% 
    distinct()

data_tbl %>% names()

deduplicated_data_tbl %>% 
    count(SnapshotDate)

deduplicated_data_tbl %>% skim()    

# * Character Variables ----

deduplicated_data_tbl %>% 
    count(Cycle) %>% 
    arrange(-n)

deduplicated_data_tbl %>% 
    count(CustomerStatus) %>% 
    arrange(-n)

deduplicated_data_tbl %>% 
    count(CustomerTypeGroup) %>% 
    arrange(-n)

deduplicated_data_tbl %>% 
    filter(CustomerTypeGroup == "Business Customer") %>% 
    select(campaign_1:campaign_16) %>% 


deduplicated_data_tbl %>% 
    count(CustomerSegment) %>% 
    arrange(-n)

deduplicated_data_tbl %>% 
    count(SalesChannel) %>% 
    arrange(-n)

deduplicated_data_tbl %>% 
    count(Gender) %>% 
    arrange(-n)

deduplicated_data_tbl %>% 
    count(MaritalStatus) %>% 
    arrange(-n)

deduplicated_data_tbl %>% 
    count(Nationality) %>% 
    arrange(-n)

deduplicated_data_tbl %>% 
    count(Occupation) %>% 
    arrange(-n)

# * Date Variables ----

deduplicated_data_tbl %>% 
    count(SnapshotDate) %>% 
    arrange(-n)

deduplicated_data_tbl %>% 
    count(RechargeMonth) %>% 
    arrange(-n)

# * Logical Variables ----

deduplicated_data_tbl %>% 
    select(where(is.logical)) %>%
    names() %>% 
    view()

# * Numeric Variables ----

deduplicated_numeric_vars_tbl <- deduplicated_data_tbl %>% 
    select(where(is.numeric))

names <- apply(X = is.na(deduplicated_numeric_vars_tbl), MARGIN = 2, FUN = mean) %>% names() %>% as_tibble() 

column_na_prop_tbl <- names %>% 
    bind_cols(
        apply(X = is.na(deduplicated_numeric_vars_tbl), MARGIN = 2, FUN = mean) %>% as_tibble()        
    ) %>% rename(column = "value...1", na_prop = "value...2")

column_na_prop_tbl %>% arrange(na_prop) %>% view()

columns_to_remove <- column_na_prop_tbl %>% 
    filter(na_prop > 0.5) %>% 
    pull(1) # 113

columns_to_keep <- column_na_prop_tbl %>% 
    filter(na_prop < 0.5) %>% 
    pull(1) # 69
