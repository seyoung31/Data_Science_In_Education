library(tidyverse)
library(dataedu)
library(skimr)
library(janitor) # does not load 

dataedu::ma_data_init
# assign object name (ma_data)
dataedu::ma_data_init -> ma_data
# assign object name (ma_data_init)
ma_data_init <- dataedu::ma_data_init

names(ma_data_init)
# Typo ERROR
glimpse(ma_dat_init) # name error (does not match object name)
glimpse(ma_data_init)
summary(ma_data_init) # min, max, iq range 
glimpse(ma_data_init$Town)
summary(ma_data_init$Town)
# Spaces ERROR
glimpse(ma_data_init$AP_Test Takers) # item name not match 
# always use the BACKTIC when referring to names
glimpse(ma_data_init$`AP_Test Takers`)
summary(ma_data_init$`AP_Test Takers`)
# name_of_dataset$variable_in_datasets

# %>% -> "pipe" = "and then..."

# Typo ERROR (refer to name in backtics)
ma_data_init %>% 
  group_by(District Name) %>% 
  count()

# count the number of district names (frequency)
ma_data_init %>% 
  group_by(`District Name`) %>% 
  count()

# frequency가 10보다 큰 District Name 
ma_data_init %>% 
  group_by(`District Name`) %>% 
  count() %>% 
  filter(n>10)

# arrange in descending order
ma_data_init %>% 
  group_by(`District Name`) %>% 
  count() %>% 
  filter(n>10) %>% 
  arrange(desc(n))

# typo ERROR (==)
ma_data_init %>% 
  group_by(`District Name`) %>% 
  count() %>% 
  filter(n=10)

ma_data_init %>% 
  group_by(`District Name`) %>% 
  count() %>% 
  filter(n==10)

ma_data_init %>% 
  rename(district_name = `District Name`,
         grade = Grade) %>%  
  select(district_name, grade)

# ERROR - space in name
ma data <- 
  ma_data_init %>% 
  clean_names()

# ERROR - starts with num
01_ma_data <- 
  ma_data_init %>% 
  clean_names()

# ERROR - starts with symbol
$_ma_data <-
  ma_data_init %>% 
  clean_names()

ma_data_01 <-
  ma_data_init %>% 
  clean_names()

MA_data_02 <-
  ma_data_init %>% 
  clean_names()
