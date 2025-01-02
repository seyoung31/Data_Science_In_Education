# data, analyses, studies changing over time 
# longitudinal analysis = analyses of data at multiple time points 

# 1. read multiple dataset using map()
# 2. cleaning variable names 
# 3. visualize student counts and compare male&female ratio

library(tidyverse)
library(dataedu)
library(lubridate)
library(here)

download.file( 
  # url argument takes a URL for a CSV file 
  url = 'https://bit.ly/3dCtVtf',
  #d specifies where the file should be saved
  destfile = here::here("data",
            "longitudinal_data",
            "bchildcountandedenvironments2012.csv"),
  mode = "wb")

download.file(
  url = 'https://bit.ly/33WXnFX', 
  destfile = here::here("data", 
                        "longitudinal_data",
                        "bchildcountandedenvironments2013.csv"),
  mode = "wb")

download.file(
  url = 'https://bit.ly/2UvSwbx', 
  destfile = here::here("data",
                        "longitudinal_data",
                        "bchildcountandedenvironments2014.csv"), 
  mode = "wb")

download.file(
  url = 'https://bit.ly/39wQAUg', 
  destfile = here::here("data",
                        "longitudinal_data",
                        "bchildcountandedenvironments2015.csv"), 
  mode = "wb")

download.file(
  url = 'https://bit.ly/2JubWHC', 
  destfile = here::here("data",
                        "longitudinal_data",
                        "bchildcountandedenvironments2016.csv"), 
  mode = "wb")

download.file(
  url = 'https://bit.ly/2wPLu8w',
  destfile = here::here("data",
                        "longitudinal_data",
                        "bchildcountandedenvironments2017-18.csv"), 
  mode = "wb")

read_csv(here::here(
  "data",
  "longitudinal_data",
  "bchildcountandedenvironments2012.csv"
),
skip = 4)

# Get filenames from the data folder 
filenames <-
  list.files(path = here::here("data", "longitudinal_data"),
             full.names = TRUE)

# A list of filenames and paths 
filenames

# Pass filenames to map and read_csv
all_files <-
  filenames %>%
  # apply the function to each element of filenames 
  map(., ~ read_csv(., skip=4))

# variables of first and second dataset don't match
identical(names(all_files[[1]]), names(all_files[[2]]))

identical(names(all_files[[2]]), names(all_files[[3]]))

all_files %>%
  # apply the function ncol to each element of all_files
  map(ncol)

all_files <- dataedu::all_files

# Look at the first 10 column names of 2016
names(all_files[[5]])[1:10]

filenames[[1]]
filenames[[5]]

all_files[[5]]

# fixing the 5th variable to include the col name
all_files[[5]] <- 
  # skip the first 3 lines instead of the first 4
  read_csv(filenames[[5]], skip=3)

all_files[[5]]

all_files[[1]] %>%
  select(
    Year, 
    contains("State", ignore.case = FALSE),
    contains("SEA", ignore.case = FALSE),
    contains("male")
  )

# build the function
pick_vars <-
  function(df) {
    df %>%
      select_at(vars(
        Year,
        contains("State", ignore.case = FALSE),
        contains("SEA", ignore.case = FALSE),
        contains("male")
      ))
  }

# use the function with "all_files"
all_files <-
  all_files %>%
  map(pick_vars)

# check variable names and number before combining
all_files %>%
  map(names)

all_files[[5]] <-
  all_files[[5]] %>%
  mutate(Year = as.numeric(Year))

# six datasets combined
child_counts <-
  all_files %>%
  bind_rows()

str(child_counts)

# importing processed dataset
longitudinal_data <- dataedu::child_counts

child_counts %>%
  # count number of times the category appears in the dataset 
  count(`SEA Disability Category`)

# filter out all subgroups except "All Disabilities"
child_counts <-
  child_counts %>%
  filter(
    `SEA Disability Category` == "All Disabilities",
    # filter all but the age totals
    `SEA Education Environment` %in% c("Total, Age 3-5", "Total, Age 6-21")
  )

child_counts <- 
  child_counts %>%
  rename(
    # change to more convenient names 
    year = Year,
    state = "State Name",
    age = "SEA Education Environment",
    disability = "SEA Disability Category",
    f_3_5 = "Female Age 3 to 5",
    m_3_5 = "Male Age 3 to 5",
    f_6_21 = "Female Age 6 to 21",
    m_6_21 = "Male Age 6 to 21"
  )

child_counts %>%
  count(state) %>%
  head()

child_counts <-
  child_counts %>%
  mutate(state = tolower(state))

child_counts <-
  child_counts %>%
  pivot_longer(cols = f_3_5:m_6_21,
               names_to = "gender",
               values_to = "total")

child_counts <-
  child_counts %>%
  mutate(
    gender = case_when(
      gender == "f_3_5" ~ "f",
      gender == "m_3_5" ~ "m",
      gender == "f_6_21" ~ "f",
      gender == "m_6_21" ~ "m",
      TRUE ~ as.character(gender)
    )
  )

child_counts <- 
  child_counts %>%
  mutate(total = as.numeric(total))

# convert character format to data format 
# truncated -> we don't have a month or data to convert
child_counts <-
  child_counts %>%
  mutate(year = ymd(year, truncated = 2))

child_counts %>%
  arrange(year, state, gender)

child_counts <-
  child_counts %>%
  filter(!is.na(total))

child_counts %>%
  arrange(year, state, gender)

# ============== ANALYSIS
# the number of students in special education over time 
# compare count of male and female students 
# quantify differences 

# which state has the highest mean count of students with disabilities
child_counts %>%
  group_by(state) %>%
  summarize(mean_count = mean(total)) %>%
  top_n(6, mean_count)

# use only 5 states with the highest count 
high_count <- 
  child_counts %>%
  filter(state %in% c("california", "florida", "new your", "pennsylvania", "texas"))

high_count %>%
  filter(gender == "m", age == "Total, Age 6-21") %>%
  ggplot(aes(x = year, y = total, color = state)) +
  geom_freqpoly(stat = "identity", size = 1) + 
  labs(title = "Count of Female Students in Special Education Over Time",
       subtitle = "Ages 6-21") + 
  scale_color_dataedu() + 
  theme_dataedu()

high_count %>%
  group_by(year, state) %>%
  summarize(n = sum(total)) %>%
  ggplot(aes(x = state, y = n)) + 
  geom_boxplot(fill = dataedu_colors("yellow")) + 
  labs(title = "Median Students with Disabilities Count",
       subtitle = "All ages and genders, 2012-2017") + 
  theme_dataedu()

high_count_1 <-
  high_count %>%
  group_by(year, state, gender) %>%
  summarize(total = sum(total)) %>%
  # create new columns for male and female student counts
  pivot_wider(names_from = gender,
              values_from = total) %>%
  # create a new ratio column
  mutate(ratio = m/f)
  
high_count_1 %>%
  ggplot(aes(x=year, y=ratio, color=state)) + 
  geom_freqpoly(stat="identity", size=1) + 
  scale_y_continuous(limits = c(1.5, 2.5)) +
  labs(title = "Male Students to Female Students Ratio Over Time",
       subtitle = "Ages 6-21") + 
  scale_color_dataedu() +
  theme_dataedu()

child_counts %>%
  filter(age == "Total, Age 6-21") %>%
  pivot_wider(names_from = gender,
              values_from = total) %>%
  ggplot(aes(x=f,y=m)) + 
  geom_point(size=3, alpha=.5, color=dataedu_colors("green")) + 
  geom_smooth() + 
  labs(
    title = "Comparison of Female Students to Male Students in Special Education",
    subtitle = "Counts of students in each state, ages 6-21",
    x = "Female students",
    y = "Male students",
    captions = "Data: US Dept of Education"
  ) +
  theme_dataedu()

# identify outliers
child_counts %>%
  filter(age == "Total, Age 6-21") %>%
  pivot_wider(names_from = gender,
              values_from = total) %>%
  filter(f>500000) %>%
  select(year, state, age, f, m)

# filter outliers
child_counts %>%
  filter(age == "Total, Age 6-21") %>%
  pivot_wider(names_from = gender,
              values_from = total) %>%
  filter(f <= 500000) %>%
  ggplot(aes(x = f, y = m)) + 
  geom_point(size = 3, alpha = .5, color = dataedu_colors("green")) + 
  labs(
    title = "Comparison of Female Students to Male Students with Disabilities",
    subtitle = "Counts of students in each state, ages 6-21.\nDoes not include outlying areas and freely associated states",
    x = "Female students",
    y = "Male students",
    caption = "Data: US Dept of Education"
  ) +
  theme_dataedu()

model_data <- child_counts %>%
  filter(age == "Total, Age 6-21") %>%
  mutate(year = as.factor(year(year))) %>%
  pivot_wider(names_from = gender,
              values_from = total) %>%
  # exclude outliers
  filter(f <= 500000) %>%
  # compute male student to female student ratio
  mutate(ratio = m/f) %>%
  select(-c(age, disability))

ggplot(data = model_data, aes(x=year, y=ratio)) + 
  geom_jitter(alpha = .5, color = dataedu_colors("green")) + 
  labs(title = "Male to Female Ratio Across years (Jittered)") + 
  theme_dataedu()

# predict ratio using year
ratio_year <-
  lm(ratio ~ year, data = model_data)

summary(ratio_year)

model_data %>%
  pivot_longer(cols = c(f,m),
               names_to = "gender",
               values_to = "students") %>%
  ggplot(aes(x=year, y=students,color=gender)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma) + 
  labs(
    title = "Median Male and Female Student Counts in Special Education",
    subtitle = "Ages 6-21. Does not include outlying areas and freely associated states",
    x = "",
    y = "",
    caption = "Data: US Dept of Education"
  ) +
  scale_color_dataedu() +
  theme_dataedu()
  )
