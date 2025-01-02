library(tidyverse)

tibble(
  student = letters[1:10],
  school = rep(letters[11:15], 2),
  test_score = sample(0:100, 10, replace=TRUE)
)

tibble(
  student = letters[1:10],
  school = rep(letters[11:15], 2),
  test_score = sample(0:100, 10, replace=TRUE)
) %>%
  # aggregate by school
  group_by(school) %>%
  summarize(mean_score = mean(test_score))

library(tidyverse)
library(here)
library(janitor)
library(dataedu)
#library(tabulizer)

race_pdf <-
  dataedu::race_pdf

race_df <-
  race_pdf %>%
  # turn each page into a tibble
  map( ~ as_tibble(.x, .name_repair = "unique")) %>%
  # make data frame and remove unnecessary rows
  map_df(~slice(., -1:-2)) %>%
  # use descriptive column names
  set_names(
    c(
      "school_group",
      "school_name",
      "grade",
      "na_num", # Native American number of students
      "na_pct", # Native American percentage of students
      "aa_num", # African American number of students
      "aa_pct", # African American percentage
      "as_num", # Asian number of students
      "as_pct", # Asian percentage
      "hi_num", # Hispanic number of students
      "hi_pct", # Hispanic percentage
      "wh_num", # White number of students
      "wh_pct", # White percentage
      "pi_pct", # Pacific Islander percentage
      "blank_col",
      "tot" # Total number of students (from the Race PDF)
    )
  )

race_df2 <-
  race_df %>%
  # remove unnecessary columns
  select(-school_group, -grade, -pi_pct, -blank_col) %>%
  # filter to get grade-level numbers 
  filter(str_detect(school_name, "Total"),
         school_name != "Grand Total") %>%
  # clean up school names
  mutate(school_name = str_replace(school_name, "Total", "")) %>%
  # remove white space
  mutate_if(is.character, trimws) %>%
  # turn percentage into numberic and decimal format
  mutate_at(vars(contains("pct")), list( ~ as.numeric(str_replace(., "%", "")) / 100))

frpl_pdf <-
  dataedu::frpl_pdf

frpl_df <-
  frpl_pdf %>%
  # turn each page into a tibble
  map(~ as_tibble(.x, .name_repair = "unique")) %>%
  # make data frame and remove unnecessary rows
  map_df( ~ slice(.,-1)) %>%
  # use descriptive column names
  set_names(
    c(
      "school_name",
      "not_eligible_num", # Number of non-eligible students,
      "reduce_num", # Number of students receiving reduced price lunch
      "free_num",   # Number of students receiving free lunch
      "frpl_num",  # Total number of students (from the FRPL PDF)
      "frpl_pct" # Free/reduced price lunch percentage
    )
  )

frpl_df2 <-
  frpl_df %>%
  filter(
    # remove blanks
    school_name != "",
    # filter out the rows in this list
    !school_name %in% c(
      "ELM K_08",
      "Mid Schl",
      "High Schl",
      "Alt HS",
      "Spec Ed Total",
      "Cont Alt Total",
      "Hospital Sites Total",
      "Dist Total"
    )
  ) %>%
  # turn percentage columns into numeric and decimal format
  mutate_at(vars(contains("pct")), list( ~ as.numeric(str_replace(frpl_pct, "%", "")) / 100))

joined_df <-
  left_join(race_df2, frpl_df2, by = c("school_name")) %>%
  mutate_at(2:17, as.numeric)

district_merged_df <-
  joined_df %>%
  # calculate high poverty numbers 
  mutate(
    hi_povnum = case_when(frpl_pct > .75 ~ hi_num),
    aa_povnum = case_when(frpl_pct > .75 ~ aa_num),
    wh_povnum = case_when(frpl_pct > .75 ~ wh_num),
    as_povnum = case_when(frpl_pct > .75 ~ as_num),
    na_povnum = case_when(frpl_pct > .75 ~ na_num)
  ) %>%
  adorn_totals() %>%
  # create percentage by demographic
  mutate(
    na_pct = na_num / tot,
    aa_pct = aa_num / tot,
    as_pct = as_num / tot,
    hi_pct = hi_num / tot,
    wh_pct = wh_num / tot,
    frpl_pct = (free_num + reduce_num) / frpl_num,
    # create percentage by demographic and poverty 
    hi_povsch = hi_povnum / hi_num[which(school_name == "Total")],
    aa_povsch = aa_povnum / aa_num[which(school_name == "Total")],
    as_povsch = as_povnum / as_num[which(school_name == "Total")],
    wh_povsch = wh_povnum / wh_num[which(school_name == "Total")],
    na_povsch = na_povnum / na_num[which(school_name == "Total")]
  )

district_tidy_df <-
  district_merged_df %>%
  pivot_longer(
    cols = -matches("school_name"),
    names_to = "category",
    values_to = "value"
  )

district_tidy_df <- dataedu::district_tidy_df
district_merged_df <- dataedu::district_merged_df

district_tidy_df %>%
  # Filter for Total rows, since we want district-level information
  filter(school_name == "Total",
         str_detect(category, "pct"),
         category != "frpl_pct") %>%
  # Reordering x-axis so bars appear by descending value
  ggplot(aes(x = reorder(category, -value), y = value)) +
  geom_bar(stat = "identity", aes(fill = category)) +
  labs(title = "Percentage of Population by Subgroup",
       x = "Subgroup",
       y = "Percentage of Population") +
  # Make labels more readable
  scale_x_discrete(
    labels = c(
      "aa_pct" = "Black",
      "wh_pct" = "White",
      "hi_pct" = "Hispanic",
      "as_pct" = "Asian",
      "na_pct" = "Native Am."
    )
  ) +
  # Makes labels present as percentages
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_dataedu() +
  theme_dataedu() +
  theme(legend.position = "none")

district_tidy_df %>%
  filter(category == "frpl_pct",
         school_name == "Total")

# percentage of White students within schools 
district_merged_df %>%
  # remove district totals
  filter(school_name != "Total") %>%
  # x-axis will be the percentage of white students 
  ggplot(aes(x = wh_pct)) + 
  geom_histogram(breaks = seq(0, 1, by = .1),
                 fill = dataedu_colors("darkblue")) + 
  labs(title = "Count of Schools by White Population",
       x = "White Percentage",
       y = "Count") +
  scale_x_continuous(labels = scales::percent) + 
  theme(legend.position = "none") + 
  theme_dataedu()

district_tidy_df %>%
  filter(school_name == "Total",
         str_detect(category, "povsch")) %>%
  # order by descending values
  ggplot(aes(x = reorder(category, -value), y = value)) + 
  geom_bar(stat = "identity", aes(fill = factor(category))) + 
  labs(title = "Distribution of Subgroups in High Poverty Schools",
       x = "Subgroup",
       y = "Percentage in High Poverty Schools") +
  scale_x_discrete(
    labels = c(
      "aa_pct" = "Black",
      "wh_pct" = "White",
      "hi_pct" = "Hispanic",
      "as_pct" = "Asian",
      "na_pct" = "Native Am."
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_dataedu() +
  theme_dataedu() +
  theme(legend.position = "none")

#Correlate race and FRPL percentage
district_merged_df %>%
  filter(school_name != "Total") %>% 
  ggplot(aes(x = wh_pct, y = frpl_pct)) + 
  geom_point(color = dataedu_colors("green")) + 
  labs(title = "FRPL Percentage vs. White Percentage",
       x = "White Percentage",
       y = "FRPL Percentage") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  theme_dataedu() + 
  theme(legend.position = "none")
