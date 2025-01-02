library(tidyverse)
library(apaTables)
library(sjPlot)
library(readxl)
library(dataedu)

# pre-survey for the F15 and S16 semesters
pre_survey <- dataedu::pre_survey

# gradebook and log-trace data for F15 and S16 semesters
course_data <- dataedu::course_data

# log-trace data for F15 and S16 semesters - for time spent
course_minutes <- dataedu::course_minutes

pre_survey

course_data

course_minutes

# rename pre_survey data as a new object with the same name "pre_survey"
pre_survey <-
  pre_survey %>%
  # rename the questions to smt easier to work with 
  rename(
    q1 = Q1MaincellgroupRow1,
    q2 = Q1MaincellgroupRow2,
    q3 = Q1MaincellgroupRow3,
    q4 = Q1MaincellgroupRow4,
    q5 = Q1MaincellgroupRow5,
    q6 = Q1MaincellgroupRow6,
    q7 = Q1MaincellgroupRow7,
    q8 = Q1MaincellgroupRow8,
    q9 = Q1MaincellgroupRow9,
    q10 = Q1MaincellgroupRow10
  ) %>%
  # convert all question responses to numeric
  mutate_at(vars(q1:q10), list( ~ as.numeric(.)))

# dataset of students
df <- tibble(
  male = 5,
  female = 5
)

# use mutate to crease a column called total_students  
df %>% mutate(total_students = male + female)

# function for reversing scales
reverse_scale <- function(question) {
  # argument: questions
  # returns: a numeric converted response
  x <- case_when(
    question == 1 ~ 5,
    question == 2 ~ 4,
    question == 3 ~ 3, 
    question == 4 ~ 2,
    question == 5 ~ 1,
    TRUE ~ NA_real_
  )
  x
}

pre_survey <-
  pre_survey %>%
  mutate(q4 = reverse_scale(q4),
         q7 = reverse_scale(q7))

measure_mean <-
  pre_survey %>%
  # gather questions and responses
  pivot_longer(cols = q1:q10,
               names_to = "question",
               values_to = "response")

# indicate question category
measure_mean <- measure_mean %>%
  mutate(
    measure = case_when(
      question %in% c("q1", "q4", "q5", "q8", "q10") ~ "int",
      question %in% c("q2", "q6", "q9") ~ "uv",
      question %in% c("q3", "q7") ~ "pc",
      TRUE ~ NA_character_)
  )

measure_mean <- measure_mean %>%
  group_by(measure) %>%
  summarize(
    mean_response = mean(response, na.rm = TRUE),
    # variable to indicate the percent of each measure that had NAs in the response field
    percent_NA = mean(is.na(response))
  )

measure_mean

# split course section into components
course_data <- 
  course_data %>%
  separate(
    col = CourseSectionOrigID,
    into = c("subject", "semester", "section"),
    sep = "-",
    remove = FALSE
  )

# rename variables
pre_survey <-
  pre_survey %>%
  rename(student_id = opdata_username, 
         course_id = opdata_CourseID)

pre_survey

# recreate "student_id" to exclude extraneous characters
pre_survey <- pre_survey %>%
  mutate(student_id = str_sub(student_id, start=2, end=-3))

# convert string to int
pre_survey <- pre_survey %>%
  mutate(student_id = as.numeric(student_id))

course_data <-
  course_data %>%
  rename(student_id = Bb_UserPK,
         course_id = CourseSectionOrigID)

course_minutes <-
  course_minutes %>%
  rename(student_id = Bb_UserPK,
         course_id = CourseSectionOrigID)

course_mintutes <-
  course_minutes %>%
  # change the data type for student_id in course_mintues to match student_id in dat
  mutate(student_id = as.integer(student_id))

dat <-
  dat %/%
  left_join(course_data, pre_survey, 
            by = c("student_id", "course_id"))

dat

glimpse(dat)

distinct(dat, course_id, Gradebook_Item)

dat <- 
  distinct(dat, course_id, student_id, .keep_all = TRUE)

dat <- rename(dat, final_grade = "FinalGradeCEMS")

students <- tibble(
  school_id = c("a","b","c"),
  mean_score = c(10,20,30)
)

students %>%
  ggplot(aes(x = school_id, y = mean_score)) + 
  #draw the plot 
  geom_bar(stat = "identity",
           fill = dataedu_colors("darkblue")) + 
  theme_dataedu()

dat %>%
  ggplot(aes(x = TimeSpent, y = final_grade)) + 
  geom_point(color = dataedu_colors("green")) + 
  # "lm": fit the line using linear regression
  geom_smooth(method = "lm") + 
  theme_dataedu() +
  labs(x = "Time Spent",
       y = "Final Grade")

m_linear <- 
  # predict final_grade using TimeSpent (independent)
  lm(final_grade ~ TimeSpent, data=dat)

summary(m_linear)

tab_model(m_linear, 
          title = "Table 7.1")

apa.reg.table(m_linear, filename = "regression-table-output.doc")

survey_responses <-
  pre_survey %>%
  pivot_longer(cols = q1:q10,
               names_to = "question",
               values_to = "response") %>%
  mutate (
    # make the column of question categories 
    measure = case_when(
      question %in% c("q1", "q4", "q5", "q8", "q10") ~ "int",
      question %in% c("q2", "q6", "q9") ~ "uv",
      question %in% c("q3", "q7") ~ "pc",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(student_id, measure) %>% 
  # calculate mean of responses 
  summarize(
    # mean response for each measure
    mean_response = mean(response, na.rm = TRUE)
  ) %>% 
  # filter NA (missing responses)
  filter(!is.na(mean_response)) %>% # removes data that does not match the criteria
  pivot_wider(names_from = measure,
              values_from = mean_response)

survey_responses %>%
  apa.cor.table()

dat <- 
  dat %>%
  mutate(TimeSpent_hours = TimeSpent/60)

m_linear_1 <- 
  lm(final_grade ~ TimeSpent_hours, data=dat)

tab_model(m_linear_1,
          title = "Table 7.2")

# standardize data to have mean of 0 and std of 1
dat <- 
  dat %>%
  mutate(TimeSpent_std = scale(TimeSpent))

m_linear_2 <- 
  lm(final_grade ~ TimeSpent_std, data=dat)

tab_model(m_linear_2,
          title = "Table 7.3")

m_linear_3 <- 
  lm(final_grade ~ TimeSpent_std + subject, data = dat)

tab_model(m_linear_3,
          title = "Table 7.4")
