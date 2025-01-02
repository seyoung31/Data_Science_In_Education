library(tidyverse)
library(apaTables)
library(sjPlot)
library(readxl)
library(dataedu)

# pre-survey for the F15 and S16 semesters 
pre_survey <- dataedu::pre_survey

# gradebook and log-trace data for F15 and S16 semesters
course_data <- dataedu::course_data

# log-trace data for F15 and S16 semesters - time spent
course_minutes <- dataedu::course_minutes

pre_survey <- 
  pre_survey %>% 
  # rename the questions something easier to work with 
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
  # convert question responses to numeric values
  # convert data in all 10 variables into a numeric format
  mutate_at(vars(q1:q10), list( ~ as.numeric(.)))

# Reverse scale of q4 and q7
reverse_scale <- function(question) {
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
               # column name
               names_to = "question",
               # value name
               values_to = "response")

# assigning type of measurement: interest, utility value, perceived competence
measure_mean <- measure_mean %>% 
  mutate(
    # create "measure" column
    measure = case_when(
      question %in% c("q1", "q4", "q5", "q8", "q10") ~ "int",
      question %in% c("q2", "q6", "q9") ~ "uv", 
      question %in% c("q3", "q7") ~ "pc",
      TRUE ~ NA_character_
    )
  )

# CALCULATING THE MEAN BY CATEGORY
measure_mean <- measure_mean %>% 
  # group by the new variable "measure"
  group_by(measure) %>% 
  # compute the mean of the responses
  summarize(
    mean_response = mean(response, na.rm = TRUE),
    # the percent of each measure that had NAs
    percent_NA = mean(is.na(response))
  )

measure_mean

course_data <-
  course_data %>% 
  # give course subject, semester, and section their own columns 
  separate(
    col = CourseSectionOrigID,
    into = c("subject", "semester", "section"),
    sep = "-",
    remove = FALSE,
  )

pre_survey <-
  pre_survey %>% 
  rename(student_id = opdata_username,
         course_id = opdata_CourseID)

pre_survey

# exclude the extraneous characters
pre_survey <- pre_survey %>%
  mutate(student_id = str_sub(student_id, start=2, end=-3))

pre_survey <- pre_survey %>% 
  mutate(student_id = as.numeric(student_id))

course_data <- 
  course_data %>% 
  rename(student_id = Bb_UserPK,
         course_id = CourseSectionOrigID)
dat <-
  left_join(course_data, pre_survey,
            by = c("student_id", "course_id"))

course_minutes <- 
  course_minutes %>% 
  rename(student_id = Bb_UserPK,
         course_id = CourseSectionOrigID)

course_minutes <- 
  course_minutes %>% 
  # change the data type for student_id to match that in dat
  mutate(student_id = as.integer(student_id))

dat <- 
  dat %>% 
  left_join(course_minutes,
            by = c("student_id", "course_id"))

dat <- 
  distinct(dat, course_id, student_id, .keep_all = TRUE)

dat <- 
  rename(dat, final_grade = FinalGradeCEMS)

students <-
  tibble(
    school_id = c("a","b","c"),
    mean_score = c(10,20,30)
  )

students %>% 
  # + meaning that there is an additional {ggplot2}layer to add
  ggplot(aes(x=school_id, y=mean_score)) +
  # draw the plot 
  geom_bar(stat = "identity",
           fill = dataedu_colors("darkblue")) +
  theme_dataedu()

dat %>%
  ggplot(aes(x = TimeSpent, y = final_grade)) + 
  # Creates a point with x- and y-axis coordinates specified above
  geom_point(color = dataedu_colors("green")) + 
  geom_smooth(method = "lm") +
  theme_dataedu() +
  labs(x = "Time Spent",
       y = "Final Grade")

# predict final grade using time spent 
m_linear <-
  lm(final_grade ~ TimeSpent, data=dat)

summary(m_linear)

tab_model(m_linear,
          title = "Table 7.1")

survey_responses <- 
  pre_survey %>% 
  
  # gather questions and responses 
  pivot_longer(cols = q1:q10,
               names_to = "question",
               values_to = "response") %>%
  
  # column of question categories 
  mutate(
    #make the column of question categories
    measure = case_when(
      question %in% c("q1", "q4", "q5", "q8", "q10") ~ "int",
      question %in% c("q2", "q6", "q9") ~ "uv",
      question %in% c("q3", "q7") ~ "pc",
      TRUE ~ NA_character_
    )
  ) %>% 
  
  group_by(student_id, measure) %>%
  
  # compute mean of responses 
  summarize(
    # mean for each measure 
    mean_response = mean(response, na.rm = TRUE)
  ) %>% 
  #filter NA (missing) responses
  filter(!is.na(mean_response)) %>% 
  
  pivot_wider(names_from = measure,
              values_from = mean_response)

survey_responses

survey_responses %>% 
  apa.cor.table() 

dat <- 
  dat %>% 
  mutate(TimeSpent_hours = TimeSpent / 60)

# same linear model, but with TimeSpent variable in hurs 
m_linear_1 <- 
  lm(final_grade ~ TimeSpent_hours, data = dat)

# view output of linear model 
tab_model(m_linear_1,
          title = "Table 7.2")

# TimeSpent variable to a mean of 0 and std of 1
dat <- 
  dat %>% 
  mutate(TimeSpent_std = scale(TimeSpent))

# linear model with TimeSpent standardized 
m_linear_2 <- 
  lm(final_grade ~ TimeSpent_std, data=dat)

# viewing the output 
tab_model(m_linear_2,
          title = "Table 7.3")

# add subject variable to determine the differences based on the course subject
m_linear_3 <-
  lm(final_grade ~ TimeSpent_std + subject, data = dat)

tab_model(m_linear_3,
          title = "Table 7.4")
