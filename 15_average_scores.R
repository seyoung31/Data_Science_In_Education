# quiz data to compute average scores
library(tidyverse)
set.seed(2020)

quizzes_1 <- tibble(
  teacher_id = 1,
  student_id = c(1:3),
  quiz_1 = sample(c(0:100), 3, replace = TRUE),
  quiz_2 = sample(c(0:100), 3, replace = TRUE),
  quiz_3 = sample(c(0:100), 3, replace = TRUE),
)

# compute summary statistics by grouping values in any of the new columns 
# mean quiz score for each student
quizzes_1 %>%
  pivot_longer(cols = quiz_1:quiz_3, names_to = "quiz_number", values_to = "score") %>%
  group_by(student_id) %>%
  summarize(quiz_mean = mean(score))

# add intervention column to first dataset
quizzes_1 <- quizzes_1 %>%
  mutate(intervention = sample(c(0, 1), 3, replace = TRUE))

# second imaginary dataset 
quizzes_2 <- tibble(
  teacher_id = 2,
  student_id = c(4:6),
  quiz_1 = sample(c(0:100), 3, replace = TRUE),
  quiz_2 = sample(c(0:100), 3, replace = TRUE), 
  quiz_3 = sample(c(0:100), 3, replace = TRUE), 
  intervention = sample(c(0, 1), 3, replace = TRUE)
)

# thiird imaginary dataset 
quizzes_3 <- tibble(
  teacher_id = 3,
  student_id = c(4:6),
  quiz_1 = sample(c(0:100), 3, replace = TRUE),
  quiz_2 = sample(c(0:100), 3, replace = TRUE), 
  quiz_3 = sample(c(0:100), 3, replace = TRUE), 
  intervention = sample(c(0, 1), 3, replace = TRUE)
)

# combine three quiz exports into one big dataset 
all_quizzes <- 
  bind_rows(quizzes_1, quizzes_2, quizzes_3)

all_quizzes %>%
  pivot_longer(cols = quiz_1:quiz_3, names_to = "quiz_number", values_to = "score") %>%
  # compute mean of each student 
  group_by(student_id, intervention) %>%
  summarize(quiz_mean = mean(score))
