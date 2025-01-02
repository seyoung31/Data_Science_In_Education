library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(dataedu)

ExcelGradeBook <- read_excel("/Users/student/Documents/Programming/Data Science Education /Project/ExcelGradeBook.xlsx", sheet=1, skip=10)
# sheet=1 -> only read the first sheet 
# skip=10 -> skip the first 10 rows
gradebook <- ExcelGradeBook

head(colnames(gradebook))

gradebook <- 
  gradebook %>%
  clean_names()

head(colnames(gradebook))

gradebook <-
  gradebook %>%
  remove_empty(c("rows", "cols"))

# we want the dataset without absent and late
gradebook <-
  gradebook %>%
  select(-absent, -late)

classwork_df <-
  gradebook %>%
  select(
    name, 
    running_average,
    letter_grade,
    homeworks,
    classworks,
    formative_assessments,
    projects,
    summative_assessments,
    contains("classwork_")) %>%
  mutate_at(vars(contains("classwork_")), list(~ as.numeric(.))) %>%
  pivot_longer(
    cols = contains("classwork_"),
    names_to = "classwork_number",
    values_to = "score"
  )

summary(gradebook)

# bar graph for categorical variable 
gradebook %>%
  mutate(letter_grade = 
           factor(letter_grade, levels = c("A+",
                                           "A",
                                           "A-",
                                           "B+",
                                           "B",
                                           "B-",
                                           "C+"))) %>%
  ggplot(aes(x = letter_grade,
             fill = running_average > 90)) +
  geom_bar() +
  labs(title = "Bar Graph of Student Grades",
       x = "Letter Grades", 
       y = "Count",
       fill = "A or Better") + 
  scale_fill_dataedu() +
  theme_dataedu()

classwork_df %>%
  mutate(classwork_number = 
           factor(classwork_number, levels = c("classwork_1",
                                               "classwork_2",
                                               "classwork_3",
                                               "classwork_4",
                                               "classwork_5",
                                               "classwork_6",
                                               "classwork_7",
                                               "classwork_8",
                                               "classwork_9",
                                               "classwork_10",
                                               "classwork_11",
                                               "classwork_12",
                                               "classwork_13",
                                               "classwork_14",
                                               "classwork_15",
                                               "classwork_16",
                                               "classwork_17"))) %>%
  ggplot(aes(x = classwork_number,
             y = score,
             fill = classwork_number)) + 
  geom_boxplot(fill = dataedu_colors("yellow")) + 
  labs(title = "Distribution of Classwork Scores",
       x = "Classwork",
       y = "Scores") + 
  theme_dataedu() + 
  theme(
    # removes legend
    legend.position = "none",
    # angles the x axis labels
    axis.text.x = element_text(angle=45, hjust=1)
  )

# Scatterbplot to determine linear relationship
gradebook %>%
  ggplot(aes(x = formative_assessments,
             y = running_average)) + 
  geom_point(color = dataedu_colors("green")) + 
  labs(title = "Relationship Between Overall Grade and Formative Assessments",
       x = "Formative Assessment Score",
       y = "Overall Grade iin Percentage") + 
  theme_dataedu()

# Scatterbplot with line of best fit to determine linear relationship
gradebook %>%
  ggplot(aes(x = formative_assessments,
             y = running_average)) + 
  geom_point(color = dataedu_colors("green")) + 
  geom_smooth(method = "lm",
              se = TRUE) + 
  labs(title = "Relationship Between Overall Grade and Formative Assessments",
       x = "Formative Assessment Score",
       y = "Overall Grade in Percentage") + 
  theme_dataedu()

# Boxplot to find outliers
gradebook %>%
  ggplot(aes(x="",
             y = formative_assessments)) + 
  geom_boxplot(fill = dataedu_colors("yellow")) + 
  labs(title = "Distribution of Formative Assessment Scores",
       x = "Formative Assessment",
       y = "Score") +
  theme_dataedu()
gradebook %>%
  ggplot(aes(x="",
             y = running_average)) + 
  geom_boxplot(fill = dataedu_colors("yellow")) + 
  labs(title = "Distribution of Formative Assessment Scores",
       x = "Overall Grade",
       y = "Score in Percentage") +
  theme_dataedu()

cor(gradebook$formative_assessments, gradebook$running_average)

linear_mod <-
  lm(running_average ~ formative_assessments, data = gradebook)

summary(linear_mod)
