library(tidyverse)
library(dummies)
library(sjPlot)
library(lme4)
library(performance)
library(dataedu)

str(iris)

# how many possible values are there for Species
levels(iris$Species)

d_iris <- 
  dummy.data.frame(iris)

get.dummy(d_iris, name = "Species") %>% 
  head()

# create matrix of dummy-coded variables
species_dummy_coded <- 
  get.dummy(d_iris, name = "Species")

# add dummy coded variables to iris
iris_with_dummy_codes <- 
  bind_cols(iris, species_dummy_coded)

iris_with_dummy_codes %>%
  count(Species, Speciessetosa, Speciesversicolor, Speciesvirginica)

dat <- dataedu::sci_mo_processed

dat %>%
  count(course_id)

dat <- 
  dat %>%
  rename(final_grade = "FinalGradeCEMS")

# predict student's final grade
m_linear_dc <- 
  lm(final_grade ~ TimeSpent_std + course_id, data = dat)

tab_model(m_linear_dc,
          title = "Table 13.1")

# convert course_id to a factor data type with another reference class
dat <- 
  dat %>%
  mutate(course_id = fct_relevel(course_id, "PhysA-S116-01"))

m_linear_dc_1 <- 
  lm(final_grade ~ TimeSpent_std + course_id, data=dat)

tab_model(m_linear_dc_1,
          title = "Table 13.2")

# when you don't want a single factor level to be the reference group - pass -1 as first val
m_linear_dc_2 <- 
  lm(final_grade ~ -1 + TimeSpent_std + course_id, data=dat)

tab_model(m_linear_dc_2,
          title = "Table 13.3")

m_course <- 
  # one intercept(|) for each group
  lmer(final_grade ~ TimeSpent_std + (1|course_id), data=dat)

tab_model(m_course, 
          title = "Table 13.4")

# specify a group effect for both the course and school
# when there are classes from different schools
m_course_school <- 
  lmer(final_grade ~ TimeSpent + (1|course_id) + (1|school_id), data = dat)
