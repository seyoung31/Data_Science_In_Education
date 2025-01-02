# use self-reported motivation and behavioral trace data to identify predictors of final course grade 
# determines educational success in terms of student interactions with an online science course. 

# exploring how many factors affect A -> machine learning approach

# Three main factors to consider 
# 1) predictive accuracy of the random forest model 
# 2) variable importance 
# 3) variance explained by the final random forest model 

library(tidyverse)
library(caret)
library(ranger)
library(e1071)
library(tidylog)
library(dataedu)

df <- dataedu::sci_mo_with_text

# select only the variables that we need 
df <-
  df %>%
  select(
    int, 
    uv,
    pc,
    time_spent,
    final_grade,
    subject,
    enrollment_reason,
    semester,
    enrollment_status,
    cogproc,
    social,
    posemo,
    negemo,
    n
  )

# eliminate any rows with missing values
df <- na.omit(df)

# determiine if ther are variables with no variability
nearZeroVar(df, saveMetrics = TRUE)

# remove variables with no varriability ini certain models
df <- 
  df %>%
  select(-enrollment_status)

# converting text variables into factors 
# continuous -> categorical variables
df <-
  df %>%
  mutate_if(is.character, as.factor)

# set seed to ensure the reproducibility
set.seed(2020)

# take 80% of data
trainIndex <- createDataPartition(df$final_grade,
                                  p = .8,
                                  list = FALSE,
                                  times = 1)

# temporary new variable 
# populate the rows with the numbers 1~464 in order
df <- 
  df %>%
  mutate(temp_id = 1:464)

# filter dataset to include rows inside the "trainIndex" vector
df_train <-
  df %>%
  filter(temp_id %in% trainIndex)

# get only thr rows NOT in the "trainIndex" vector 

df_test <-
  df %>%
  filter(!temp_id %in% trainIndex)

# delete temp_id
df <-
  df %>%
  select(-temp_id)
df_train <-
  df_train %>%
  select(-temp_id)
df_test <-
  df_test %>%
  select(-temp_id)

set.seed(2020)

# train model
# resampling using bootstrap
rf_fit <- train(final_grade ~ .,
                data = df_train, 
                method = "ranger")

rf_fit
# looking for a tuning parameter that minimize the
# Root Mean Square Error (RMSE) and maximize the variance (rsquared)
# in this case, it would be mtry = 19

set.seed(2020)

train_control <-
  trainControl(method = "repeatedcv",
                number = 10,
                repeats = 10)

rf_fit1 <-
  train(final_grade ~ .,
        data = df_train,
        method = "ranger",
        trControl = train_control)

# when we do not fix "min.node.size" to five 
# let "min.node.size" change and let "mtry" change as well 
set.seed(2020)

# a grid of different values of mtry, different splitrules, and different minimum node sizes to test
tune_grid <-
  expand.grid(
    mtry = c(2, 3, 7, 10, 19),
    splitrule = c("variance", "extratrees"),
    min.node.size = c(1, 5, 10, 15, 20)
  )

# fit a new model, using the tuning grid we created above
rf_fit2 <-
  train(final_grade ~ .,
        data = df_train,
        method = "ranger",
        tuneGrid = tune_grid)

rf_fit2

# select "finalModel" output from the rf_fit2 model
# use "$" instead of "select" because it is not a normal dataframe
# (stored random forest model as the model)
rf_fit2$finalModel

# ==============================================
# EXAMINING PREDICTIVE ACCURACY ON TEST DATA SET
set.seed(2020)

# create new column called pred to assign the predicted values
df_test_augmented <-
  df_test %>%
  mutate(pred = predict(rf_fit2, df_test),
         obs = final_grade)

# transform this new object(column) into a data frame
defaultSummary(as.data.frame(df_test_augmented))

# compare RMSE and Rsquared with those from rf_fit2

# which variables contributed most strongly to the predictions
set.seed(2020)

# same model as earlier with adding variable importance metric 
rf_fit2_imp <- 
  train(
    final_grade ~ .,
    data = df_train,
    method = "ranger",
    tuneGrid = tune_grid,
    importance = "permutation"
  )

# extract variable importance from this new model 
varImp(rf_fit2_imp)

# visualize 
varImp(rf_fit2_imp) %>%
  pluck(1) %>%
  rownames_to_column("var") %>%
  ggplot(aes(x = reorder(var, Overall), y = Overall)) + 
  geom_col(fill = dataedu_colors("darkblue")) + 
  coord_flip() + 
  theme_dataedu()

# COMPARING RANDOM FOREST TO LINEAR REGRESSION
# comparing the predictive accuracy of the model to a linear model 
# making a dataset that includes the predicted values from rf_fit2 model wiith the actual rf_fit2 values

# convert characters to factors
df_train_lm <-
  df_train %>%
  mutate_if(is.character, as.factor)

# create linear regression model using the same formula approach as in the random forest
lm_fit <-
  train(final_grade ~ .,
        data = df_train_lm,
        method = "lm")

# append predicted values to the training dataset for the linear model 
df_train_lm <-
  df_train %>%
  mutate(obs = final_grade,
         pred = predict(lm_fit, df_train_lm))

# append the predicted values to the training dataset for the random forest 
df_train_randomfor <-
  df_train %>%
  mutate(pred = predict(rf_fit2, df_train),
         obs = final_grade)

defaultSummary(as.data.frame(df_train_lm))
defaultSummary(as.data.frame(df_train_randomfor))
