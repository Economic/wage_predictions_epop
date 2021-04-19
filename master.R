library(tidyverse)
library(here)
library(tidymodels)
library(epiextractr)
library(lubridate)
library(arrow)
library(fixest)
library(slider)
library(hrbrthemes)
library(finetune)
# also requires: MetricsWeighted, data.table

# load misc functions used throughout code
source("helpers.R")

################################################################################
# processing parameters
################################################################################
# Training sample fraction
training_sample_prop <- 0.50

# CV folds
folds <- 10
seed <- 88261
num_threads <- 14

# define predictors
outcome <- "log_wage"
continuous_pred_vars <- "age"
categorical_pred_vars <- c(
  "educ",
  "wbho",
  "female",
  "married",
  "metro",
  "statefips"
)
all_predictors <- c(continuous_pred_vars, categorical_pred_vars)


################################################################################
# create training and testing data and run models
################################################################################
source("clean_cps_wages.R")
source("run_models.R")

################################################################################
# other stuff
################################################################################
#source("predict_testing.R")
#source("predict_basic.R")
#source("analyze_epops.R")