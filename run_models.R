# define subset of valid wage observations for training & testing
set.seed(seed)
cps_wages_split <- read_feather(here("data", "cps_wages.feather")) %>% 
  initial_split(prop = training_sample_prop, strata = year)

cps_training <- training(cps_wages_split) 
cps_testing <- testing(cps_wages_split)
cps_folds <- vfold_cv(cps_training, v = folds, strata = year)

# problems w/ factors in xgboost using add_variables()
# but not with add_formula. so here I construct the formula:
# see https://github.com/tidymodels/workflows/issues/44
# define ML model
ml_model_formula <- as.formula(
  paste(outcome, 
        paste(all_predictors, 
              collapse = " + "), 
        sep = " ~ "))
ml_model_formula


################################################################################
# linear model
################################################################################
linear_model <-
  linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")

linear_workflow <- 
  workflow() %>% 
  add_model(linear_model) %>% 
  add_variables(outcomes = all_of(outcome), predictors = c(all_of(all_predictors), age2, age3))

linear_fit_training <- fit(linear_workflow, cps_training)

linear_fit_training %>% 
  predict(new_data = cps_testing) %>% 
  bind_cols(cps_testing %>% select(log_wage)) %>% 
  metrics(truth = log_wage, estimate = .pred)


################################################################################
# random-forest
################################################################################
ranger_model <-
  rand_forest(
    mtry = tune(), 
    min_n = tune(), 
    trees = 1000
  ) %>% 
  set_mode("regression") %>% 
  set_engine(
    "ranger", 
    num.threads = num_threads,
    respect.unordered.factors = TRUE
  )

ranger_workflow <- 
  workflow() %>% 
  add_model(ranger_model) %>% 
  add_formula(ml_model_formula)

# ranger_tune <-
#   tune_grid(ranger_workflow,
#             resamples = cps_folds,
#             grid = 10,
#             control = control_grid(verbose = TRUE)
#   )

ranger_tune <-
  tune_race_anova(ranger_workflow,
            resamples = cps_folds,
            grid = 10,
            control = control_race(verbose = TRUE, verbose_elim = TRUE)
  )

ranger_workflow_final <- ranger_workflow %>%
  finalize_workflow(select_best(ranger_tune))

ranger_fit_training <- fit(ranger_workflow_final, cps_training)

ranger_fit_training %>% 
  predict(new_data = cps_testing) %>% 
  bind_cols(cps_testing %>% select(log_wage)) %>% 
  metrics(truth = log_wage, estimate = .pred)

break

################################################################################
# boosted trees
################################################################################

xgboost_model <- 
  boost_tree(
    trees = 1000, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## first three: model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune(),                         ## step size
  ) %>% 
  set_engine("xgboost", nthread = num_threads) %>% 
  set_mode("regression")

# from Doruk:
# learning rate = seq(0.001, 0.01, 0.002) num_trees = c(2500, 4000, 6000) and the tree depth as c(2, 4, 6)

xgboost_workflow <- 
  workflow() %>% 
  add_model(xgboost_model) %>% 
  add_formula(ml_model_formula)

xgboost_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), cps_training),
  learn_rate(),
  size = 30
)

xgboost_tune <-
  tune_grid(xgboost_workflow,
            resamples = cps_folds,
            grid = xgboost_grid,
            control = control_grid(verbose = TRUE)
  )

xgboost_workflow_final <- xgboost_workflow %>%
  finalize_workflow(select_best(xgboost_tune))

xgboost_fit_training <- fit(xgboost_workflow_final, cps_training)

xgboost_fit_training %>% 
  predict(new_data = cps_testing) %>% 
  bind_cols(cps_testing %>% select(log_wage)) %>% 
  metrics(truth = log_wage, estimate = .pred)

break






################################################################################
# random-forest
################################################################################
cps_ranger <- rand_forest(trees = 2000, mode = "regression") %>%
  set_engine(
    "ranger", 
    respect.unordered.factors = TRUE,
    num.threads = num_threads
  ) %>%
  fit(model_formula, data = cps_training)

saveRDS(cps_ranger, file = "ranger_model.rds")



################################################################################
# boosted trees
################################################################################
cps_xgboost <- boost_tree(trees = 4000, mode = "regression") %>% 
  set_engine(
    "xgboost",
    nthread = num_threads
  ) %>% 
  fit(model_formula, data = cps_training)
saveRDS(cps_xgboost, file = "xgboost_model.rds")



################################################################################
# OLS
################################################################################
ols_fe <- feols(
  # slightly different model than ML models
  log_wage ~ age + age2 + age3 | educ + wbho + female + married + metro + statefips,
  data = cps_training,
  weights = ~ orgwgt,
  combine.quick = FALSE
)
saveRDS(ols_fe, file = "ols_model.rds")




