# clean basic cps
prep_cps_basic <- function(years) { 
  load_basic(years) %>% 
  filter(age >= 25 & age <= 54) %>% 
  mutate(wage_noimpute = case_when(
    paidhre == 1 & a_earnhour == 1 ~ NA_real_,
    paidhre == 0 & a_weekpay == 1 ~ NA_real_,
    TRUE ~ wage_noadj
  )) %>% 
  mutate(
    wage_noimpute = if_else(wage_noimpute > 0, wage_noimpute, NA_real_),
    metro = metstat == 1 | is.na(metstat),
    age2 = age^2,
    age3 = age^3,
    id = row_number()
  )
}

# rf predictions tend to suck up a lot of memory
# to limit memory usage, iterate over years for predictions
model_subset_predict <- function(model, year, data) {
  
  data <- filter(data, year == {{year}})
  data_id <- select(data, id, basicwgt)
  
  # predict probs
  if (class(model)[[1]] == "fixest") {
    predictions <- data_id %>%
      mutate(.pred = predict(model, newdata = data))
  }
  else {
    predictions <- predict(model, data) %>%
      bind_cols(data_id) 
  }
}

predict_from_model <- function(model_name, data) {
  
  # determine prob threshold
  model <- readRDS(paste0(model_name, "_model.rds"))
  years <- unique(data$year)
  
  # predict probs for each subset and then combine
  all_years <- map(years, ~ model_subset_predict(model, .x, data)) %>% 
    data.table::rbindlist(use.names = TRUE) %>% 
    as_tibble()
  
  # define model-specific predicted low-wage status
  all_years %>% 
    mutate(low_threshold =
             MetricsWeighted::weighted_quantile(.pred,
                                                w = basicwgt,
                                                probs = 0.3),
           high_threshold =
             MetricsWeighted::weighted_quantile(.pred,
                                                w = basicwgt,
                                                probs = 0.7)) %>% 
    mutate(pred_cat = case_when(
      .pred <= low_threshold ~ 1L,
      .pred >= high_threshold ~ 3L,
      TRUE ~ 2L
    )) %>% 
    select(id, 
           "{model_name}_pred_cat" := pred_cat,
           "{model_name}_pred_wage" := .pred
    )
}



