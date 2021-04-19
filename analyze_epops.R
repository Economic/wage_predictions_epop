basic_predictions <- read_feather("cps_basic_predictions.feather") %>%
  mutate(month_date = ym(paste0(year, "m", month)))

models <- c("ols", "xgboost", "ranger")

create_epops <- function(model) {
  
  grouping_var <- sym(paste0(model, "_pred_cat"))

  basic_predictions %>%
    group_by(month_date, prediction := !!grouping_var) %>%
    summarize(epop = weighted.mean(emp, w = basicwgt)) %>%
    mutate(prediction = case_when(
      prediction == 1 ~ "low",
      prediction == 2 ~ "middle",
      prediction == 3 ~ "high"
    )) %>% 
    ungroup() %>% 
    mutate(model = model)
}


overall_epop <- basic_predictions %>% 
  group_by(month_date) %>% 
  summarize(epop = weighted.mean(emp, w = basicwgt)) %>%
  mutate(model = "none")

epops <- map_dfr(models, create_epops) %>% 
  bind_rows(overall_epop) %>% 
  # seasonally adjust by calculating average calendar month difference
  # operationalize this by calculating month-specific deviations
  # via regression of demeaned epop on calendar month dummies
  # on pre-2020 data, avoiding pandemic,
  # and then predict demeaned epop for all time periods
  mutate(
    year = year(month_date),
    month = as.factor(month(month_date))
  ) %>% 
  group_by(model, prediction, year) %>% 
  mutate(epop_d = epop - mean(epop)) %>% 
  ungroup() %>% 
  group_by(model, prediction) %>% 
  nest() %>%
  # regress on pre-2020 data
  mutate(data_pre2020 = map(data, ~ filter(.x, year < 2020))) %>% 
  mutate(lm = map(data_pre2020, ~ lm(epop_d ~ month, data = .x))) %>% 
  # predict on original data
  mutate(delta = map2(lm, data, ~ predict(.x, newdata = .y))) %>% 
  mutate(data = map2(data, delta, ~ mutate(.x, delta = .y))) %>% 
  select(prediction, model, data) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  # finally calculate SA and NSA epops
  rename(epop_nsa = epop) %>% 
  mutate(epop_sa = epop_nsa - delta) %>% 
  select(model, prediction, month_date, epop_nsa, epop_sa) %>% 
  pivot_longer(starts_with("epop"), names_to = "sa", names_prefix = "epop_", values_to = "epop")

plot <- epops %>% 
  mutate(year = year(month_date)) %>% 
  group_by(model, prediction, sa, year) %>% 
  mutate(epop_2019 = if_else(year == 2019, mean(epop), NA_real_)) %>% 
  group_by(model, prediction, sa) %>% 
  mutate(epop = slide_index_dbl(epop, month_date, mean, .before = months(2), .complete = TRUE)) %>% 
  mutate(epop_2019 = max(epop_2019, na.rm = TRUE)) %>% 
  mutate(epop_pct = epop / epop_2019 - 1) %>% 
  filter(model == "ranger") %>%
  filter(sa == "sa") %>% 
  mutate(prediction = case_when(
    prediction == "high" ~ "Top 30%",
    prediction == "low" ~ "Bottom 30%",
    prediction == "middle" ~ "Middle 40%"
  )) %>% 
  mutate(name = paste(model, prediction, sa)) %>% 
  ggplot() +
  geom_point(aes(x = month_date, y = epop_pct, color = prediction)) +
  theme_ipsum_rc() +
  labs(y = "Percent change",
       title = "Percent change in EPOP from 2019 average, by predicted wage group",
       subtitle = "Seasonally adjusted and 3-month smoothed averages",
       caption = "EPI Current Population Extracts") +
  theme(axis.title.x = element_blank()) +
  #expand_limits(y = c(-0.3, 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
plot
ggsave("ranger_epops.png", width = 7, height = 4)

break

plot <- epops %>% 
  mutate(year = year(month_date)) %>% 
  group_by(model, prediction, sa, year) %>% 
  mutate(epop_2019 = if_else(year == 2019, mean(epop), NA_real_)) %>% 
  group_by(model, prediction, sa) %>% 
  mutate(epop = slide_index_dbl(epop, month_date, mean, .before = months(2), .complete = TRUE)) %>% 
  mutate(epop_2019 = max(epop_2019, na.rm = TRUE)) %>% 
  mutate(epop_pct = epop / epop_2019 - 1) %>% 
  mutate(epop_diff = epop - epop_2019) %>% 
  filter(model == "ranger") %>%
  filter(sa == "sa") %>% 
  mutate(prediction = case_when(
    prediction == "high" ~ "Top 30%",
    prediction == "low" ~ "Bottom 30%",
    prediction == "middle" ~ "Middle 40%",
    TRUE ~ "Overall"
  )) %>% 
  mutate(name = paste(model, prediction, sa)) %>% 
  ggplot() +
  geom_point(aes(x = month_date, y = epop_diff, color = prediction)) +
  geom_line(aes(x = month_date, y = epop_diff, color = prediction)) +
  theme_ipsum_rc() +
  labs(y = "Percentage point change",
       title = "Percentage point change in EPOP from 2019 average, by predicted wage group",
       subtitle = "Seasonally adjusted and 3-month smoothed averages",
       caption = "EPI Current Population Extracts") +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
plot