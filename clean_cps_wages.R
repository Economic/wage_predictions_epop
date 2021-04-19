cps_wages <- prep_cps_basic(2016:2019) %>% 
  filter(wage_noimpute > 0)
assert_cps_version(cps_wages, "1.0.16")

# calculate wage percentiles for cutoffs and deflators
probs <- c(0.05, 0.50, 0.95)
cps_wage_percentiles <- cps_wages %>% 
  group_by(year) %>% 
  summarize(
    p = probs * 100,
    wage = MetricsWeighted::weighted_quantile(wage_noimpute,
                                                        w = orgwgt,
                                                        probs = probs)) %>% 
  pivot_wider(year, names_from = p, names_prefix = "wage_p", values_from = wage)

# median wage deflator base
wage_p50_2019 <- cps_wage_percentiles %>%
  filter(year == 2019) %>%
  pull(wage_p50)

cps_wages <- cps_wages %>% 
  full_join(cps_wage_percentiles, by = "year") %>% 
  filter(wage_noimpute >= wage_p5 & wage_noimpute <= wage_p95) %>% 
  mutate(log_wage = log(wage_noimpute * wage_p50_2019 / wage_p50)) %>%
  select(
    year, 
    month,
    id,
    minsamp,
    orgwgt,
    basicwgt,
    wage_noimpute,
    emp,
    all_of(outcome), 
    all_of(all_predictors),
    age2,
    age3) %>% 
  mutate(across(all_of(categorical_pred_vars), as.factor))

write_feather(cps_wages, here("data", "cps_wages.feather"))
