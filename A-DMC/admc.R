
# setup ----

options(scipen = 999)

library(tidyverse)
library(lavaan)

setwd("C:/Users/jhelmer3/OneDrive - Georgia Institute of Technology/FPT/cog data")

# reading ----

admc <- read.csv("data_admc_raw.csv")
session <- read.csv("session.csv")

rf_duplicates <- admc %>%
  left_join(session %>%
              select(subject_id, session_id), by = "session_id") %>% 
  select(subject_id, everything()) %>%
  filter(trial_name == "resistance_to_framing_trial") %>%
  summarize(n = n(), .by = c(subject_id, admc_id)) %>%
  filter(n > 1L) %>%
  pull(subject_id) %>%
  unique()

rf <- admc %>%
  left_join(session %>%
              select(subject_id, session_id), by = "session_id") %>% 
  select(subject_id, everything()) %>%
  filter(trial_name == "resistance_to_framing_trial") %>% 
  filter(!(subject_id %in% rf_duplicates)) %>%
  separate(admc_id, into = c("rf_type", "rf_itemid"), sep = "_") %>%
  select(subject_id, rf_type, rf_itemid, response) %>%
  pivot_wider(names_from = rf_type,
              values_from = response)

dr <- admc %>%
  filter(trial_name == "admc_dr_trial")

rp <- admc %>%
  select(session_id, trial_name, admc_response) %>%
  filter(trial_name == "admc_rp_a_trial" | trial_name == "admc_rp_b_trial") %>%
  mutate(rp_response = as.list(strsplit(substr(admc_response, 2, nchar(admc_response) - 1), ",")),
         timeframe = factor(ifelse(trial_name == "admc_rp_a_trial", "n1y", "n5y")),
         .keep = "unused") %>%
  unnest_wider(rp_response, names_sep = "_") %>%
  mutate(across(starts_with("rp_response"), ~ as.numeric(gsub("'", "", .x)))) %>% 
  pivot_longer(starts_with("rp_response"), names_to = "item", values_to = "response") %>%
  pivot_wider(names_from = timeframe, values_from = response) %>%
  mutate(item = substr(item, 13, length(item)))

temporal <- rp %>% 
  mutate(temporal = as.numeric(n1y <= n5y)) %>%
  select(session_id, item, temporal) %>%
  mutate(item = paste0("temporal_", item)) %>%
  pivot_wider(names_from = item, values_from = temporal)

subs <- rp %>%
  pivot_longer(c(n1y, n5y), names_to = "timeframe", values_to = "response") %>%
  pivot_wider(names_from = item, values_from = response) %>%
  mutate(subset_1 = as.numeric(`2` <= `9`),
         subset_2 = as.numeric(`6` <= `3`),
         subset_3 = as.numeric(`7` <= `4`)) %>%
  select(session_id, timeframe, subset_1, subset_2, subset_3) %>%
  pivot_wider(names_from = timeframe, values_from = c(subset_1, subset_2, subset_3))


complementary <- rp %>%
  pivot_longer(c(n1y, n5y), names_to = "timeframe", values_to = "response") %>%
  pivot_wider(names_from = item, values_from = response) %>%
  mutate(complementary_1 = as.numeric(`1` + `10` < 99),
         complementary_2 = as.numeric(`5` + `8` < 99)) %>%
  select(session_id, timeframe, complementary_1, complementary_2) %>%
  pivot_wider(names_from = timeframe, values_from = c(complementary_1, complementary_2))

rp_scores <- temporal %>% 
  left_join(subs, by = "session_id") %>%
  left_join(complementary, by = "session_id")


temporal_pers <- temporal %>%
  mutate(temporal = rowMeans(across(where(is.numeric))),
         .keep = "unused")

subs_pers <- subs %>%
  mutate(subs = rowMeans(across(where(is.numeric))),
         .keep = "unused")

complementary_pers <- complementary %>%
  mutate(complementary = rowMeans(across(where(is.numeric))),
         .keep = "unused")


rp_scores_pers <- temporal_pers %>%
  left_join(subs_pers, by = "session_id") %>%
  left_join(complementary_pers, by = "session_id") %>%
  mutate(rp_score = rowMeans(across(!session_id)))

dr_scores_pers <- dr %>%
  summarize(.by = session_id,
            dr_score = sum(as.numeric(admc_response)) / 15)

rf_scores_pers <- rf %>%
  mutate(across(starts_with("a") | starts_with("rc"), as.numeric),
         a = abs(a1 - a2),
         rc = abs(rc1 - rc2),
         .keep = "unused") %>%
  select(subject_id, rf_itemid, a, rc) %>%
  pivot_wider(names_from = rf_itemid,
              values_from = c(a, rc),
              names_sep = "") %>%
  .[complete.cases(.), ] 

summary(rf_scores_pers)

rf_scores_pers %>% pivot_longer(!subject_id,
                                names_to = "item",
                                values_to = "response") %>%
  ggplot(aes(x = response)) +
  geom_histogram(bins = 6) +
  facet_wrap(vars(item)) +
  theme_minimal()

shapiro.test(rf_scores_pers$rc5)

firstorder <- '

  rtof =~ a1 + a2 + a3 + a4 + a5 + a6 + a7 + rc1 + rc2 + rc3 + rc4 + rc5 + rc6 + rc7
  
'
firstorder_cfa <- lavaan::cfa(firstorder, data = rf_scores_pers, estimator = "MLR")
summary(firstorder_cfa, fit.measures = T, standardized = T)
semPlot::semPaths(firstorder_cfa, 'std', 'est', layout = "tree2")


secondorder <- '

  attribute =~ a1 + a2 + a3 + a4 + a5 + a6 + a7
  riskychoice =~ rc1 + rc2 + rc3 + rc4 + rc5 + rc6 + rc7
  
'
secondorder_cfa <- lavaan::cfa(secondorder, data = rf_scores_pers, estimator = "MLR")
summary(secondorder_cfa, fit.measures = T, standardized = T)
semPlot::semPaths(secondorder_cfa, 'std', 'est', layout = "tree2")


bif <- '

  attribute =~ a1 + a2 + a3 + a4 + a5 + a6 + a7
  riskychoice =~ rc1 + rc2 + rc3 + rc4 + rc5 + rc6 + rc7
  
  rtof =~ a1 + a2 + a3 + a4 + a5 + a6 + a7 + rc1 + rc2 + rc3 + rc4 + rc5 + rc6 + rc7
  
  attribute ~~ 0 * riskychoice
  attribute ~~ 0 * rtof
  riskychoice ~~ 0 * rtof
  
'
bif_cfa <- cfa(bif, data = rf_scores_pers, estimator = "MLR")
summary(bif_cfa, fit.measures = T, standardized = T)

png(filename="PSYC7302_bif.png", bg = "transparent", units = "in", width = 9.3, height = 4.6, res = 800)
 semPlot::semPaths(bif_cfa, whatLabels = 'std', what = "std", layout = "tree2", bifactor = "rtof", edge.color = "black") #%>%
#   png(filename = "PSYC7302_bif.png", bg = "transparent", units = "in", width = 9.3, height = 4.6, res = 600)
 dev.off()


rbind(
  fitmeasures(firstorder_cfa, fit.measures = c("df", "chisq", "pvalue", "cfi", "tli", "rmsea", "aic", "bic")) %>%
    as.matrix() %>%
    t() %>% 
    as.data.frame %>%
    mutate(Model = "First Order",
           .before = "df"),
  fitmeasures(secondorder_cfa, fit.measures = c("df", "chisq", "pvalue", "cfi", "tli", "rmsea", "aic", "bic")) %>%
    as.matrix() %>%
    t() %>% 
    as.data.frame %>%
    mutate(Model = "Second Order",
           .before = "df"),
  fitmeasures(bif_cfa, fit.measures = c("df", "chisq", "pvalue", "cfi", "tli", "rmsea", "aic", "bic")) %>%
    as.matrix() %>%
    t() %>% 
    as.data.frame %>%
    mutate(Model = "Bifactor",
           .before = "df")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  gt::gt()
































