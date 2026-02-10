
options(scipen = 999)

library(tidyverse)

load(here::here("Data", "data_processed.RData"))
final_completers <- rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/metadata_tables/final_completers.csv")
session <- rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/metadata_tables/session.csv")
# anchoraig_full <- rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/metadata_tables/task_aig_version.csv")

dat <- data |>
  left_join(session |> select(subject_id, session_id),
            by = "session_id") |>
  filter(subject_id %in% pull(final_completers, subject_id)) |>
  select(-c(session_restart_id, custom_timer_ended_trial, trial_index,
            block, trial)) |>
  # getting to just instructions or pt (practice trials?)
  # practice trials have same `trial_type` and `trial_name` as regular trials.
  # seem to be differentiated by `pt_trial`
  mutate(pt_trial = ifelse(is.na(pt_trial), F, pt_trial), 
         startup = ifelse((trial_type == "instructions" | pt_trial == T), 1, 0),
         .before = trial_name) |>
  # taking out words in `trial_name` so they better match the typical naming
  mutate(version = case_when(trial_name == "bayesian_update_general_instructions" ~ lead(version, 3),
                             trial_name == "bayesian_update_pt_trials_instructions" ~ lead(version, 1),
                             trial_name == "bayesian_update_test_trials_instructions" ~ lead(version, 1),
                             trial_name == "bayesian_update_between_unique_trials_trial" ~ lag(version, 1),
                             .default = version),
         unique_trial = case_when(trial_name == "bayesian_update_between_unique_trials_trial" ~ lag(unique_trial, 1),
                                  .default = unique_trial),
         task_version = case_when(trial_name == "dn_general_instructions" ~ lead(task_version, 4),
                                  trial_name == "dn_pt_trials_instructions" ~ lead(task_version, 2),
                                  trial_name == "dn_test_trials_instructions" ~ lag(task_version, 2),
                                  .default = task_version),
         # aig_version = case_when(trial_name == "bayesian_update_general_instructions" ~ lead(aig_version, 3),
         #                     trial_name == "bayesian_update_pt_trials_instructions" ~ lead(aig_version, 1),
         #                     trial_name == "bayesian_update_test_trials_instructions" ~ lead(aig_version, 1),
         #                     trial_name == "bayesian_update_between_unique_trials_trial" ~ lag(aig_version, 1),
         #                     .default = aig_version),
         task = gsub("_instructions|_trial[s]*|_pt|_practice|_test|_matrix|_main|_version_A|_general|_between_unique",
                     replacement = "", x = trial_name),
         # specifying the task version in the name
         task = ifelse(task == "dn", paste0("denominator_neglect_version_", task_version),
                       ifelse(task == "bayesian_update", paste0(task, "_", version), task)),
         # this trial name is for the screen in between bayesian update trials ("this was the correct box")
         # this line groups the value of startup with whether they're from a practice or real trial
         startup = ifelse(trial_name == "bayesian_update_between_unique_trials_trial", lag(startup, 1), startup),
         .after = session_id) |>
  # getting just relevant rows
  filter(!is.na(task) & !is.na(rt) & !str_detect(task, "break|_NA|feedback|fixation_cross|session_parameters|async_data_save|browser_check|fullscreen"))
  

# first priority: get startup cost and average item time based on average item time. see how those compare to FPT paper's stuff. 
# then consider specific item time differences. if relevant.

startup_costs <- dat |>
  filter(startup == 1) |>
  summarize(.by = c(session_id, task),
            rt = sum(rt)) |>
  summarize(.by = c(task),
            rt = median(rt, na.rm = T) / 1000 / 60) |>
  mutate(task = case_when(task == "bayesian_update_easy" ~ "bu.e",
                          task == "bayesian_update_hard" ~ "bu.h",
                          task == "denominator_neglect_version_A" ~ "dn.s",
                          task == "denominator_neglect_version_B" ~ "dn.c",
                          .default = task),
         # gives BU and DN own grouping variable (none others have "." in name)
         higher_task = str_split_i(task, "\\.", 1)) |>
  # sets cost of BU and DN to be the max cost (one that took longer to start up)
  # since people saw both versions, second start up cost lower. this prevents that advantage
  mutate(.by = higher_task,
         startup = max(rt)) |>
  mutate(task = case_when(task == "admc_dr" ~ "admc.dr",
                          task == "number_series" ~ "ns",
                          task == "time_series" ~ "ts",
                          .default = task)) |>
  select(task, startup)

saveRDS(startup_costs, here::here("Data", "startup_costs.rds"))

# dat |>
#   summarize(.by = c(session_id, task),
#             rt = sum(rt)) |>
#   summarize(.by = c(task),
#             rt = median(rt, na.rm = T) / 1000 / 60) |>
#   print(n = Inf)
# 
# dat |> select(session_id, trial_name, trial_type,
#                pt_trial, version, rt, task, task_version) |>
#   View()
# 
# 
# # want to consider how to group these timings. note the `bayesian_update_between_unique_trials_trial` rows.
# # group those timings in with the trials themselves i'm guessing? also, no `version` for the instruction
# # trials. fill upwards?
# data |> select(session_id, trial_name, trial_type,
#                pt_trial, version, rt, unique_trial) |>
#   View()

# check out other tasks. see what's up with them.
