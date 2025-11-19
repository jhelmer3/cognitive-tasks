
# setup ----

options(scipen=999)

library(tidyverse)
library(mirt)
library(rstan)

here::here()

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = T)

# reading ----

dn_full <- rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/task_datasets/data_denominator_neglect.csv")

anchoraig_full <- rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/metadata_tables/task_aig_version.csv")

# cleaning ----

anchor_ids <- anchoraig_full %>%
  filter(task == "denominator_neglect_version_A" |
           task == "denominator_neglect_version_B") %>%
  mutate(task_version = factor(ifelse(task == "denominator_neglect_version_A", "A", "B"))) %>%
  select(!task) %>%
  filter(AIG_version == "anchor" & task_version == "B") %>%
  pull(session_id)

dn_long <- dn_full %>%
  arrange(session_id, trial_id) %>%
  filter(session_id %in% anchor_ids & task_version == "B") %>%
  mutate(proportion_difference = abs(left_lottery_gold_prop - right_lottery_gold_prop),
         trial_id = paste0("item_", trial_id)) |> 
  select(-c(session_restart_id, time_elapsed, custom_timer_ended_trial,
            trial_index, trial, response, trial_type))

dn_items <- dn_long %>%
  select(trial_id, choice_type, proportion_difference) %>%
  unique() %>%
  separate_wider_delim(trial_id, delim = "_", names = c("trial", "item")) %>%
  mutate(cond = paste0(choice_type, " ", round(proportion_difference * 100, 0), "%")) %>%
  select(item, cond)

dn <- dn_long %>%
  select(session_id, trial_id, correct) %>% # no choice_type or proportion_difference
  pivot_wider(id_cols = session_id,
              names_from = trial_id, values_from = correct) %>% 
  select(-session_id) %>%
  drop_na()

# mirt ----

dn_mirt_2pm <- mirt(data = dn, model = 1, itemtype = "2PL",
                 verbose = F, SE = TRUE)

itemfit(dn_mirt_2pm)
coef(dn_mirt_2pm, IRTpars = TRUE, printSE = TRUE)

plot(dn_mirt_2pm, type = "trace")

plot(dn_mirt_2pm, type = "infotrace")

# stan ----

# dn_subset <- dn %>%
#   slice_head(n = 500)

dn_list <- list(J = nrow(dn),
                K = ncol(dn),
                y = dn)

dn_m <- stan(here::here("Models", "2pl-code.stan"), 
             data = dn_list,
             chains = 4,
             iter = 2500,
             seed = 50401)
saveRDS(dn_m, here::here("Models", "denominator-neglect_2pl.rds"))

dn_m <- readRDS(here::here("Models", "denominator-neglect_2pl.rds"))
summary(dn_m)

rstan::extract(dn_m, c("a", "b")) %>%
  as.data.frame() %>%
  pivot_longer(everything(),
               names_to = "item", values_to = "est") %>%
  separate_wider_delim(item, ".", names = c("param", "item")) %>%
  ggplot(aes(x = est, y = fct_inorder(item))) +
  geom_violin(fill = "lightblue3", alpha = .8, color = NA) +
  stat_summary(fun = mean, color = "steelblue4") +
  facet_wrap(~ param) +
  labs(y = "item") +
  coord_cartesian(xlim = c(-4, 4)) +
  theme_classic() +
  theme(panel.grid.major.x = element_line("gray85", linewidth = .3))


ps <- rstan::extract(dn_m, c("a", "b")) %>%
  as.data.frame() %>%
  mutate(rep = row_number()) %>%
  filter(rep %in% 1:50) %>%
  pivot_longer(-rep,
               names_to = "item", values_to = "est") %>%
  separate_wider_delim(item, ".", names = c("param", "item")) %>%
  pivot_wider(id_cols = c(item, rep),
              names_from = param, values_from = est) %>%
  expand_grid(th = seq(-6, 6, length.out = 100)) %>%
  mutate(p_1 = exp(a * (th - b)) / (1 + exp(a * (th - b))),
         p_2 = 1 - p_1,
         info = a^2 * (p_1 * p_2),
         choice_type = ifelse(as.numeric(item) <= 24, "conflict", "harmony"))


irc <- ps %>%
  ggplot(aes(x = th, y = p_1, group = rep)) +
  geom_line(alpha = .3, color = "slategray3") +
  geom_line(data = . %>% summarize(.by = c(th, item),
                                   p_1 = mean(p_1)),
            aes(x = th, y = p_1),
            inherit.aes = F, linewidth = .8) +
  geom_text(data = dn_items,
            aes(label = cond),
            x = 5, y = .05, color = "gray50",
            hjust = 1,
            inherit.aes = F) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  scale_x_continuous(breaks = c(-4, 0, 4)) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 1)) +
  facet_wrap(~ fct_inorder(item), nrow = 6) +
  labs(y = "P(Y = 1 | ϴ)", x = "ϴ") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        aspect.ratio = 1)

saveRDS(irc, here::here("Figures", "irc_denominator-neglect.rds"))  

ic <- ps %>%
  ggplot(aes(x = th, y = info, group = rep)) +
  geom_line(alpha = .3, color = "slategray3") +
  geom_line(data = . %>% summarize(.by = c(th, item),
                                   info = mean(info)),
            aes(x = th, y = info),
            inherit.aes = F, linewidth = .8) +
  # geom_text(data = dn_items,
  #           aes(label = cond),
  #           x = 0, y = 2, color = "gray50",
  #           vjust = 1,
  #           inherit.aes = F) +
  scale_y_continuous(breaks = c(0, 1, 2)) +
  scale_x_continuous(breaks = c(-4, 0, 4)) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 2)) +
  facet_wrap(~ fct_inorder(item), nrow = 6) +
  labs(y = "I(ϴ)", x = "ϴ") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        #strip.text.x = element_blank(),
        aspect.ratio = 1)
saveRDS(ic, here::here("Figures", "ic_denominator-neglect.rds")) 

ps |>
  summarize(.by = c(rep, th),
            info = sum(info)) |>
  ggplot(aes(x = th, y = info, group = rep)) +
  geom_line(alpha = .3, color = "slategray3") +
  geom_line(data = . %>% summarize(.by = th,
                                   info = mean(info)),
            aes(x = th, y = info),
            inherit.aes = F, linewidth = .8) +
  scale_x_continuous(breaks = c(-4, 0, 4)) +
  coord_cartesian(xlim = c(-5, 5)) +
  labs(y = "I(ϴ)", x = "ϴ") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())



{ps_temp <- ps
map_df(list(count = 1:10), \(count)
       {top_pair <- expand_grid(conf_item = ps_temp |>
                                   filter(choice_type == "conflict") |>
                                   pull(item) |> unique(),
                                 harm_item = ps_temp |>
                                   filter(choice_type == "harmony") |>
                                   pull(item) |> unique()) |>
           pmap_df(\(conf_item, harm_item) {
             ps_temp |>
               filter(item == conf_item | item == harm_item) |>
               summarize(conf_item = conf_item,
                         harm_item = harm_item,
                         info = sum(info))
           }) |>
           arrange(desc(info)) |>
           head(1)
         
         ps_temp <- ps_temp |>
             filter(!(item %in% top_pair))
         
         top_pair
       })} 

ps_temp <- ps
top_pairs <- data.frame(conf_item = as.character(),
                        harm_item = as.character(),
                        info = as.numeric())
for (i in 1:10) {
    top_pair <- expand_grid(conf_item = ps_temp |>
                              filter(choice_type == "conflict") |>
                              pull(item) |> unique(),
                            harm_item = ps_temp |>
                              filter(choice_type == "harmony") |>
                              pull(item) |> unique()) |>
      pmap(\(conf_item, harm_item) {
        ps_temp |>
          filter(item == conf_item | item == harm_item) |>
          summarize(conf_item = conf_item,
                    harm_item = harm_item,
                    info = sum(info))
      }) |>
      list_rbind() |>
      arrange(desc(info)) |>
      head(1) 
    
    ps_temp <<- ps_temp |>
      filter(!(item %in% pull(top_pair, conf_item) |
                 item %in% pull(top_pair, harm_item)))
    
    top_pairs <- rbind(top_pairs,
                       top_pair)
  }


{data_temp <- data
  map_df(list(count = 1:10), \(count) {
    top_pair <- expand_grid(red_item = data_temp |>
                              filter(color == "red") |>
                              pull(item) |> unique(),
                            blue_item = data_temp |>
                              filter(choice_type == "blue") |>
                              pull(item) |> unique()) |>
      pmap_df(\(red_item, blue_item) {
        ps_temp |>
          filter(item == red_item | item == blue_item) |>
          summarize(red_item = red_item,
                    blue_item = blue_item,
                    score = sum(score))
      }) |>
      arrange(desc(info)) |>
      head(1)
    
    data_temp <<- data |>
      filter(!(item %in% pull(top_pair, )))
    
    data.frame(count, top_pair)
  })}

items_ranked <- ps |>
  summarize(.by = item,
            info = sum(info)) |>
  full_join(dn_items,
            by = "item") |>
  separate_wider_delim(cond, " ", names = c("choice_type", "proportion_difference")) |>
  arrange(-info) |>
  filter(choice_type == "conflict") |>
  mutate(rank = row_number()) |>
  select(-c(info, proportion_difference)) |>
  pivot_wider(names_from = choice_type, values_from = item) |>
  left_join(ps |>
              summarize(.by = item,
                        info = sum(info)) |>
              full_join(dn_items,
                        by = "item") |>
              separate_wider_delim(cond, " ", names = c("choice_type", "proportion_difference")) |>
              arrange(-info) |>
              filter(choice_type == "harmony") |>
              mutate(rank = row_number()) |>
              select(-c(info, proportion_difference)) |>
              pivot_wider(names_from = choice_type, values_from = item),
            by = "rank")

dndat <- dn_long |>
  left_join(rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/metadata_tables/session.csv"),
            by = "session_id") |>
  left_join(rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_forecasting/processed_data/scores_quantile.csv") |>
            select(subject_id, sscore_standardized) |>
            summarize(.by = subject_id,
                      sscore = mean(sscore_standardized)),
            by = "subject_id") |>
  select(subject_id, trial_id, correct, sscore) |>
  mutate(item = str_split_i(trial_id, "_", 2),
         .keep = "unused")

dn_func <- function(n_items) {
  pmap_df(expand_grid(n_items = n_items), 
          \(n_items, rep) {
            
            included_items <- items_ranked |> head(n_items) |>
              select(conflict, harmony) |> c() |> unlist() |> unname()
            
            dndat |>
              filter(item %in% included_items) |>
              pivot_wider(names_from = item, values_from = correct) |> # need to figure out why 2000
              mutate(meanscore = rowMeans(across(!c(subject_id, sscore)), na.rm = T)) |> # alert alert na.rm = T
              select(sscore, meanscore) |>
              drop_na() |>
              summarize(n_items = n_items,
                        cor = cor(sscore, meanscore) |> abs())
          })
}

dn_func_notinfo <- function(n_items, rep = 1) {
  pmap_df(expand_grid(n_items = n_items, rep = rep), 
          \(n_items, rep) {
            
            included_items <- c(items_ranked |> 
                                  filter(rank %in% sample(unique(rank), n_items)) |>
                                  select(conflict),
                                items_ranked |> 
                                  filter(rank %in% sample(unique(rank), n_items)) |>
                                  select(harmony)) |> unlist() |> unname()
            
            dndat |>
              filter(item %in% included_items) |>
              pivot_wider(names_from = item, values_from = correct) |> 
              mutate(meanscore = rowMeans(across(!c(subject_id, sscore)), na.rm = T)) |> # alert alert na.rm = T
              select(sscore, meanscore) |>
              drop_na() |>
              summarize(n_items = n_items,
                        rep = rep,
                        cor = cor(sscore, meanscore) |> abs())
          })
}




dn_more <- dn_func_notinfo(n_items = 1:24, rep = 1:1000)

dn_all <- dn_func(n_items = 1:24) 

saveRDS(dn_more, here::here("Data", "Denominator Neglect", "dn_noinfo.rds"))
saveRDS(dn_all, here::here("Data", "Denominator Neglect", "dn_all.rds"))


dn_all |>
  summarize(.by = n_items,
            across(cor, list(mean = mean))) |>
  ggplot(aes(x = n_items, y = cor_mean)) +
  geom_ribbon(data = dn_more |> 
                summarize(.by = n_items,
                          across(cor, list(mean = mean,
                                           lower = ~ quantile(.x, 0.05, names = FALSE),
                                           upper = ~ quantile(.x, 0.95, names = FALSE)))),
              aes(ymin = cor_lower, ymax = cor_upper),
              alpha = .5, fill = "thistle3") +
  geom_line(data = dn_more |> 
              summarize(.by = n_items,
                        across(cor, list(mean = mean,
                                         lower = ~ quantile(.x, 0.05, names = FALSE),
                                         upper = ~ quantile(.x, 0.95, names = FALSE)))),
            linewidth = 1, color = "thistle3") +
  geom_line(linewidth = 1.5, color = "slategray") +
  annotate("text", label = str_wrap("Using summed information gives an advange over randomly selecting item pairs at small item counts",
                                    width = 28), 
           x = 2.4, y = .63, hjust = 0, vjust = 0,
           color = "thistle4", lineheight = .8) +
  annotate("curve", arrow = arrow(length = unit(2.5, "mm")),
           x = 2.2, xend = 1.9, y = .61, yend = .5,
           color = "thistle4") +
  coord_cartesian(ylim = c(.1, .8)) +
  labs(y = "Correlation with S-Scores", x = "Number of C/H Item Pairs") +
  guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both")) +
  theme_classic() 
  
dn_full |>
  left_join(rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/metadata_tables/session.csv"),
            by = "session_id") |> View()

