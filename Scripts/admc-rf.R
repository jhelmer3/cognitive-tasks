# setup ----

options(scipen=999)

library(tidyverse)
library(mirt)
library(rstan)

here::here()

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = T)

# reading ----

admc <- rio::import("https://raw.githubusercontent.com/forecastingresearchfpt/refs/heads/main/data_cognitive_tasks/task_datasets/data_admc_raw.csv")

session <- rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/metadata_tables/session.csv")

# cleaning ----

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
              values_from = response) %>%
  mutate(across(starts_with("a") | starts_with("rc"), as.numeric),
         a = ifelse(a1 == a2, 1, 0),
         rc = ifelse(rc1 == rc2, 1, 0),
         .keep = "unused") %>%
  select(subject_id, rf_itemid, a, rc) |>
  pivot_longer(!c(subject_id, rf_itemid),
               values_to = "response", names_to = "itemtype") |>
  mutate(item = paste0(rf_itemid, itemtype),
         .keep = "unused") |>
  pivot_wider(names_from = item, values_from = response) |>
  drop_na() |>
  select(-subject_id)

a <- rf |>
  select(ends_with("a"))

rc <- rf |>
  select(ends_with("rc"))

# mirt ----

a_mirt_2pm <- mirt(data = a, model = 1, itemtype = "2PL",
                    verbose = F, SE = TRUE)

itemfit(a_mirt_2pm)
coef(rf_mirt_2pm, IRTpars = TRUE, printSE = TRUE)

plot(a_mirt_2pm, type = "trace")

plot(a_mirt_2pm, type = "infotrace")

rc_mirt_2pm <- mirt(data = rc, model = 1, itemtype = "2PL",
                   verbose = F, SE = TRUE)

itemfit(rc_mirt_2pm)
coef(rc_mirt_2pm, IRTpars = TRUE, printSE = TRUE)

plot(rc_mirt_2pm, type = "trace")

plot(rc_mirt_2pm, type = "infotrace")

# stan ----

a_list <- list(J = nrow(a),
               K = ncol(a),
               y = a)

a_m <- stan(here::here("Models", "irt.stan"), 
            data = a_list,
            chains = 4,
            iter = 2500,
            seed = 50401)
saveRDS(a_m, here::here("Models", "a_m.rds"))
a_m <- readRDS(here::here("Models", "a_m.rds"))

rstan::extract(a_m, c("a", "b")) %>%
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


ps <- rstan::extract(a_m, c("a", "b")) %>%
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
         info = a^2 * (p_1 * p_2))

ps %>%
  ggplot(aes(x = th, y = p_1, group = rep)) +
  geom_line(alpha = .3, color = "slategray3") +
  geom_line(data = . %>% summarize(.by = c(th, item),
                                   p_1 = mean(p_1)),
            aes(x = th, y = p_1),
            inherit.aes = F, linewidth = .8) +
  # geom_text(data = dn_items,
  #           aes(label = cond),
  #           x = 5, y = .05, color = "gray50",
  #           hjust = 1,
  #           inherit.aes = F) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  scale_x_continuous(breaks = c(-4, 0, 4)) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 1)) +
  facet_wrap(~ fct_inorder(item), nrow = 2) +
  labs(y = "P(Y = 1 | ϴ)", x = "ϴ") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

ps %>%
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
  coord_cartesian(xlim = c(-5, 5)) +
  facet_wrap(~ fct_inorder(item), nrow = 6) +
  labs(y = "I(ϴ)", x = "ϴ") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())



data.frame(i = seq(-1, 6, length = 100)) |>
  mutate(dens = dnorm(i, .25, .25),
         logdens = dnorm(log(i))) |>
  ggplot(aes(x = i, y = dens)) +
  geom_line() +
  geom_line(aes(y = logdens))









