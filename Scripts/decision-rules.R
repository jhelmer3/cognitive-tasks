
# setup ----

options(scipen=999)

library(tidyverse)
library(mirt)
library(rstan)

here::here()

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = T)

# reading ----

admc <- rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/task_datasets/data_admc_raw.csv")

session <- rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/metadata_tables/session.csv")

# cleaning ----

dr <- admc |>
  left_join(session,
            by = "session_id") |>
  filter(trial_name == "admc_dr_trial") |>
  mutate(.by = admc_id,
         admc_response = as.numeric(admc_response),
         correct = case_when(admc_id %in% c("dr9", "dr10") ~ ifelse(admc_response == 3, 1, 0),
                             admc_id %in% c("dr8") ~ ifelse(admc_response == 2, 1, 0),
                             .default = ifelse(admc_response == 1, 1, 0)))

dr |>
  summarize(.by = admc_id,
            prop_correct = (mean(correct) * 100) |> round(2))

dr_wide <- dr |>
  select(subject_id, admc_id, correct) |>
  pivot_wider(names_from = admc_id, values_from = correct) |>
  select(-subject_id)


# modeling ----

dr_m <- stan(here::here("Models", "2pl-code.stan"), 
             data = list(J = nrow(dr_wide),
                         K = ncol(dr_wide),
                         y = dr_wide),
             chains = 4,
             iter = 2500,
             seed = 50401)
saveRDS(dr_m, here::here("Models", "decision-rules_2pl.rds"))
dr_m <- readRDS(here::here("Models", "decision-rules_2pl.rds"))
summary(dr_m)

# post-model ----

ps <- rstan::extract(dr_m, c("a", "b")) %>%
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


irc <- ps %>%
  ggplot(aes(x = th, y = p_1, group = rep)) +
  geom_line(alpha = .3, color = "slategray3") +
  geom_line(data = . %>% summarize(.by = c(th, item),
                                   p_1 = mean(p_1)),
            aes(x = th, y = p_1),
            inherit.aes = F, linewidth = .8) +
  geom_text(aes(label = paste("DR", item)),
            x = Inf, y = -Inf, color = "gray50",
            hjust = 1, vjust = -.25,
            inherit.aes = F) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  scale_x_continuous(breaks = c(-4, 0, 4)) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 1)) +
  facet_wrap(~ fct_inorder(item), nrow = 2) +
  labs(y = "P(Y = 1 | ϴ)", x = "ϴ") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        aspect.ratio = 1)

saveRDS(irc, here::here("Figures", "Decision Rules", "irc.rds"))  

ic <- ps %>%
  ggplot(aes(x = th, y = info, group = rep)) +
  geom_line(alpha = .3, color = "slategray3") +
  geom_line(data = . %>% summarize(.by = c(th, item),
                                   info = mean(info)),
            aes(x = th, y = info),
            inherit.aes = F, linewidth = .8) +
  geom_text(aes(label = paste("DR", item)),
            y = Inf, x = 0, color = "gray50",
            vjust = 1,
            inherit.aes = F) +
  scale_y_continuous(breaks = c(0, 1, 2)) +
  scale_x_continuous(breaks = c(-4, 0, 4)) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 1.5)) +
  facet_wrap(~ fct_inorder(item), nrow = 2) +
  labs(y = "I(ϴ)", x = "ϴ") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        aspect.ratio = 1)

saveRDS(ic, here::here("Figures", "Decision Rules", "ic.rds")) 





