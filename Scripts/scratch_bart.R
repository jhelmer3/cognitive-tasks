
v5_dat <- readRDS(here::here("Data", "v5_dat.rds"))
startup_costs <- readRDS(here::here("Data", "startup_costs.rds"))
source(here::here("Scripts", "get_startup.R"))


v5_dat_wide <- v5_dat |>
  # condensing denominator neglect scores down to one score per item pair
  mutate(.by = c(subject_id, item),
         score = ifelse(task == "dn.c" | task == "dn.s",
                        mean(score),
                        score)) |>
  unique() |>
  select(-c(time, task)) |>
  pivot_wider(names_from = "item", values_from = "score",
              id_cols = c(subject_id, sscore)) |>
  # alert alert !!
  drop_na() |>
  mutate(across(-c(subject_id, sscore), ~ (.x - mean(.x)) / sd(.x)))

v5_dat_wide

frm <- v5_dat_wide |>
  select(-c(sscore, subject_id)) |>
  names() |>
  paste(collapse = " + ") |>
  paste("sscore ~ ", ... = _) |>
  as.formula()

library(dbarts)
library(gt)

fit1 <- bart2(frm, data = v5_dat_wide, keepTrees = T)
fitted_test <- predict(fit1, newdata = v5_dat_wide, type = "ppd")
dim(fitted_test)

fit1 |> str()

cv <- xbart(frm, data = v5_dat_wide,
            k = seq(0.1, 5, 0.1),
            seed = 2)

cv |> 
  as_tibble() |>
  mutate(fold = row_number()) |>
  pivot_longer(-fold, names_to = "k", values_to = "loss") |>
  summarize(.by = k, mean_loss = min(loss)) |>
  gt() |>
  fmt_number() |>
  tab_style(cell_fill(color = "lightgreen"),
            cells_body(rows = mean_loss == min(mean_loss),
                       columns = mean_loss))

fit2 <- bart2(frm, data = v5_dat_wide, keepTrees = T, k = 0.3)
fit2 |> str()

varcounts <- fit2$varcount |>
  as_tibble() |>
  pivot_longer(everything(), 
               names_to = c("rep", ".value"), 
               names_pattern = "(\\d+)\\.(.+)") |>
  pivot_longer(!rep,
               names_to = "item", values_to = "count") 

varcounts |>
  summarize(.by = item,
            rank = mean(count)) |>
  arrange(desc(rank)) -> ranks

varcounts |>
  filter(item %in% sample(item, 25)) |>
  ggplot(aes(x = count)) +
  geom_bar() +
  facet_wrap(~ item, ncol = 5)


ranks_w_timing <- ranks  |>
  inner_join(v5_dat |>
               select(task, item, time) |>
               unique(),
             by = "item") |>
  mutate(.by = task,
         time_w_startup = time + ifelse(item == first(item), get_startup(first(task)), 0)) |>
  mutate(cum_time = cumsum(time_w_startup)) 

r2_w_time <- map(1:99,
                 \(i) {
                   included_items <- ranks_w_timing |>
                     select(item, task, rank, cum_time) |>
                     head(i)
                   
                   inner_join(v5_dat|>
                                select(subject_id, sscore, item, score),
                              included_items,
                              by = "item") |>
                     summarize(.by = c(subject_id, task),
                               sscore = first(sscore),
                               score = mean(score)) |>
                     mutate(formula = paste("sscore ~", task |> unique() |> paste(collapse = "+"))) |>
                     pivot_wider(names_from = task, values_from = score) |>
                     drop_na() |>
                     summarize(rep = i,
                               item = included_items |> filter(cum_time == max(cum_time)) |> pull(item),
                               test_r2 = summary(lm(as.formula(first(formula)),
                                                    data = pick(everything())))$r.squared,
                               cum_time = included_items |> filter(cum_time == max(cum_time)) |> pull(cum_time))}) |>
  list_rbind()


r2_w_time |>
  mutate(task = str_split_i(item, "_", 1)) |>
  ggplot(aes(x = cum_time, y = test_r2)) +
  geom_line() +
  geom_point(aes(color = task)) +
  coord_cartesian(ylim = c(.1, .5)) +
  labs(title = "BART", y = "R<sup>2</sup>", x = "Time") +
  theme(axis.title.y = ggtext::element_markdown())

v5_dat_long <- v5_dat |>
  mutate(.by = subject_id,
         subject_id = cur_group_id()) |>
  mutate(.by = item,
         item = cur_group_id())

fit3 <- bart2(sscore ~ score + (1 | subject_id) + (1 | item), data = v5_dat_long)



varcounts <- fit3$varcount |>
  as_tibble() |>
  pivot_longer(everything(), 
               names_to = c("rep", ".value"), 
               names_pattern = "(\\d+)\\.(.+)") |>
  pivot_longer(!rep,
               names_to = "item", values_to = "count") 

varcounts |>
  summarize(.by = item,
            rank = mean(count)) |>
  arrange(desc(rank)) -> ranks

varcounts |>
  filter(item %in% sample(item, 25)) |>
  ggplot(aes(x = count)) +
  geom_bar() +
  facet_wrap(~ item, ncol = 5)


ranks_w_timing <- ranks  |>
  inner_join(v5_dat |>
               select(task, item, time) |>
               unique(),
             by = "item") |>
  mutate(.by = task,
         time_w_startup = time + ifelse(item == first(item), get_startup(first(task)), 0)) |>
  mutate(cum_time = cumsum(time_w_startup)) 

r2_w_time <- map(1:99,
                 \(i) {
                   included_items <- ranks_w_timing |>
                     select(item, task, rank, cum_time) |>
                     head(i)
                   
                   inner_join(v5_dat|>
                                select(subject_id, sscore, item, score),
                              included_items,
                              by = "item") |>
                     summarize(.by = c(subject_id, task),
                               sscore = first(sscore),
                               score = mean(score)) |>
                     mutate(formula = paste("sscore ~", task |> unique() |> paste(collapse = "+"))) |>
                     pivot_wider(names_from = task, values_from = score) |>
                     drop_na() |>
                     summarize(rep = i,
                               item = included_items |> filter(cum_time == max(cum_time)) |> pull(item),
                               test_r2 = summary(lm(as.formula(first(formula)),
                                                    data = pick(everything())))$r.squared,
                               cum_time = included_items |> filter(cum_time == max(cum_time)) |> pull(cum_time))}) |>
  list_rbind()


r2_w_time |>
  mutate(task = str_split_i(item, "_", 1)) |>
  ggplot(aes(x = cum_time, y = test_r2)) +
  geom_line() +
  geom_point(aes(color = task)) +
  coord_cartesian(ylim = c(.1, .5)) +
  labs(title = "BART", y = "R<sup>2</sup>", x = "Time") +
  theme(axis.title.y = ggtext::element_markdown())











