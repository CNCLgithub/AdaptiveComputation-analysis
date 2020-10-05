
library(estimatr)
library(ggplot2)
library(readr)
library(scales)
library(minpack.lm)
library(raster)
library(tidyverse)

th <- theme_classic()
theme_set(th)

attention_full <- read_csv("data/attention.csv") %>%
  dplyr::rename(frame = t,
         scene = trial)


full_data <- attention_full %>%
  pivot_longer(-c(frame, scene), names_to = "tracker", 
               values_to = "att")  %>%
  separate(tracker, c(NA, "tracker"), sep = '_') %>%
  mutate(tracker = as.numeric(tracker)) %>%
  filter(frame > 24)

# top_att %>%
#   ggplot(aes(frame, tracker)) + 
#   geom_tile(aes(fill = att)) + 
#   scale_fill_gradient2(low = muted("blue"), 
#                        high = muted("red")) + 
# facet_wrap(vars(scene))
# ggsave("output/attention_trial_tps.png")

tps <- full_data %>%
  group_by(scene, tracker) %>%
  mutate(t_max = which.max(att) + min(frame),
         delta_t = frame - t_max) %>%
  group_by(scene) %>%
  filter(between(delta_t, -10, 10)) %>%
  group_by(scene) %>%
  mutate(n_t = dplyr::n()) %>%
  filter(n_t == 4 * 21) %>%
  arrange(scene, tracker)

n_scenes = 10

n_unique_scenes = length(unique(tps$scene))
top_trials <- tps %>%
  group_by(scene,tracker) %>%
  mutate(max_att = max(att)) %>%
  group_by(scene) %>%
  mutate(sd_max_att = sd(max_att),
         tracker_rank = dense_rank(max_att)) %>%
  ungroup() %>%
  mutate(sd_rank = dense_rank(sd_max_att)) %>%
  filter(sd_rank > n_unique_scenes - n_scenes & tracker_rank > 1)

top_trials %>%
  ggplot(aes(x = delta_t, y = att, color = factor(tracker))) +
  geom_line() +
  facet_wrap(vars(scene))

fits <- top_trials %>%
  nest_by(scene, tracker) %>%
  mutate(mod = list(nlsLM( att ~ k*exp(-1/2*(delta_t -mu)^2/sigma^2), start=c(mu=0,sigma=3,k=5),
                      data = data)))
coefs <- fits %>% 
  summarise(broom::tidy(mod)) %>%
  dplyr::select(scene, tracker, term, estimate) %>%
  filter(term == "sigma") %>%
  select(-term) %>%
  transmute(sigma = clamp(sqrt(estimate), 2, 3.33))
 
att_map <- top_trials %>% 
  left_join(coefs) %>%
  filter(delta_t == 0) %>%
  mutate(t_1 = -3 * sigma + frame,
         t_2 = -1.5 * sigma + frame,
         t_3 = 0 * sigma + frame,
         t_4 = 1.5 * sigma + frame,
         t_5 = 3.0 * sigma + frame,
         ) %>%
  select(-t_max) %>%
  pivot_longer(cols = starts_with("t_"),
               names_to = "epoch",
               values_to = "t") %>%
  mutate(frame = round(t)) %>% 
  select(scene, tracker, epoch, frame) %>%
  left_join(full_data)

att_map %>%
  ggplot(aes(x = factor(epoch), y = att, color = factor(tracker))) +
  geom_point() +
  facet_wrap(vars(scene))


att_map %>%
  ggplot(aes(frame)) +
  geom_histogram() +
  facet_wrap(vars(epoch))


write.csv(att_map, row.names = FALSE,
      file = "output/exp0_probe_map.csv")

