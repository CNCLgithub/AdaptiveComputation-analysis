
library(estimatr)
library(ggplot2)
library(readr)
library(scales)
library(raster)
library(minpack.lm)
library(tidyverse)

th <- theme_classic()
theme_set(th)

attention_full <- read_csv("data/isr_inertia_target_designation_attention.csv") %>%
  dplyr::rename(frame = t,
         scene = trial)


full_data <- attention_full %>%
  pivot_longer(-c(frame, scene), names_to = "tracker", 
               values_to = "att")  %>%
  separate(tracker, c(NA, "tracker"), sep = '_') %>%
  mutate(tracker = as.numeric(tracker))

probe_limit = 48
window_size = 24
tmax = max(full_data$frame)

tps <- full_data %>%
  filter(between(frame, probe_limit, tmax-probe_limit)) %>%
  group_by(scene, tracker) %>%
  mutate(t_max = which.max(att) + min(frame),
         delta_t = frame - t_max) %>%
  group_by(scene) %>%
  filter(between(delta_t, -window_size, window_size)) %>%
  mutate(n_t = dplyr::n()) %>%
  ungroup() %>%
  # filter(n_t >= 4*2*window_size) %>%
  arrange(scene, tracker)

n_scenes = 12

n_unique_scenes = length(unique(tps$scene))
top_trials <- tps %>%
  group_by(scene,tracker) %>%
  mutate(max_att = max(att),
         sd_att = sd(att)) %>%
  group_by(scene) %>%
  mutate(scene_max = max(max_att),
         max_sd_att = max(sd_att),
         tracker_rank = dense_rank(max_att)) %>%
  ungroup() %>%
  mutate(s_rank = dense_rank(max_sd_att)) %>%
  filter(s_rank > n_unique_scenes - n_scenes)

# top_trials <- top_trackers %>%
#   left_join(attention_full)
#  
top_trials %>%
  ggplot(aes(x = delta_t, y = att, color = factor(tracker_rank))) +
  geom_line() +
  facet_wrap(vars(scene))

fits <- top_trials %>%
  filter(tracker_rank == max(tracker_rank)) %>%
  nest_by(scene) %>%
  mutate(mod = list(nlsLM( att ~ k*exp(-1/2*(delta_t -mu)^2/sigma^2), start=c(mu=0,sigma=3,k=5),
                      data = data)))

max_sigma = probe_limit / 3
coefs <- fits %>% 
  summarise(broom::tidy(mod)) %>%
  dplyr::select(scene, term, estimate) %>%
  filter(term == "sigma") %>%
  select(-term) %>%
  # rename(sigma = estimate)
  transmute(sigma = clamp(estimate, lower = 3.0, upper = max_sigma))
 
att_map <- top_trials %>%
  right_join(coefs) %>%
  filter(delta_t == 0 & tracker_rank == max(tracker_rank)) %>%
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
  select(scene, frame) %>%
  left_join(full_data) %>%
  group_by(scene) %>%
  mutate(epoch = dense_rank(frame)) %>%
  ungroup() %>%
  arrange(scene, tracker)

att_map %>%
  ggplot(aes(x = epoch, y = att, color = factor(tracker))) +
  geom_line() +
  facet_wrap(vars(scene))


att_map %>%
  ggplot(aes(frame)) +
  geom_histogram() +
  facet_wrap(vars(epoch))

full_data %>%
  filter(scene %in% att_map$scene) %>%
  mutate(zatt = scale(att)) %>%
  ggplot(aes(frame, tracker)) +
  geom_tile(aes(fill = zatt)) +
  scale_fill_gradient2(low = muted("blue"),
                       high = muted("red")) +
facet_wrap(vars(scene))
# ggsave("output/attention_trial_tps.png")

write.csv(att_map, row.names = FALSE,
          file = "output/isr_inertia_probe_map.csv")

