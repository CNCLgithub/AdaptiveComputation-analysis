
library(estimatr)
library(ggplot2)
library(readr)
library(scales)
#library(devtools)
#install_github("onofriandreapg/aomisc")
# library(nls)
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
               values_to = "att")

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

n_scenes = 24
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
  ggplot(aes(x = delta_t, y = att, color = tracker)) +
  geom_line() +
  facet_wrap(vars(scene))

fits <- top_trials %>%
  nest_by(scene, tracker) %>%
  mutate(mod = list(nlsLM( att ~ k*exp(-1/2*(delta_t -mu)^2/sigma^2), start=c(mu=0,sigma=3,k=5),
                      data = data)),
         coefs = list(tidy(mod)))

coefs <- fits %>% 
  summarise(broom::tidy(mod)) %>%
  select(scene, tracker, term, estimate) %>%
  filter(term == "sigma") %>%
  select(-term) %>%
  transmute(sigma = clamp(estimate, 0, 8))
 
top_trials <- top_trials %>% left_join(coefs)
# %>%
#   mutate(Slope = summary(mod)$coeff[2]) %>%
#   select(-mod)

# 
# max_att <- tps %>%
#   slice(which.max(att)) %>%
#   separate(tracker, c(NA, "tracker"), "_") %>%
#   mutate(tracker = as.numeric(tracker))
# 
# min_att <- tps %>%
#   slice(which.min(att)) %>%
#   separate(tracker, c(NA, "tracker"), "_") %>%
#   mutate(tracker = as.numeric(tracker))
# 
# time_points <- full_join(max_att, min_att) %>%
#   arrange(scene)
# 
# scene_shuffle <- time_points %>%
#   group_by(scene) %>%
#   summarise() %>%
#   sample_n(20) %>%
#   mutate(unique_id=1:NROW(.))
# 
# shuffled_tps <- time_points %>%
#   group_by(scene) %>%
#   right_join(scene_shuffle) %>%
#   arrange(unique_id) %>%
#   ungroup()
#   
# write.csv(shuffled_tps, row.names = FALSE, 
#           file = "output/attention_trial_tps.csv")
# 
