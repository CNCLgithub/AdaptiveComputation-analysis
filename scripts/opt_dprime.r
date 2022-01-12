
library(estimatr)
library(ggplot2)
library(readr)
library(scales)
library(raster)
library(minpack.lm)
library(tidyverse)
library(pracma)

th <- theme_classic()
theme_set(th)

exp <- read_csv("data/isr_inertia_480_target_designation.csv") %>%
  group_by(scene, frame, tracker) %>%
  summarise(across(-c(pred_target, prob_target, chain, particle),
                   list(mu = mean, sd = sd)),
            across(c(pred_target), )) %>%
  ungroup() %>%
  mutate(zatt = scale(attention_mu))


# sum up the total amount of attention
total_att <- exp_summary %>%
  group_by(scene, frame) %>%
  summarise(total_att = sum(attention_mu)) %>%
  ungroup()

# add lag and lead att values
# also add cumulative att
full_data <- exp_summary %>%
  left_join(total_att) %>%
  mutate(prop_att = attention_mu / total_att) %>%
  group_by(scene, tracker) %>%
  mutate(across(contains("att"), list(lag10 = ~lag(.x, 10))),
         across(!contains("lag")  & contains("att"), list(lead10 = ~lead(.x, 10))),
         cum_att = cumsum(attention_mu),
         cum_zatt = cumsum(zatt)/frame) %>%
  ungroup()


probe_limit = 48
window_size = 60
max_peaks = 4
tmax = max(full_data$frame)
n_trackers = length(unique(full_data$tracker))

scene_peaks <- function(att) {
  matt <- findpeaks(att, npeaks = max_peaks, minpeakheight = 1,
                    minpeakdistance = window_size * n_trackers)
  return(matt[, 2])
}

# take the highs for each scene with at least some buffer between
tps <- full_data %>%
  filter(between(frame, probe_limit, tmax-probe_limit)) %>%
  nest_by(scene) %>%
  mutate(frame = list(scene_peaks(data$att))) %>%
  select(-data) %>%
  unnest(cols = c(frame)) %>%
  mutate(frame = round(frame / n_trackers) + probe_limit) %>%
  left_join(full_data) %>%
  arrange(scene, frame) %>%
  group_by(scene, frame) %>%
  mutate(att_rank = dense_rank(att)) %>%
  ungroup()

# full att distribution
tps %>%
  ggplot(aes(att)) +
  geom_histogram(aes(fill = factor(att_rank)))

tps %>%
  ggplot(aes(log_att)) +
  geom_histogram(aes(fill = factor(att_rank)))

# peak att
tps %>%
  group_by(scene, frame) %>%
  summarise(att = max(att)) %>%
  ggplot(aes(frame)) +
  geom_density()



att_map <- tps %>%
  group_by(scene, frame) %>%
  mutate(att_rank = dense_rank(att)) %>%
  ungroup() %>%
  filter(att_rank== 1 | att_rank == 4) %>%
  arrange(scene, frame, att)

att_map %>%
  ggplot(aes(att)) +
  geom_density()

full_data %>%
  filter(scene %in% c(34,36)) %>%
  ggplot(aes(frame, tracker)) +
  geom_tile(aes(fill = zatt)) +
  scale_fill_gradient2(low = muted("blue"),
                       high = muted("red")) +
  facet_grid(rows = vars(scene))
ggsave("output/attention_trial_tps.png")

full_data %>%
  filter(scene %in% c(34,36)) %>%
  ggplot(aes(frame, tracker)) +
  geom_tile(aes(fill = zatt_lag10 + zatt)) +
  scale_fill_gradient2(low = muted("blue"),
                       high = muted("red")) +
  facet_grid(rows = vars(scene))
# ggsave("output/attention_trial_tps.png")

# 
# write.csv(att_map, row.names = FALSE,
#           file = "output/isr_inertia_probe_map.csv")

