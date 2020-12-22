
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

attention_full <- read_csv("data/isr_inertia_480_attention.csv") %>%
  rename(scene = trial) %>%
  mutate(frame = t) %>%
  select(-t)

full_data <- attention_full %>%
  pivot_longer(-c(frame, scene), names_to = "tracker", 
               values_to = "att")  %>%
  separate(tracker, c(NA, "tracker"), sep = '_') %>%
  mutate(tracker = as.numeric(tracker),
         zatt = scale(att))

probe_limit = 48
tmax = max(full_data$frame)
# n_trackers = length(unique(full_data$tracker))


sample_timepoints <- function(probe_limit, tmax) {
  while (TRUE) {
    possible_frames = seq(probe_limit, tmax-probe_limit)
    samples = sort(sample(possible_frames, 4))
    if (all(sort(dist(samples)) > probe_limit)) {
      return(samples)
    }
  }
}

tps <- full_data %>%
  nest_by(scene) %>%
  mutate(frame = list(sample_timepoints(probe_limit, tmax)),
         tracker = list(sample(4, replace = TRUE))) %>%
  select(-data) %>%
  unnest(cols = c(frame, tracker)) %>%
  left_join(full_data) %>%
  arrange(scene, frame)

# # take the highs for each scene with at least some buffer between
# tps <- full_data %>%
#   filter(between(frame, probe_limit, tmax-probe_limit)) %>%
#   nest_by(scene) %>%
#   mutate(frame = list(scene_peaks(data$att))) %>%
#   select(-data) %>%
#   unnest(cols = c(frame)) %>%
#   mutate(frame = round(frame / n_trackers) + probe_limit) %>%
#   left_join(full_data) %>%
#   arrange(scene, frame)


scene_bd <- tps %>%
  group_by(scene, frame) %>%
  summarise(att = max(att))


scene_bd %>%
ggplot(aes(frame)) +
geom_density()

full_data %>%
filter(scene %in% att_map$scene) %>%
mutate(zatt = scale(att)) %>%
ggplot(aes(frame, tracker)) +
geom_tile(aes(fill = zatt)) +
scale_fill_gradient2(low = muted("blue"),
                     high = muted("red")) +
facet_wrap(vars(scene))
# ggsave("output/attention_trial_tps.png")

att_map <- tps %>%
  group_by(scene, frame) %>%
  mutate(att_rank = dense_rank(att)) %>%
  ungroup() %>%
  filter(att_rank== 1 | att_rank == 4) %>%
  arrange(scene, frame, att)

att_map %>%
  ggplot(aes(att)) +
  xlim(0, 13) +
  geom_histogram(binwidth = 0.3)


write.csv(att_map, row.names = FALSE,
        file = "output/isr_inertia_probe_map_random.csv")

