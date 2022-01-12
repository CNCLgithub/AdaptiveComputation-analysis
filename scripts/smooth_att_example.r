
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
  mutate(frame = t + 2) %>%
  select(-t)

# convert the columns for each tracker att into a "long" format
pivoted <- attention_full %>%
  pivot_longer(-c(frame, scene), names_to = "tracker", 
               values_to = "att")  %>%
  separate(tracker, c(NA, "tracker"), sep = '_') %>%
  mutate(tracker = as.numeric(tracker),
         zatt = scale(att),
         log_att = log(att))

# sum up the total amount of attention
total_att <- pivoted %>%
  group_by(scene, frame) %>%
  summarise(total_att = sum(att)) %>%
  ungroup()

# add lag and lead att values
# also add cumulative att
full_data <- pivoted %>%
  left_join(total_att) %>%
  mutate(prop_att = att / total_att) %>%
  group_by(scene, tracker) %>%
  mutate(across(contains("att"), list(lag10 = ~lag(.x, 10))),
         across(!contains("lag")  & contains("att"), list(lead10 = ~lead(.x, 10))),
         cum_att = cumsum(att)) %>%
  ungroup()


smoothed_tot_att <- total_att %>%
  nest_by(scene) %>%
  mutate(smoothed = list(with(data, 
                         ksmooth(frame, total_att, kernel = "box", bandwidth = 7)))) %>%
  mutate(smooth_x = list(smoothed$x),
         smoothed = list(smoothed$y)) %>%
  unnest(cols = c(data, smooth_x, smoothed))
  