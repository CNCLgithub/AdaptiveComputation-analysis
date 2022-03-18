library(tidyverse)
library(ggplot2)
library(readr)
library(scales)
# library(raster)
library(minpack.lm)
library(pracma)

th <- theme_classic()
theme_set(th)

# cycles per frame
# scene, frame, tracker, cycles, chain
model_att <- read_csv("data/exp1/exp1_difficulty_target_designation_att_fig4.csv") 

model_smoothing = 8

# apply same time-smoothing procedure as analysis
exp_summary <- model_att %>%
  group_by(scene, frame, tracker) %>%
  summarise(across(-chain, list(mu = mean, sd = sd))) %>%
  ungroup() %>%
  mutate(cycles_mu = cycles_mu + 0.1, # padding to allow for log
         zatt = scale(attention_mu),
         log_att = log(cycles_mu)) %>%
  group_by(scene, tracker) %>%
  # add smoothing
  nest_by() %>%
  mutate(att_xy = list(with(data,
                            ksmooth(frame, cycles_mu, kernel = "normal", 
                                    bandwidth = model_smoothing)))) %>%
  mutate(arr_smoothed = list(att_xy$y)) %>%
  unnest(cols = c(data, arr_smoothed)) %>%
  dplyr::select(-c(att_xy)) %>%
  ungroup()


exp_summary %>%
  filter(scene %in% c(1, 65)) %>%
  group_by(scene, frame) %>%
  summarise(total_att = sum(arr_smoothed)) %>%
  ungroup() %>%
  pivot_wider(names_from = scene,
              names_glue = "scene_{scene}_{.value}",
              values_from = total_att) %>%
  ggplot(aes(x = frame)) + 
  geom_ribbon(aes(ymin = scene_1_total_att,
                  ymax = scene_65_total_att),
              fill = "#fbe8e8",
              alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = scene_1_total_att),
              fill = "#e8e8fb",
              alpha = 0.5) + 
  geom_line(aes(y = scene_1_total_att), 
            size = 1.75,
            colour = "#2222D7") + 
  geom_line(aes(y = scene_65_total_att), 
            size = 1.75,
            colour = "#D72222") + 
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        aspect.ratio = 0.4,
        # axis.line = element_blank()
  ) + 
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.01)))

