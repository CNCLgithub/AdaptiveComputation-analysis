
library(ggplot2)
library(readr)
library(scales)
# library(raster)
# library(minpack.lm)
library(tidyverse)
# library(pracma)

th <- theme_classic()
theme_set(th)

# cycles per frame
# scene, frame, tracker, cycles, chain
# model_att <- read_csv("project/data/exp2/exp2_probes_target_designation_att.csv") 
model_att <- read_csv("project/data/exp2/exp2_probes_ac_td_att.csv") %>%
  group_by(scene, frame, tracker) %>%
  summarise(across(-chain, list(mu = mean, sd = sd))) %>%
  ungroup()


model_smoothing = 12

smoothed <- model_att %>%
  # mutate(cycles_mu = cycles_mu + 5) %>% # add base compute
  group_by(scene, tracker) %>%
  # add smoothing
  nest_by() %>%
  mutate(att_xy = list(with(data,
                            ksmooth(frame, importance_mu, kernel = "normal", 
                                    bandwidth = model_smoothing))),
         arr_xy = list(with(data,
                            ksmooth(frame, cycles_mu, kernel = "normal", 
                                    bandwidth = model_smoothing)))) %>%
  mutate(importance_smoothed = list(att_xy$y),
         cycles_smoothed = list(arr_xy$y)) %>%
  unnest(cols = c(data, importance_smoothed, cycles_smoothed)) %>%
  dplyr::select(-c(att_xy, arr_xy)) %>%
  ungroup() %>%
  mutate(zatt_smoothed = scale(importance_mu))

arousal <- smoothed %>%
  group_by(scene, frame) %>%
  summarise(arousal = sum(cycles_smoothed)) %>%
  ungroup()

full_data <- smoothed %>%
  left_join(arousal, by = c("scene", "frame"))


scene_for_fig = 24

colors = c("#a28f9d","#881671","#e77728","#29e7cd")

fig1_e_importance <- full_data %>%
  filter(scene == scene_for_fig) %>%
  ggplot(aes(x = frame, y = importance_smoothed)) +
  scale_color_manual(values = colors) +
  geom_line(aes(color = factor(tracker)), size = 1.5) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        # axis.text = element_blank(),
        aspect.ratio = 0.075,
        axis.line = element_blank()
  )

ggsave("~/project/figures/fig1_e_importance.svg", plot = fig1_e_importance)

fig1_e_arousal <- arousal %>%
  filter(scene == scene_for_fig) %>%
  ggplot(aes(x = frame, y = arousal)) +
  scale_color_manual(values = colors) +
  geom_ribbon(aes(ymin = 0, ymax = arousal),
              fill = "gray",
              alpha = 0.5) + 
  geom_line(size = 2.0, colour = "black") + 
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        # axis.text = element_blank(),
        aspect.ratio = 0.075,
        axis.line = element_blank()
  )

ggsave("~/project/figures/fig1_e_arousal.svg", plot = fig1_e_arousal)
