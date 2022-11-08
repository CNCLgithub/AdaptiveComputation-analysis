
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

# cycles per frame
# scene, frame, tracker, cycles, chain
model_att <- read_csv("project/data/exp2/exp2_probes_target_designation_att.csv") 

model_smoothing = 8

smoothed <- model_att %>%
  filter(scene == 24) %>%
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
                            ksmooth(frame, attention_mu, kernel = "normal", 
                                    bandwidth = model_smoothing)))) %>%
  mutate(att_smoothed = list(att_xy$y)) %>%
  unnest(cols = c(data, att_smoothed)) %>%
  dplyr::select(-c(att_xy)) %>%
  ungroup() %>%
  mutate(zatt_smoothed = scale(att_smoothed))


# sum up the total amount of attention
tau = 17.5
aggregated <- smoothed %>%
  group_by(scene, frame) %>%
  summarise(total_att = sum(exp(att_smoothed / tau)))%>%
  ungroup()


# add lag and lead att values
# also add cumulative att
full_data <- smoothed %>%
  left_join(aggregated, by = c("scene", "frame")) %>%
  mutate(importance = exp(att_smoothed / tau) / total_att)


full_data %>%
  ggplot(aes(x = frame, y = importance)) +
  scale_color_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3")) +
  geom_line(aes(color = factor(tracker)), size = 1.2) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        aspect.ratio = 0.15,
        axis.line = element_blank()
  )

full_data %>%
  group_by(frame) %>%
  summarise(cycles = sum(cycles_mu),
            tatt = mean(total_att)) %>%
  ungroup() %>%
  ggplot(aes(x = frame, y = tatt)) +
  scale_color_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3")) +
  geom_ribbon(aes(ymin = 0, ymax = tatt),
              fill = "gray",
              alpha = 0.5) + 
  geom_line(size = 1.2, colour = "black") + 
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        aspect.ratio = 0.15,
        axis.line = element_blank()
  )

