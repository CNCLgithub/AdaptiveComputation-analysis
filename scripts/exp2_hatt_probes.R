library(estimatr)
library(ggplot2)
library(readr)
library(scales)
# library(raster)
library(minpack.lm)
library(tidyverse)
library(pracma)

th <- theme_classic()
theme_set(th)

# cycles per frame
# scene, frame, tracker, cycles, chain
model_att <- read_csv("data/exp2/exp2_probes_target_designation_att.csv") 

model_smoothing = 6

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
                            ksmooth(frame, attention_mu, kernel = "normal", 
                                    bandwidth = model_smoothing)))) %>%
  mutate(att_smoothed = list(att_xy$y)) %>%
  unnest(cols = c(data, att_smoothed)) %>%
  dplyr::select(-c(att_xy)) %>%
  ungroup() %>%
  mutate(zatt_smoothed = scale(att_smoothed))


# sum up the total amount of attention
tau = 15
total_att <- exp_summary %>%
  group_by(scene, frame) %>%
  summarise(total_att = sum(exp(att_smoothed)),
            geo_x = mean(pred_x_mu),
            geo_y = mean(pred_y_mu),
            total_exp_att = sum(exp(att_smoothed / tau)),
            weighted_x = sum(pred_x_mu * exp(att_smoothed / tau) / total_exp_att),
            weighted_y = sum(pred_y_mu * exp(att_smoothed / tau) / total_exp_att)) %>%
  ungroup()


# add lag and lead att values
# also add cumulative att
full_data <- exp_summary %>%
  left_join(total_att, by = c("scene", "frame")) %>%
  group_by(scene, tracker) %>%
  mutate(across(contains("smoothed"), list(lag10 = ~lag(.x, 10))),
         across(!contains("lag")  & contains("smoothed"), list(lead10 = ~lead(.x, 10))),
         ttwm = sqrt((weighted_x - pred_x_mu)^2 + (weighted_y - pred_y_mu)^2),
         tttm = sqrt((geo_x - pred_x_mu)^2 + (geo_y - pred_y_mu)^2)) %>%
  ungroup()

full_data %>%
  filter(scene %in% c(1,2,3,4,5)) %>%
  ggplot(aes(x = frame, y = ttwm)) +
  scale_color_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3")) +
  geom_line(aes(color = factor(tracker)), size = 1.5) +
  # geom_point(aes(y = if_else(peak, att_smoothed, NaN))) +
  facet_grid(rows = vars(scene)) + 
  ggtitle("Example of peaks on a scene")


full_data %>%
  filter(scene %in% c(1,2)) %>%
  ggplot(aes(x = frame, y = abs(ttwm - tttm))) +
  scale_color_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3")) +
  geom_line(aes(color = factor(tracker)), size = 1.5) +
  # geom_point(aes(y = if_else(peak, att_smoothed, NaN))) +
  facet_grid(rows = vars(scene)) + 
  ggtitle("Example of peaks on a scene")

probe_limit = 48
window_size = 60
max_peaks = 4
tmax = max(full_data$frame)
n_trackers = length(unique(full_data$tracker))

scene_peaks <- function(att) {
  matt <- findpeaks(att, 
                    npeaks = max_peaks, 
                    minpeakheight = -200,
                    minpeakdistance = window_size)
  return(matt[, 2])
}

#take the highs for each scene with at least some buffer between
tps <- total_att %>%
  # cant be too close to the beginning or end of trial
  filter(between(frame, probe_limit, tmax-probe_limit)) %>%
  # for each scene
  nest_by(scene) %>%
  # get n = `max_peaks` frames with peak attention
  mutate(frame = list(scene_peaks(data$total_att))) %>%
  # format into     scene     frame      peak 
  #               "numeric" "numeric" "logical" 
  select(-data) %>%
  unnest(cols = c(frame)) %>%
  mutate(frame = frame + probe_limit - 1) %>% # shift from relative to absolute
  # added tracker ids to ensure only one tracker peak per scene
  left_join(full_data, by = c("scene", "frame")) %>%
  select(scene, frame, tracker, att_smoothed) %>%
  group_by(scene, frame) %>%
  arrange(desc(att_smoothed)) %>%
  # top tracker for each peak
  filter(row_number()==1) %>%
  group_by(scene, tracker) %>%
  # one tracker id peak per scene
  filter(row_number()==1) %>%
  ungroup() %>%
  mutate(peak = TRUE,
         peak_tracker = TRUE) %>%
  # combine with the rest of the data
  select(-c(att_smoothed)) %>%
  right_join(full_data, by = c("scene", "frame", "tracker")) %>%
  mutate(peak = !is.na(peak),
         peak_tracker = !is.na(peak_tracker)) %>%
  arrange(scene, frame) %>%
  group_by(scene, frame) %>%
  mutate(att_rank = dense_rank(att_smoothed)) %>%
  ungroup()

# Several visualizations for sanity checks

tps %>%
  filter(scene %in% c(24)) %>%
  ggplot(aes(x = frame, y = att_smoothed)) +
  scale_color_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3")) +
  geom_line(aes(color = factor(tracker)), size = 1.5) +
  geom_point(aes(y = if_else(peak, att_smoothed, NaN))) +
  facet_grid(rows = vars(scene)) + 
  ggtitle("Example of peaks on a scene")

tps %>%
  filter(peak) %>%
  ggplot(aes(frame)) +
  geom_histogram(bins = 40) + 
  ggtitle("Probe temporal distribution")

probe_timings <- tps %>%
  filter(peak, peak_tracker) %>%
  select(scene, frame, tracker)

probe_timings %>%
  write.csv(row.names = FALSE,
            file = "data/exp2/exp2_probes_hatt_timings.csv")

