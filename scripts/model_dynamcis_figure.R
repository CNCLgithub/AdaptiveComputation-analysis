
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
model_att <- read_csv("data/exp2/exp2_probes_target_designation_att.csv") 

model_smoothing = 12

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
total_att <- exp_summary %>%
  group_by(scene, frame) %>%
  summarise(total_att = sum(exp(att_smoothed)),
            cycles_sd = sd(att_smoothed)) %>%
  ungroup()



# add lag and lead att values
# also add cumulative att
full_data <- exp_summary %>%
  left_join(total_att, by = c("scene", "frame")) %>%
  mutate(prop_cycles = att_smoothed / total_att) %>%
  group_by(scene, tracker) %>%
  mutate(across(contains("smoothed"), list(lag10 = ~lag(.x, 10))),
         across(!contains("lag")  & contains("smoothed"), list(lead10 = ~lead(.x, 10))),
         cum_att = cumsum(att_smoothed),
         cum_zatt = cumsum(zatt)/frame) %>%
  ungroup()


probe_limit = 48
window_size = 60
max_peaks = 4
tmax = max(full_data$frame)
n_trackers = length(unique(full_data$tracker))

scene_peaks <- function(att) {
  matt <- findpeaks(att, npeaks = max_peaks, minpeakheight = -200,
                    minpeakdistance = window_size)
  return(matt[, 2])
}

#take the highs for each scene with at least some buffer between
tps <- total_att %>%
  # cant be too close to the beginning or end of trial
  filter(between(frame, probe_limit, tmax-probe_limit)) %>%
  nest_by(scene) %>%
  mutate(frame = list(scene_peaks(data$total_att))) %>%
  select(-data) %>%
  unnest(cols = c(frame)) %>%
  mutate(frame = frame + probe_limit - 1,
         peak = TRUE) %>%
  right_join(full_data, by = c("scene", "frame") ) %>%
  mutate(peak = !is.na(peak)) %>%
  arrange(scene, frame) %>%
  group_by(scene, frame) %>%
  mutate(att_rank = dense_rank(cycles_mu)) %>%
  ungroup()

tps %>%
  filter(scene %in% c(24)) %>%
  ggplot(aes(x = frame, y = att_smoothed)) +
  scale_color_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3")) +
  geom_line(aes(color = factor(tracker)), size = 1.5) +
  # geom_point(aes(y = if_else(peak, att_smoothed, NaN))) +
  # geom_vline(aes(xintercept = which(peak))) +
  facet_grid(rows = vars(scene))

hex_codes1 <- hue_pal()(5)                             # Identify hex codes
hex_codes1          

tps %>%
  filter(scene %in% c(11)) %>%
  ggplot(aes(frame, tracker)) +
  geom_tile(aes(fill = att_smoothed)) +
  scale_fill_gradient(low = "white",
                      high = "#bc2626ff") + # muted("red")) +
  facet_grid(rows = vars(scene))

exp_summary %>%
  filter(scene == 24) %>%
  group_by(scene, frame) %>%
  summarise(total_att = sum(cycles_mu)) %>%
  # add smoothing
  nest_by() %>%
  mutate(att_xy = list(with(data,
                            ksmooth(frame, total_att, kernel = "normal", 
                                    bandwidth = 12)))) %>%
  mutate(att_smoothed = list(att_xy$y)) %>%
  unnest(cols = c(data, att_smoothed)) %>%
  dplyr::select(-c(att_xy)) %>%
  ungroup() %>%
  ggplot(aes(x = frame, y = att_smoothed)) + 
  geom_line(size = 1.5)
# max_peaks = 4
# scene_peaks <- function(att) {
#   matt <- findpeaks(att, npeaks = max_peaks, minpeakheight = -200,
#                     minpeakdistance = window_size * n_trackers)
#   return(matt[, 2])
# }
# take the highs for each scene with at least some buffer between
# tps <- full_data %>%
#   # cant be too close to the beginning or end of trial
#   filter(between(frame, probe_limit, tmax-probe_limit)) %>%
#   nest_by(scene) %>%
#   mutate(frame = list(scene_peaks(data$cycles_mu))) %>%
#   select(-data) %>%
#   unnest(cols = c(frame)) %>%
#   mutate(frame = round(frame / n_trackers) + probe_limit) %>%
#   left_join(full_data) %>%
#   arrange(scene, frame) %>%
#   group_by(scene, frame) %>%
#   mutate(att_rank = dense_rank(cycles_mu)) %>%
#   ungroup()

# full att distribution
tps %>%
  ggplot(aes(cycles_mu)) +
  geom_histogram(aes(fill = factor(att_rank)))

tps %>%
  ggplot(aes(log_att)) +
  geom_histogram(aes(fill = factor(att_rank)))

# peak att
tps %>%
  group_by(scene, frame) %>%
  summarise(att = max(cycles_mu)) %>%
  ggplot(aes(frame)) +
  geom_histogram()



att_map <- tps %>%
  group_by(scene, frame) %>%
  mutate(att_rank = dense_rank(cycles_mu)) %>%
  ungroup() %>%
  filter(att_rank== 1 | att_rank == 4) %>%
  arrange(scene, frame, cycles_mu)

att_map %>%
  ggplot(aes(cycles_mu)) +
  geom_histogram()

tps %>%
  filter(scene %in% c(34)) %>%
  ggplot(aes(frame, tracker)) +
  geom_tile(aes(fill = zatt_smoothed)) +
  scale_fill_gradient2(low = muted("blue"),
                       high = "#FF0000") + # muted("red")) +
  facet_grid(rows = vars(scene))
ggsave("output/exp2/attention_trial_tps.png")
