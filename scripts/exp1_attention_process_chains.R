
################################# includes #####################################
library(tidyverse)
library(ggplot2)
library(readr)
library(boot)
library(slider)

############################# hyper parameters #################################
model_smoothing = 48.0 # 12 frames per sd in gaussian kernel; 
window = 16L
min_sensitivity = -200

############################### load raw data ##################################
probe_timings <- read_csv("project/data/exp2/exp2_probe_map_random.csv") %>%
  select(-contains("att")) %>%
  filter(scene <= 40) %>%
  group_by(scene) %>%
  mutate(epoch = dense_rank(frame)) %>%
  ungroup() %>%
  rename(probed_tracker = tracker)

# attention traces; processed from `mot/scripts/analysis/aggregate_chains.jl`
# scene, frame, tracker, cycles, chain, ...,
# model_att <- read_csv("project/data/exp2/exp2_probes_target_designation_att.csv")
# model_att <- read_csv("project/data/exp2/07_24_23_exp2_probes_adaptive_computation_td_att.csv")
model_att <- read_csv("project/data/exp2/exp2_probes_adaptive_computation_td_att.csv")


# distance to nearest distractor; from `mot/scripts/analysis/nd_probes.jl`
# (not possible to extract directly from model predictions)
dnd_predictions <- read_csv("project/data/exp2/exp2_probes_adaptive_computation_td_dnd_centroid.csv") %>%
  select(scene, frame, d_ic) %>%
  rename(dist_to_nd = d_ic)

############################# smoothing kernel##################################

averaged_chains <- model_att %>%
  # clean up -Inf sensitivity values
  # mutate(sensitivity = ifelse(is.infinite(sensitivity),
  #                             min_sensitivity,
  #                             sensitivity)) %>%
  group_by(scene, tracker, frame) %>%
  select(-chain) %>%
  summarise(across(.fns = list(mean = mean)))

smoothed_df <- averaged_chains %>%
  group_by(scene, tracker) %>%
  # add smoothing
  nest_by() %>%
  mutate(importance_smoothed = list(with(data,
                                         slide_vec(importance_mean, mean, 
                                                   .before = window,
                                                   .after = window))),
         # cycles_xy = list(with(data,
         #                       ksmooth(frame, cycles, kernel = "normal",
         #                               bandwidth = model_smoothing))),
         # pred_xs = list(with(data,
         #                     ksmooth(frame, pred_x, kernel = "normal",
         #                             bandwidth = model_smoothing))),
         # pred_ys = list(with(data,
         #                     ksmooth(frame, pred_y, kernel = "normal",
         #                             bandwidth = model_smoothing))),
         
         # sens_xy = list(with(data,
         #                     ksmooth(frame, sensitivity, kernel = "normal",
         #                             bandwidth = model_smoothing))),
  ) %>%
  # select(-c(data)) %>%
  unnest(cols = c(data, importance_smoothed)) %>%
  ungroup()

# extract probed frames
smoothed_df <- probe_timings %>%
  left_join(smoothed_df, by = c("scene", "frame"))

centroids <- smoothed_df %>%
  group_by(scene, frame) %>%
  summarise(total_cycles = sum(cycles_mean),
            weighted_x = sum(pred_x_mean * importance_smoothed),
            weighted_y = sum(pred_y_mean * importance_smoothed),
            geo_x = mean(pred_x_mean),
            geo_y = mean(pred_y_mean)) %>%
  ungroup()

probe_positions <- smoothed_df %>%
  filter(tracker == probed_tracker) %>%
  rename(probe_x = pred_x_mean, 
         probe_y = pred_y_mean) %>%
  select(scene, epoch, probe_x, probe_y)

importance_weighted <- smoothed_df %>%
  select(scene, epoch, tracker, pred_x_mean, pred_y_mean, importance_smoothed) %>%
  left_join(probe_positions, by = c("scene", "epoch")) %>%
  mutate(dist_to_probe = sqrt((probe_x - pred_x_mean)^2 + (probe_y - pred_y_mean)^2))

probe_dist_w = max(importance_weighted$dist_to_probe)

importance_weighted <- importance_weighted %>%
         # importance_weighted = cycles_smoothed / log(dist_to_probe),
         mutate(
           importance_weighted = importance_smoothed * exp(-dist_to_probe/probe_dist_w),
         ) %>%
  select(-c(starts_with("pred"), importance_smoothed))

# add distance to weighted and unweighted tracker means
result <- smoothed_df %>%
  left_join(centroids, by = c("scene", "frame")) %>%
  left_join(importance_weighted, by = c("scene", "epoch", "tracker")) %>%
  mutate(a3_centroid = sqrt((weighted_x - pred_x_mean)^2 + (weighted_y - pred_y_mean)^2),
         geo_centroid = sqrt((geo_x - pred_x_mean)^2 + (geo_y - pred_y_mean)^2),
         dist_to_center = sqrt(pred_x_mean^2 + pred_y_mean^2),
  ) %>%
  group_by(scene, frame, tracker, probed_tracker, epoch) %>%
  ungroup() %>%
  left_join(dnd_predictions)


############################ visual inspection #################################
result %>%
  filter(scene <= 5) %>%
  ungroup() %>%
  ggplot(aes(x = frame)) +
  geom_point(aes(y = a3_centroid, color = factor(tracker))) +
  facet_grid(rows = vars(scene))


############################### save result ####################################
write_csv(result, "project/data/exp2/model_probe_covariates.csv")
