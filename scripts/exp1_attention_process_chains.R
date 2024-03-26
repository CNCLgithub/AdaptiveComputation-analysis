
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
  # filter(between(scene, 6, 12)) %>%
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
# model_att <- read_csv("project/data/exp2/force_probes_adaptive_computation_eu_att.csv")



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
                                                 .before = 6,
                                                 .after = 6))),
       cycles_smoothed = list(with(data,
                                     slide_vec(cycles_mean, mean, 
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
  unnest(cols = c(data, importance_smoothed, cycles_smoothed)) %>%
  ungroup()


############################ visual inspection #################################

averaged_chains %>%
  filter(scene <= 5) %>%
  ungroup() %>%
  ggplot(aes(x = frame)) +
  geom_line(aes(y = importance_mean, color = factor(tracker))) +
  facet_grid(rows = vars(scene))

smoothed_df %>%
  filter(scene <= 5) %>%
  ungroup() %>%
  ggplot(aes(x = frame)) +
  geom_line(aes(y = importance_smoothed, color = factor(tracker))) +
  facet_grid(rows = vars(scene))



# extract probed frames
smoothed_df <- probe_timings %>%
  left_join(smoothed_df, by = c("scene", "frame"))

all_centroids <- smoothed_df %>%
  group_by(scene, frame) %>%
  summarise(total_cycles = sum(cycles_mean),
            all_weighted_x = sum(pred_x_mean * importance_smoothed),
            all_weighted_y = sum(pred_y_mean * importance_smoothed),
            all_geo_x = mean(pred_x_mean),
            all_geo_y = mean(pred_y_mean)) %>%
  ungroup()

tgt_centroids <- smoothed_df %>%
  filter(tracker <= 4) %>%
  group_by(scene, frame) %>%
  summarise(tgt_weighted_x = sum(pred_x_mean * importance_smoothed),
            tgt_weighted_y = sum(pred_y_mean * importance_smoothed),
            tgt_geo_x = mean(pred_x_mean),
            tgt_geo_y = mean(pred_y_mean)) %>%
  ungroup()

probe_positions <- smoothed_df %>%
  filter(tracker == probed_tracker) %>%
  rename(probe_x = pred_x_mean, 
         probe_y = pred_y_mean) %>%
  select(scene, epoch, probe_x, probe_y)

importance_weighted <- smoothed_df %>%
  # filter(tracker <= 4) %>%
  select(scene, epoch, tracker, pred_x_mean, pred_y_mean, importance_smoothed, cycles_smoothed) %>%
  left_join(probe_positions, by = c("scene", "epoch")) %>%
  mutate(dist_to_probe = sqrt((probe_x - pred_x_mean)^2 + (probe_y - pred_y_mean)^2))

probe_dist_w = max(importance_weighted$dist_to_probe)

importance_weighted <- importance_weighted %>%
  mutate(
    # importance_weighted = importance_smoothed * (1.0 -dist_to_probe/probe_dist_w),
    importance_weighted = importance_smoothed * exp(-dist_to_probe / probe_dist_w),
    # importance_weighted =  cycles_smoothed * exp(-dist_to_probe/max(dist_to_probe)),
    
  ) %>%
  select(c(scene, epoch, tracker, importance_weighted))

# add distance to weighted and unweighted tracker means
result <- smoothed_df %>%
  # left_join(tgt_centroids, by = c("scene", "frame")) %>%
  left_join(all_centroids, by = c("scene", "frame")) %>%
  left_join(importance_weighted, by = c("scene", "epoch", "tracker")) %>%
  mutate(
    a3_centroid = sqrt((all_weighted_x - pred_x_mean)^2 + (all_weighted_y - pred_y_mean)^2),
    geo_centroid = sqrt((all_geo_x - pred_x_mean)^2 + (all_geo_y - pred_y_mean)^2),
    # a3_centroid = sqrt((tgt_weighted_x - pred_x_mean)^2 + (tgt_weighted_y - pred_y_mean)^2),
    # geo_centroid = sqrt((tgt_geo_x - pred_x_mean)^2 + (tgt_geo_y - pred_y_mean)^2),
    dist_to_center = sqrt(pred_x_mean^2 + pred_y_mean^2),
  ) %>%
  left_join(dnd_predictions)




############################### save result ####################################
write_csv(result, "project/data/exp2/model_probe_covariates.csv")
