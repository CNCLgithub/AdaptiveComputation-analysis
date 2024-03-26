
################################# includes #####################################
library(tidyverse)
library(ggplot2)
library(readr)
library(boot)
# library(slider)

############################# hyper parameters #################################
model_smoothing = 48.0 # 12 frames per sd in gaussian kernel; 
min_sensitivity = -60000

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
# model_att <- read_csv("project/data/exp2/exp2_probes_adaptive_computation_td_att.csv")
# model_att <- read_csv("project/data/exp2/force_probes_adaptive_computation_eu_att.csv")
# model_att <- read_csv("project/data/exp2/fe_probes_adaptive_computation_eu_att.csv")
model_att <- read_csv("project/data/exp2/exp2_probes_ac_td_att.csv")



model_perf <- read_csv("project/data/exp2/fe_probes_adaptive_computation_eu_perf.csv") %>%
  group_by(scene, chain) %>%
  summarise(td = mean(td_acc)) %>%
  ungroup()

# distance to nearest distractor;s from `mot/scripts/analysis/nd_probes.jl`
# (not possible to extract directly from model predictions)
dnd_predictions <- read_csv("project/data/exp2/exp2_probes_adaptive_computation_td_dnd_centroid.csv") %>%
  select(scene, frame, d_ic) %>%
  rename(dist_to_nd = d_ic)

############################# smoothing kernel##################################
# min_sensitivity <- model_att %>%
#   filter(!is.infinite((sensitivity)))
# min_sensitivity = min(min_sensitivity$sensitivity)

smoothed_df <- model_att %>%
  left_join(model_perf, by = c("scene", "chain")) %>%
  # clean up -Inf sensitivity values
  # mutate(sensitivity = ifelse(is.infinite(sensitivity),
  #                             min_sensitivity,
  #                             sensitivity)) %>%
  group_by(chain, scene, tracker) %>%
  # add smoothing
  nest_by() %>%
  mutate(att_xy = list(with(data,
                            ksmooth(frame, importance, kernel = "normal",
                                    bandwidth = model_smoothing))),
         cycles_xy = list(with(data,
                               ksmooth(frame, cycles, kernel = "normal",
                                       bandwidth = model_smoothing))),
         pred_xs = list(with(data,
                             ksmooth(frame, pred_x, kernel = "normal",
                                     bandwidth = model_smoothing))),
         pred_ys = list(with(data,
                             ksmooth(frame, pred_y, kernel = "normal",
                                     bandwidth = model_smoothing))),
# 
#          sens_xy = list(with(data,
#                              ksmooth(frame, sensitivity, kernel = "normal",
#                                      bandwidth = model_smoothing))),
  ) %>%
  mutate(importance_smoothed = list(att_xy$y),
         cycles_smoothed = list(cycles_xy$y),
         # sensitivity_smoothed = list(sens_xy$y),
         pred_x_smoothed = list(pred_xs$y),
         pred_y_smoothed = list(pred_ys$y),
  ) %>%
  unnest(cols = c(data,
                  importance_smoothed,
                  cycles_smoothed,
                  # sensitivity_smoothed,
                  pred_x_smoothed, pred_y_smoothed,
  )) %>%
  dplyr::select(-c(att_xy,
                   cycles_xy,
                   # sens_xy,
                   pred_xs,
                   pred_ys,
  )) %>%
  ungroup()

# smoothed_df <- model_att %>%
#   left_join(model_perf, by = c("scene", "chain")) %>%
#   mutate(importance_smoothed = importance,
#          pred_x_smoothed = pred_x,
#          pred_y_smoothed = pred_y,
#          cycles_smoothed = cycles)

# extract probed frames
smoothed_df <- probe_timings %>%
  left_join(smoothed_df, by = c("scene", "frame"))

centroids <- smoothed_df %>%
  group_by(scene, chain, frame) %>%
  summarise(total_cycles = sum(cycles_smoothed),
            tot_att_x = sum(pred_x_smoothed * importance_smoothed),
            tot_att_y = sum(pred_y_smoothed * importance_smoothed),
            tot_avg_x = mean(pred_x_smoothed),
            tot_avg_y = mean(pred_y_smoothed)) %>%
  ungroup()

tgt_centroids <- smoothed_df %>%
  filter(tracker <= 4) %>%
  group_by(scene, chain, frame) %>%
  summarise(tgt_att_x = sum(pred_x_smoothed * importance_smoothed),
            tgt_att_y = sum(pred_y_smoothed * importance_smoothed),
            tgt_avg_x = mean(pred_x_smoothed),
            tgt_avg_y = mean(pred_y_smoothed)
            ) %>%
  ungroup()

probe_positions <- smoothed_df %>%
  filter(tracker == probed_tracker) %>%
  rename(probe_x = pred_x, 
         probe_y = pred_y) %>%
  select(chain, scene, epoch, probe_x, probe_y)

importance_weighted <- smoothed_df %>%
  select(chain, scene, epoch, tracker, pred_x_smoothed, pred_y_smoothed, importance_smoothed, cycles_smoothed) %>%
  left_join(probe_positions, by = c("chain", "scene", "epoch")) %>%
  mutate(dist_to_probe = sqrt((probe_x - pred_x_smoothed)^2 + (probe_y - pred_y_smoothed)^2))

probe_dist_w = max(importance_weighted$dist_to_probe)

importance_weighted <- importance_weighted %>%
  mutate(
    # importance_weighted = importance_smoothed * exp(dist_to_probe - probe_dist_w),
    # importance_weighted = importance_smoothed * dist_to_probe/probe_dist_w,
    # importance_weighted = cycles_smoothed * dist_to_probe/probe_dist_w,
    importance_weighted = importance_smoothed * exp(-dist_to_probe/probe_dist_w),
  ) %>%
  select(c(chain, scene, epoch, tracker, importance_weighted))

# add distance to weighted and unweighted tracker means
result <- smoothed_df %>%
  # filter(tracker <= 4) %>%
  left_join(centroids, by = c("chain", "scene", "frame")) %>%
  left_join(tgt_centroids, by = c("chain", "scene", "frame")) %>%
  left_join(importance_weighted, by = c("chain", "scene", "epoch", "tracker")) %>%
  mutate(
    # a3_centroid = sqrt((tgt_att_x - pred_x_smoothed)^2 + (tgt_att_y - pred_y_smoothed)^2),
    # a3_centroid_tot = sqrt((tot_att_x - pred_x_smoothed)^2 + (tot_att_y - pred_y_smoothed)^2),
    a3_centroid = sqrt((tot_att_x - pred_x)^2 + (tot_att_y - pred_y)^2),
    
    # geo_centroid = sqrt((tgt_avg_x - pred_x_smoothed)^2 + (tgt_avg_y - pred_y_smoothed)^2),
    # geo_centroid_tot = sqrt((tot_avg_x - pred_x_smoothed)^2 + (tot_avg_y - pred_y_smoothed)^2),
    geo_centroid = sqrt((tot_avg_x - pred_x)^2 + (tot_avg_y - pred_y)^2),
    
    dist_to_center = sqrt(pred_x_smoothed^2 + pred_y_smoothed^2),
  ) %>%
  group_by(scene, frame, tracker, probed_tracker, epoch) %>%
  # average across chains
  summarise(across(-chain, mean)) %>%
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
