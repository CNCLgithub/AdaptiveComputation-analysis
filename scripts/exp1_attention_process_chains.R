
################################# includes #####################################
library(tidyverse)
library(ggplot2)
library(readr)
library(boot)

############################# hyper parameters #################################
model_smoothing = 48.0 # 12 frames per sd in gaussian kernel; 
min_sensitivity = -200

############################### load raw data ##################################
probe_timings <- read_csv("project/data/exp2/exp2_probe_map_random.csv") %>%
  select(-contains("att")) %>%
  filter(scene <= 40) %>%
  group_by(scene) %>%
  mutate(epoch = dense_rank(frame)) %>%
  ungroup() %>%
  rename(probed_tracker = tracker)

# attention traces 
# scene, frame, tracker, cycles, chain, ...,
model_att <- read_csv("project/data/exp2/exp2_probes_adaptive_computation_td_att.csv")
# model_att <- read_csv("project/data/exp2/exp2_probes_adaptive_computation_att.csv")

# distance to nearest distractor
# (not possible to extract directly from model predictions)
dnd_predictions <- read_csv("project/data/exp2/exp2_probes_adaptive_computation_td_dnd_centroid.csv") %>%
  select(scene, frame, d_ic) %>%
  rename(dist_to_nd = d_ic)

############################# smoothing kernel##################################
smoothed_df <- model_att %>%
  # clean up -Inf sensitivity values
  mutate(sensitivity = ifelse(is.infinite(sensitivity),
                              min_sensitivity,
                              sensitivity)) %>%
  group_by(chain, scene, tracker) %>%
  # add smoothing
  nest_by() %>%
  mutate(att_xy = list(with(data,
                            ksmooth(frame, importance, kernel = "normal",
                                    bandwidth = model_smoothing))),
       cycles_xy = list(with(data,
                               ksmooth(frame, cycles, kernel = "normal",
                                       bandwidth = model_smoothing))),
         sens_xy = list(with(data,
                             ksmooth(frame, sensitivity, kernel = "normal",
                                     bandwidth = model_smoothing))),
         ) %>%
  mutate(importance_smoothed = list(att_xy$y),
         cycles_smoothed = list(cycles_xy$y),
         sensitivity_smoothed = list(sens_xy$y),
         ) %>%
  unnest(cols = c(data, 
                  importance_smoothed, 
                  cycles_smoothed, 
                  sensitivity_smoothed,
                  )) %>%
  dplyr::select(-c(att_xy, 
                   cycles_xy, 
                   sens_xy,
                   )) %>%
  ungroup()

# extract probed frames
smoothed_df <- probe_timings %>%
left_join(smoothed_df, by = c("scene", "frame"))

centroids <- smoothed_df %>%
  group_by(scene, chain, frame) %>%
  summarise(total_cycles = sum(cycles),
            weighted_x = sum(pred_x * importance_smoothed),
            weighted_y = sum(pred_y * importance_smoothed),
            geo_x = mean(pred_x),
            geo_y = mean(pred_y)) %>%
  ungroup()

probe_positions <- smoothed_df %>%
  filter(tracker == probed_tracker) %>%
  rename(probe_x = pred_x, 
         probe_y = pred_y) %>%
  select(chain, scene, epoch, probe_x, probe_y)

importance_weighted <- smoothed_df %>%
  select(chain, scene, epoch, tracker, pred_x, pred_y, cycles_smoothed) %>%
  left_join(probe_positions, by = c("chain", "scene", "epoch")) %>%
  mutate(dist_to_probe = sqrt((probe_x - pred_x)^2 + (probe_y - pred_y)^2),
         # importance_weighted = cycles_smoothed / log(dist_to_probe),
         # importance_weighted = cycles_smoothed / (1 + dist_to_probe),
         
         ) %>%
  select(-c(starts_with("pred"), cycles_smoothed))

# add distance to weighted and unweighted tracker means
result <- smoothed_df %>%
  left_join(centroids, by = c("chain", "scene", "frame")) %>%
  left_join(importance_weighted, by = c("chain", "scene", "epoch", "tracker")) %>%
  mutate(a3_centroid = sqrt((weighted_x - pred_x)^2 + (weighted_y - pred_y)^2),
         geo_centroid = sqrt((geo_x - pred_x)^2 + (geo_y - pred_y)^2),
         dist_to_center = sqrt(pred_x^2 + pred_y^2),
  ) %>%
  group_by(scene, frame, tracker, probed_tracker, epoch) %>%
  # average across chains
  summarise(across(-chain, list(mu = mean,
                                sd = sd))) %>%
  ungroup() %>%
  left_join(dnd_predictions)


############################ visual inspection #################################
result %>%
  filter(scene <= 5) %>%
  ungroup() %>%
  ggplot(aes(x = frame)) +
  geom_point(aes(y = a3_centroid_mu, color = factor(tracker))) +
  facet_grid(rows = vars(scene))


############################### save result ####################################
write_csv(result, "project/data/exp2/model_probe_covariates.csv")




################################## misc ########################################
# 
# 
# loc_error <- result %>%
#   group_by(scene) %>%
#   filter(frame == max(frame)) %>%
#   mutate(pred_sd = pred_x_sd_mu + pred_y_sd_mu)
# 
# loc_error %>%
#   ggplot(aes(x = cycles_mu, y = pred_sd)) + 
#   geom_point() + 
#   geom_smooth(method="lm")
# 
# loc_error %>%
#   ggplot(aes(x = a3_centroid_mu, y = pred_sd)) + 
#   geom_point() + 
#   geom_smooth(method="lm")
# 
# loc_error %>%
#   ggplot(aes(x = importance_smoothed_mu, y = pred_sd)) + 
#   geom_point() + 
#   geom_smooth(method="lm")
# 
# loc_error %>%
#   ggplot(aes(x = sensitivity_smoothed_mu, y = pred_sd)) + 
#   geom_point() + 
#   geom_smooth(method="lm")
# 
# loc_error %>%
#   with(lm(pred_sd ~ cycles_smoothed_mu)) %>%
#   summary()

