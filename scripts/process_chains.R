
################################# includes #####################################
library(tidyverse)
library(ggplot2)
library(readr)
library(boot)

############################# hyper parameters #################################
model_smoothing = 48.0 # 12 frames per sd in gaussian kernel; 
min_sensitivity = -60000

process_chains <- function(probe_path,
                           att_path,
                           dnd_path) {
  
  ############################### load raw data ##################################
  
  probe_timings <- read_csv(probe_path) %>%
    select(-contains("att")) %>%
    filter(scene <= 40) %>%
    group_by(scene) %>%
    mutate(epoch = dense_rank(frame)) %>%
    ungroup() %>%
    rename(probed_tracker = tracker)
  

  # attention traces; processed from `mot/scripts/analysis/aggregate_chains.jl`
  # scene, frame, tracker, cycles, chain, ...,
  model_att <- read_csv(att_path)

  # distance to nearest distractor;s from `mot/scripts/analysis/nd_probes.jl`
  # (not possible to extract directly from model predictions)
  dnd_predictions <- read_csv(dnd_path) %>%
    select(scene, frame, d_ic) %>%
    rename(dist_to_nd = d_ic)
  
  ############################# smoothing kernel##################################
  
  
  smoothed_df <- model_att %>%
    group_by(chain, scene, tracker) %>%
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
           
    ) %>%
    mutate(importance_smoothed = list(att_xy$y),
           cycles_smoothed = list(cycles_xy$y),
           pred_x_smoothed = list(pred_xs$y),
           pred_y_smoothed = list(pred_ys$y),
    ) %>%
    unnest(cols = c(data,
                    importance_smoothed,
                    cycles_smoothed,
                    pred_x_smoothed, pred_y_smoothed,
    )) %>%
    dplyr::select(-c(att_xy,
                     cycles_xy,
                     pred_xs,
                     pred_ys,
    )) %>%
    ungroup()
  
  # select probed frames
  smoothed_df <- probe_timings %>%
    left_join(smoothed_df, by = c("scene", "frame"))
  
  # centroids of all tracked objects (targets)
  centroids <- smoothed_df %>%
    group_by(scene, chain, frame) %>%
    summarise(total_cycles = sum(cycles_smoothed),
              # Target center (unweighted)
              tot_x = mean(pred_x_smoothed),
              tot_y = mean(pred_y_smoothed),
              # Attention weighted 
              tot_att_x = sum(pred_x_smoothed * importance_smoothed),
              tot_att_y = sum(pred_y_smoothed * importance_smoothed),
    ) %>%
    ungroup()
  
  # positions of probed object
  probe_positions <- smoothed_df %>%
    filter(tracker == probed_tracker) %>%
    rename(probe_x = pred_x, 
           probe_y = pred_y) %>%
    select(chain, scene, epoch, probe_x, probe_y)
  
  # importance weighted average distance of objects to probe
  # Step 1: l2 distance of probe to each object
  importance_weighted <- smoothed_df %>%
    select(chain, scene, epoch, tracker, pred_x_smoothed, pred_y_smoothed, 
           importance_smoothed, cycles_smoothed) %>%
    left_join(probe_positions, by = c("chain", "scene", "epoch")) %>%
    mutate(dist_to_probe = sqrt((probe_x - pred_x_smoothed)^2 + (probe_y - pred_y_smoothed)^2))
  
  # Step 2: Normalization constant (largest L2 probe-target distance in dataset)
  probe_dist_w = max(importance_weighted$dist_to_probe)
  
  # Step 3: weight the exp(L2 distance) by importance
  importance_weighted <- importance_weighted %>%
    mutate(
      importance_weighted = importance_smoothed * exp(-dist_to_probe/probe_dist_w),
    ) %>%
    select(c(chain, scene, epoch, tracker, importance_weighted))
  
  # add distance to weighted and unweighted tracker means
  result <- smoothed_df %>%
    # filter(tracker <= 4) %>%
    left_join(centroids, by = c("chain", "scene", "frame")) %>%
    left_join(importance_weighted, by = c("chain", "scene", "epoch", "tracker")) %>%
    # Compute measures for AC-centroid, Target center, and ECC
    mutate(
      a3_centroid = sqrt((tot_att_x - pred_x)^2 + (tot_att_y - pred_y)^2),
      geo_centroid = sqrt((tot_x - pred_x)^2 + (tot_y - pred_y)^2),
      dist_to_center = sqrt(pred_x_smoothed^2 + pred_y_smoothed^2),
    ) %>%
    group_by(scene, frame, tracker, probed_tracker, epoch) %>%
    # average across chains
    summarise(across(-chain, mean)) %>%
    ungroup() %>%
    left_join(dnd_predictions)
  
  return(result)

}

# 
# 
# 
# ############################ visual inspection #################################
# result %>%
#   filter(scene <= 5) %>%
#   ungroup() %>%
#   ggplot(aes(x = frame)) +
#   geom_point(aes(y = a3_centroid, color = factor(tracker))) +
#   facet_grid(rows = vars(scene))
# 
# 
# ############################### save result ####################################
# write_csv(result, "project/data/probes/model_probe_covariates_revision.csv")
