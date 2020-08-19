library(tidyverse)
library(estimatr)
library(ggplot2)
library(readr)
library(scales)

th <- theme_classic()
theme_set(th)

parsed_trials <- read_csv("data/parsed_trials.csv")
parsed_trials <- parsed_trials %>%
  mutate(answer = dot <= 4,
         correct = (Response == answer))

# subject screening
good_subjects <- parsed_trials %>%
  group_by(ID) %>%
  summarise(avg_acc = mean(correct),
            n = n(),
            acc_se = sd(correct)/sqrt(n),
            passed = (avg_acc - acc_se) > 0.5) %>%
  filter(passed)
good_subjects_data <- parsed_trials %>%
  filter(ID %in% good_subjects$ID)

across_subjects <-good_subjects_data %>%
  group_by(ID) %>%
  mutate(z_rating = scale(Rating)) %>%
  group_by(scene) %>%
  summarise(avg_acc = mean(correct),
            avg_rating = mean(Rating),
            avg_z_rating = mean(z_rating),
            spring = mean(spring),
            sigma_w = mean(sigma_w),
            n = n(),
            se = sd(correct)/sqrt(n))

attention_df <- read_csv("data/attention.csv")

euc.dist <- function(x) sqrt(sum((x) ^ 2))

attention_full <- attention_df %>%
  pivot_longer(-c(t, trial), names_to = "tracker", 
               values_to = "diff") %>%
  group_by(trial, t) %>%
  summarise(l2diff = euc.dist(diff),
            difvar = sd(diff)) %>%
  left_join(attention_df)  %>%
  mutate(scene = trial - 1)



att_sum <- attention_full %>%
  group_by(trial) %>%
  summarise(diff = max(l2diff))

full_data <- merge(across_subjects, att_sum)

# Lets take a look at Attention differences across scenes

full_data %>%
  ggplot(aes(x = spring, y = sigma_w, fill = diff)) +
  geom_tile() +
  labs(fill = "Max att diff.") + 
  scale_fill_gradient(low = "#0072B2", high = "white") +
  ggtitle("Attention difference across parameters")

full_data %>%
  ggplot(aes(x = spring, y = sigma_w, fill = avg_acc)) +
  geom_tile() +
  labs(fill = "Avg Acc") + 
  scale_fill_gradient(high = "#0072B2", low = "white") +
  ggtitle("Human Acc. across parameters")

full_data %>%
  ggplot(aes(x = spring, y = sigma_w, fill = avg_z_rating)) +
  geom_tile() +
  labs(fill = "Scaled effort") + 
  scale_fill_gradient(low = "#0072B2", high = "white") +
  ggtitle("Human Effort across parameters")

# Now lets look at the top 10 most different scenes

top_trials <- full_data %>%
  top_n(10, diff) %>%
  select(scene)

top_att <- attention_full %>%
  filter(scene %in% top_trials$scene) %>% 
  ungroup()

top_att %>%
  select(scene, t, starts_with("tracker")) %>%
  pivot_longer(-c(t, scene), names_to = "tracker", 
               values_to = "diff") %>%
  ggplot(aes(t, tracker)) + 
  geom_tile(aes(fill = diff)) + 
  scale_fill_gradient2(low = muted("blue"), 
                       high = muted("red")) + 
  facet_wrap(vars(scene))

td_points <- top_att %>%
  select(scene, t, starts_with("tracker")) %>%
  pivot_longer(-c(t, scene), names_to = "tracker", 
               values_to = "diff") %>%
  group_by(scene) %>%
  slice(which.max(diff)) %>%
  separate(tracker, c(NA, "tracker"), "_") %>%
  mutate(tracker = as.numeric(tracker)) %>%
  rename(td_t = t, td_diff = diff, td_tracker = tracker)

dc_points <- top_att %>%
  select(scene, t, starts_with("tracker")) %>%
  pivot_longer(-c(t, scene), names_to = "tracker", 
               values_to = "diff") %>%
  group_by(scene) %>%
  slice(which.min(diff)) %>%
  separate(tracker, c(NA, "tracker"), "_") %>%
  mutate(tracker = as.numeric(tracker)) %>%
  rename(dc_t = t, dc_diff = diff, dc_tracker = tracker)
  
time_points <- left_join(td_points, dc_points)
write.csv(time_points, row.names = FALSE, 
          file = "output/attention_trial_tps.csv")

