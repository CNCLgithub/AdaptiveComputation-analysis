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

attention_full <- read_csv("data/attention.csv") %>%
  mutate(frame = t * 4) %>%
  filter(between(frame, 0, 120)) %>%
  select(-t)

full_data <- attention_full %>%
  pivot_longer(-c(frame, trial), names_to = "tracker", 
               values_to = "att") %>%
  group_by(trial, frame) %>%
  summarise(total_att = sum(att))%>%
  left_join(attention_full) %>%
  mutate(scene = trial - 1)


# Now lets look at the top 10 most different scenes

top_trials <- full_data %>%
  group_by(scene) %>%
  summarise(total_att = sum(total_att)) %>%
  top_n(10, total_att) %>%
  select(scene)

top_att <- full_data %>%
  filter(scene %in% top_trials$scene) %>% 
  ungroup()

top_att %>%
  select(scene, frame, starts_with("tracker")) %>%
  pivot_longer(-c(frame, scene), names_to = "tracker", 
               values_to = "diff") %>%
  ggplot(aes(frame, tracker)) + 
  geom_tile(aes(fill = diff)) + 
  scale_fill_gradient2(low = muted("blue"), 
                       high = muted("red")) + 
  facet_wrap(vars(scene))
ggsave("output/attention_trial_tps.png")

td_points <- top_att %>%
  select(scene, frame, starts_with("tracker")) %>%
  pivot_longer(-c(frame, scene), names_to = "tracker", 
               values_to = "diff") %>%
  group_by(scene) %>%
  slice(which.max(diff)) %>%
  separate(tracker, c(NA, "tracker"), "_") %>%
  mutate(tracker = as.numeric(tracker)) %>%
  rename(td_t = frame, td_diff = diff, td_tracker = tracker)

dc_points <- top_att %>%
  select(scene, frame, starts_with("tracker")) %>%
  pivot_longer(-c(frame, scene), names_to = "tracker", 
               values_to = "diff") %>%
  group_by(scene) %>%
  slice(which.min(diff)) %>%
  separate(tracker, c(NA, "tracker"), "_") %>%
  mutate(tracker = as.numeric(tracker)) %>%
  rename(dc_t = frame, dc_diff = diff, dc_tracker = tracker)

time_points <- left_join(td_points, dc_points)
write.csv(time_points, row.names = FALSE, 
          file = "output/attention_trial_tps.csv")

