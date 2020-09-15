library(tidyverse)
library(estimatr)
library(ggplot2)
library(readr)
library(scales)

th <- theme_classic()
theme_set(th)

attention_full <- read_csv("data/attention.csv") %>%
  mutate(frame = t - 1) %>%
  filter(between(frame, 12, 108)) %>%
  select(-t)

full_data <- attention_full %>%
  pivot_longer(-c(frame, trial), names_to = "tracker", 
               values_to = "att") %>%
  group_by(trial, frame) %>%
  summarise(total_att = sum(att),
            max_att = max(att))%>%
  left_join(attention_full) %>%
  mutate(scene = trial - 1)


# Now lets look at the top 10 most different scenes

top_trials <- full_data %>%
  group_by(scene) %>%
  summarise(total_att = sum(total_att),
            max_att = max(max_att)) %>%
  top_n(20, total_att) %>%
  select(scene)

top_att <- full_data %>%
  filter(scene %in% top_trials$scene) %>% 
  ungroup()

top_att %>%
  select(scene, frame, starts_with("tracker")) %>%
  pivot_longer(-c(frame, scene), names_to = "tracker", 
               values_to = "att") %>%
  ggplot(aes(frame, tracker)) + 
  geom_tile(aes(fill = att)) + 
  scale_fill_gradient2(low = muted("blue"), 
                       high = muted("red")) + 
facet_wrap(vars(scene))
ggsave("output/attention_trial_tps.png")

tps <- top_att %>%
  select(scene, frame, starts_with("tracker")) %>%
  pivot_longer(-c(frame, scene), names_to = "tracker", 
               values_to = "att") %>%
  group_by(scene)

max_att <- tps %>%
  slice(which.max(att)) %>%
  separate(tracker, c(NA, "tracker"), "_") %>%
  mutate(tracker = as.numeric(tracker))



min_att <- tps %>%
  slice(which.min(att)) %>%
  separate(tracker, c(NA, "tracker"), "_") %>%
  mutate(tracker = as.numeric(tracker))

time_points <- full_join(max_att, min_att) %>%
  arrange(scene)

scene_shuffle <- time_points %>%
  group_by(scene) %>%
  summarise() %>%
  sample_n(20) %>%
  mutate(unique_id=1:NROW(.))

shuffled_tps <- time_points %>%
  group_by(scene) %>%
  right_join(scene_shuffle) %>%
  arrange(unique_id) %>%
  ungroup()
  
write.csv(shuffled_tps, row.names = FALSE, 
          file = "output/attention_trial_tps.csv")

