library(tidyverse)
library(estimatr)
library(ggplot2)
library(readr)
library(scales)

th <- theme_classic()
theme_set(th)

attention_full <- read_csv("data/attention.csv") %>%
  rename(frame = t,
         scene = trial) %>%
  filter(between(frame, 24, 108))

full_data <- attention_full %>%
  pivot_longer(-c(frame, scene), names_to = "tracker", 
               values_to = "att") %>%
  group_by(scene, frame) %>%
  summarise(total_att_t = sum(att),
            max_att_t = max(att))%>%
  left_join(attention_full)


# Now lets look at the top 10 most different scenes

top_trials <- full_data %>%
  group_by(scene) %>%
  summarise(total_att = sum(total_att_t),
            max_att = max(max_att_t),
            var_total_att = sd(total_att_t)) %>%
  top_n(20, var_total_att) %>%
  select(scene)

top_att <- full_data %>%
  filter(scene %in% top_trials$scene) %>% 
  ungroup()

top_att %>%
  ggplot(aes(x = frame, y = total_att_t)) + 
  geom_col() + 
  facet_wrap(vars(scene))
ggsave("output/compute_trial_tps.png")

max_att <- top_att %>%
  group_by(scene) %>%
  slice(which.max(total_att_t))

min_att <- top_att %>%
  group_by(scene) %>%
  slice(which.min(total_att_t))

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
          file = "output/compute_trial_tps.csv")

