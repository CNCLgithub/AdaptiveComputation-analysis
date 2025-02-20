library(ggplot2)
library(readr)
library(tidyverse)

th <- theme_classic()
theme_set(th)

# cycles per frame
# scene, frame, tracker, cycles, chain
model_path = "project/data/probes/complete_overlap.csv"
# model_path = "project/data/probes/exp_probes_id_att.csv" # uncomment for id
model_att <- read_csv(model_path) %>%
  filter(is.finite(nabla)) %>%
  filter(between(t, 0, 54)) %>%
mutate(tbin = cut_interval(t, 10)) %>%
  group_by(tbin) %>%
  summarise(mu = mean(nabla),
            sd = sd(nabla),
            t = mean(t),
            se = sd / sqrt(n()))

fig <- model_att %>%
  ggplot(aes(x = t, y = mu)) +
  geom_line() + 
  geom_ribbon(aes(ymin = mu - se, ymax = mu + se),
              alpha = 0.4) +
  geom_vline(xintercept = 26)

fig
ggsave("project/figures/overal_example.svg", fig,
       width = 500, height = 100, units = "mm")
