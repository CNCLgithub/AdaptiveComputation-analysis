
library(estimatr)
library(ggplot2)
library(readr)
library(scales)
library(raster)
library(minpack.lm)
library(tidyverse)
library(pracma)

th <- theme_classic()
theme_set(th)

attention_full <- read_csv("data/isr_inertia_extended_target_designation_attention.csv") %>%
rename(scene = trial) %>%
mutate(frame = t + 2) %>%
select(-t)


full_data <- attention_full %>%
pivot_longer(-c(frame, scene), names_to = "tracker", 
             values_to = "att")  %>%
separate(tracker, c(NA, "tracker"), sep = '_') %>%
mutate(tracker = as.numeric(tracker),
       zatt = scale(att))

probe_limit = 48
window_size = 12
max_peaks = 3
tmax = max(full_data$frame)
n_trackers = length(unique(full_data$tracker))

scene_peaks <- function(att) {
matt <- findpeaks(att, npeaks = max_peaks, minpeakheight = 1,
                  minpeakdistance = window_size * n_trackers)
return(matt[, 2])
}
# take the highs for each scene with at least some buffer between
tps <- full_data %>%
filter(between(frame, probe_limit, tmax-probe_limit)) %>%
nest_by(scene) %>%
mutate(frame = list(scene_peaks(data$att))) %>%
select(-data) %>%
unnest(cols = c(frame)) %>%
mutate(frame = round(frame / n_trackers) + probe_limit) %>%
left_join(full_data) %>%
arrange(scene, frame)


scene_bd <- tps %>%
group_by(scene, frame) %>%
summarise(att = max(att))


scene_bd %>%
ggplot(aes(frame)) +
geom_density()

full_data %>%
filter(scene %in% att_map$scene) %>%
mutate(zatt = scale(att)) %>%
ggplot(aes(frame, tracker)) +
geom_tile(aes(fill = zatt)) +
scale_fill_gradient2(low = muted("blue"),
                     high = muted("red")) +
facet_wrap(vars(scene))
# ggsave("output/attention_trial_tps.png")

att_map <- tps %>%
group_by(scene, frame) %>%
mutate(att_rank = dense_rank(att)) %>%
ungroup() %>%
filter(att_rank== 1 | att_rank == 4) %>%
arrange(scene, frame, att)

att_map %>%
ggplot(aes(att)) +
xlim(0, 13) +
geom_density(binwidth = 0.3)


write.csv(att_map, row.names = FALSE,
        file = "output/isr_inertia_probe_map.csv")

