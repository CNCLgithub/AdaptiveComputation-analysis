library(tidyverse)
library(estimatr)
library(ggplot2)
library(latex2exp)
library(readr)

# setup

experiment = "exp0_target_designation"

parsed_trials <- read_csv("data/parsed_trials.csv")
parsed_trials <- parsed_trials %>%
  mutate(answer = dot <= 4,
         correct = (Response == answer))
th <- theme_classic() +
  theme(text = element_text(size = 40),
        axis.text = element_text(size = 40),
        legend.text = element_text(size = 40),
        strip.text = element_text(size = 40),
        plot.title = element_text(hjust = 0.5))
theme_set(th)

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


# performance
human_summary <- good_subjects_data %>%
  summarise(
    avg_acc = mean(correct),
    n = n(),
    se_acc = sd(correct) / sqrt(n)
  )
sink("output/human_summary.txt")
print(human_summary)
sink()

acc_summary <- mean(good_subjects_data$correct)
print(acc_summary)

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

png("output/trial_acc_histo.png", width = 800, height = 600)
across_subjects %>%
  ggplot(aes(x=avg_acc)) +
  geom_histogram(bins = 10) +
  xlab("Average Accuracy") +
  ggtitle("Subject perfomance across scenes")
dev.off()

spring_sigma_w_grid <- good_subjects_data %>%
  group_by(ID) %>%
  mutate(z_rating = scale(Rating)) %>%
  group_by(spring, sigma_w) %>%
  summarise(avg_acc = mean(correct),
            avg_z_diff = mean(z_rating),
            avg_diff = mean(Rating),
            n = n(),
            se = sd(correct)/sqrt(n))

png("output/human_acc.png", width = 800, height = 600)
spring_sigma_w_grid %>%
  ggplot(aes(x = spring, y = sigma_w, fill = avg_acc)) +
  geom_tile() +
  labs(fill = "Avg Acc.") + 
  scale_fill_gradient(low = "#0072B2", high = "white") +
  ggtitle("Human Accuracy across parameters")
dev.off()

png("output/human_diff.png", width = 800, height = 600)
spring_sigma_w_grid %>%
  ggplot(aes(x = spring, y = sigma_w, fill = avg_z_diff)) +
  scale_fill_gradient(low = "white", high = "#D55E00")  +
  geom_tile() +
  labs(fill = "Effort (z)") +
  ggtitle("Human Effort across parameters")
dev.off()

model_data <- read_csv(paste("data", experiment, "performance_compute.csv", sep = "/")) %>%
  rename(accuracy = performance) %>%
  select(-contains("dot"))
model_data$scene <- 0:(nrow(model_data) - 1)

model_data %>%
  ggplot(aes(compute)) +
  geom_histogram()

full_data <- merge(across_subjects, model_data) %>%
  mutate(log_compute = scale(compute)) #log(compute + 0.1))



# Human to Human

per_subject_data <- good_subjects_data %>%
  group_by(ID) %>%
  summarise(avg_acc = mean(correct),
            avg_rating = mean(Rating),
            n = n(),
            se = sd(correct)/sqrt(n)) %>%
  ungroup() %>%
  mutate(z_avg_rating = scale(avg_rating))

per_subject_diff <- with(per_subject_data,
                         lm_robust(z_avg_rating ~ avg_acc))

print(summary(per_subject_diff))
per_subject_data %>%
  ggplot(aes(x = z_avg_rating, y = avg_acc)) +
  geom_point(size = 5) +
  stat_smooth(method = "lm_robust") +
  ggtitle("Human Accuracy vs Human Effort",
          subtitle =TeX(sprintf("$R^2=%0.2f$",per_subject_diff$r.squared)))
ggsave("output/per_subject_acc_to_effort.png")



human_acc_to_human_diff <- with(full_data, 
                               lm_robust(avg_z_rating ~ avg_acc))

sink("output/human_acc_to_human_diff.txt")
print(summary(human_acc_to_human_diff))
sink()

png("output/human_acc_to_human_diff.png", width = 800, height = 600)
full_data %>%
  ggplot(aes(x = accuracy, y = avg_acc)) +
  geom_point(size = 5) +
  stat_smooth(method = "lm_robust") +
  xlab("Human Accuracy") +
  ylab("Human Difficulty") + 
  ggtitle("Human Accuracy vs Human Effort",
          subtitle =TeX(sprintf("$R^2=%0.2f$",human_acc_to_human_diff$r.squared)))

dev.off()


# Explaining human accuracy
model_acc_to_human_acc <- with(full_data, 
                lm_robust(avg_acc ~ accuracy))
sink("output/model_acc_to_human_acc.txt")
print(summary(model_acc_to_human_acc))
sink()

png("output/model_acc_to_human_acc.png", width = 800, height = 600)
full_data %>%
  ggplot(aes(x = accuracy, y = avg_acc)) +
  stat_smooth(method = "lm_robust", color = "black", size = 2) +
  geom_point(size = 5, color = "#56B4E9") +
  xlab("Model Accuracy") +
  ylab("Human Accuracy") 
# +
#   ggtitle("Model Accuracy vs Human Accuracy",
          # subtitle =TeX(sprintf("$R^2=%0.2f$",model_acc_to_human_acc$r.squared)))
dev.off()

model_compute_to_human_acc <- with(full_data, 
                lm_robust(avg_acc ~ log_compute))
sink("output/model_compute_to_human_acc.txt")
print(summary(model_compute_to_human_acc))
sink()

png("output/model_compute_to_human_acc.png", width = 800, height = 600)
full_data %>%
  ggplot(aes(y = avg_acc, x = log_compute)) +
  geom_point(size = 5) +
  stat_smooth(method = "lm_robust") +
  xlab("Log Model Compute") +
  ylab("Avg. Human Accuracy") +
  ggtitle("Model Compute vs. Human Accuracy",
          subtitle =TeX(sprintf("$R^2=%0.2f$",model_compute_to_human_acc$r.squared)))
dev.off()

# Explaining Human Difficulty Rating

model_acc_to_human_diff <- with(full_data, 
                                lm_robust(avg_z_rating ~ accuracy))
sink("output/model_acc_to_human_diff.txt")
print(summary(model_acc_to_human_diff))
sink()

png("output/model_acc_to_human_diff.png", width = 800, height = 600)
full_data %>%
  ggplot(aes(y = avg_z_rating, x = accuracy)) +
  geom_point(size = 5) +
  stat_smooth(method = "lm_robust") +
  xlab("Model Accuracy") +
  ylab("Human Effort (z-scored)") +
  ggtitle("Model Accuracy vs. Human Effort",
          )
    
dev.off()


model_compute_to_human_diff <- with(full_data, 
                                       lm_robust(avg_z_rating ~ log_compute))
sink("output/model_compute_to_human_diff.txt")
print(summary(model_compute_to_human_diff))
sink()

png("output/model_compute_to_human_diff.png", width = 800, height = 600)
full_data %>%
  ggplot(aes(x = log_compute, y = avg_z_rating)) +
  stat_smooth(method = "lm_robust", color = "black", size = 2) +
  geom_point(size = 5, color = "#56B4E9") +
  xlab("Log Model Compute") +
  ylab("Human Effort (z-scored)") 
# +
#   ggtitle("Model Compute vs. Human Effort",
#           subtitle =TeX(sprintf("$R^2=%0.2f$",model_compute_to_human_diff$r.squared)))
dev.off()

# Residualized fit

acc_to_compute = with(full_data,
                      lm_robust(log_compute ~ accuracy))
sink("output/acc_to_compute.txt")
print(summary(acc_to_compute))
sink()

png("output/acc_to_compute", width = 800, height = 600)
full_data %>%
  ggplot(aes(x = accuracy, y = log_compute)) +
  geom_point(size = 5) +
  stat_smooth(method = "lm_robust") +
  ylab("Log Model Compute") +
  xlab("Model Acc.") 
# +
#   ggtitle("Model Accuracy vs. Model Compute",
#           subtitle =TeX(sprintf("$R^2=%0.2f$",acc_to_compute$r.squared)))
dev.off()

res_data <- full_data %>%
  mutate(
    res_log_compute = log_compute - acc_to_compute$fitted.values,
    res_avg_z_rating = avg_z_rating - model_acc_to_human_diff$fitted.values
  )


res_model_compute_to_human_diff <- with(res_data,
                                        lm_robust(res_avg_z_rating ~ res_log_compute))

sink("output/res_model_compute_to_human_diff.txt")
print(summary(res_model_compute_to_human_diff))
sink()


res_2sls_fit <- with(full_data,
                     iv_robust(avg_z_rating ~ log_compute | accuracy))
sink("output/res_model_compute_to_human_diff.txt")
print(summary(res_2sls_fit))
sink()


some_fit <- with(full_data,
                 lm_robust(avg_z_rating ~ accuracy + log_compute))
print(summary(some_fit))


png("output/res_model_compute_to_human_diff.png", width = 805, height = 600)
res_data %>%
  ggplot(aes(x = res_log_compute, y = res_avg_z_rating)) +
  stat_smooth(method = "lm_robust", color = "black", size = 2) +
  geom_point(size = 5, color = "#56B4E9") +
  xlab("Residualized Log Model Compute") +
  ylab("Residualized Human Effort") 
# +
#   ggtitle("Res. Compute vs. Res. Effort",
#           subtitle =TeX(sprintf("$R^2=%0.2f$",res_2sls_fit$r.squared)))
dev.off()


# Model Specific analysis

bsci <- function(x,B,ci){
  bstrap <- c()
  for (i in 1:B){
    bstrap <- c(bstrap,mean(sample(x,length(x),replace=T)))
  }
  ret <- quantile(bstrap,ci)
}

print(summarize(full_data,
                mc = mean(compute),
                macc = mean(accuracy)))

png("output/model_acc.png", width = 800, height = 600)
full_data %>%
  ggplot(aes(x = spring, y = sigma_w, fill = accuracy)) +
  geom_tile() +
  labs(fill = "Model Acc") + 
  scale_fill_gradient(low = "#0072B2", high = "white") +
  ggtitle("Model Accuracy across parameters")
dev.off()

png("output/model_compute.png", width = 800, height = 600)
full_data %>%
  ggplot(aes(x = spring, y = sigma_w, fill = log(compute))) +
  scale_fill_gradient(low = "white", high = "#D55E00")  +
  geom_tile() +
  labs(fill = "Log Model Compute") +
  ggtitle("Model Compute across parameters")
dev.off()


############################################################


# Bar plot across models
model_data <- model_data %>%
  mutate(model = "Attention")
accuracy_compute_no_rejuv_avg <- read_csv("accuracy_compute_no_rejuv_avg.csv") %>%
  mutate(model = "Total\nAvg", scene = trial - 1)
accuracy_compute_no_rejuv_trial <- read_csv("accuracy_compute_no_rejuv_trial.csv") %>%
  mutate(model = "Trial\nAvg", scene = trial - 1)
accuracy_compute_no_rejuv_base <- read_csv("accuracy_compute_no_rejuv_base.csv") %>%
  mutate(model = "Base", scene = trial - 1)

all_model_data <- bind_rows(
  model_data,
  accuracy_compute_no_rejuv_avg,
  accuracy_compute_no_rejuv_trial,
  accuracy_compute_no_rejuv_base,
                        ) %>%
  mutate(log_compute = log(compute))

model_accuracy_table <- all_model_data %>%
  group_by(model) %>%
  summarise(avg_acc = mean(accuracy),
            n = n(),
            se = sd(accuracy) / sqrt(n),
            conf.low = bsci(accuracy, 1000, 0.024),
            conf.high = bsci(accuracy, 1000, 0.975))
  
sink("output/model_accuracy_table.txt")
print(model_accuracy_table)
sink()

png("output/model_accuracy_exp0.png", width = 400, height = 600)
model_accuracy_table %>%
  ggplot(aes(model, avg_acc, fill = model), position = "dodge",
         width = 0.8) + 
  geom_col(show.legend = FALSE) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                size = 0.9, width = 0.3) +
  xlab("") +
  ylab("Avg Acc.") +
  coord_cartesian(ylim = c(0.5, 0.9)) +
  ggtitle("Model Accuracy")
dev.off()


## Model correlations



base_fit <- all_model_data %>%
  filter(model == "Base") %>%
  merge(across_subjects, .) %>%
  with(.,
       lm_robust(avg_z_rating ~ accuracy))
print(summary(base_fit))

total_avg_fit <- all_model_data %>%
  filter(model == "Total Avg") %>%
  merge(across_subjects, .) %>%
  with(.,
       lm_robust(avg_z_rating ~ accuracy))
print(summary(total_avg_fit))

trial_avg_fit <- all_model_data %>%
  filter(model == "Trial Avg") %>%
  merge(across_subjects, .) %>%
  with(.,
       lm_robust(avg_z_rating ~ accuracy))
print(summary(trial_avg_fit))

model_r2 <- data.frame(
  model = c("Base", "Total Avg", "Trial Avg", "Attention"),
  r2 = c(base_fit$r.squared,
         total_avg_fit$r.squared,
         trial_avg_fit$r.squared,
         model_acc_to_human_diff$r.squared)
)



loc_error_no_rejuv_avg <- read_csv("loc_error_no_rejuv_avg.csv") %>%
  mutate(model = "Total Avg")
loc_error_rejuv <- read_csv("loc_error_rejuv.csv") %>%
  mutate(model = "Attention")
loc_error_no_rejuv_base <- read_csv("loc_error_no_rejuv_base.csv") %>%
  mutate(model = "Base")
loc_error_no_rejuv_trial <- read_csv("loc_error_no_rejuv_trial.csv") %>%
  mutate(model = "Trial Avg")



loc_error <- rbind(loc_error_no_rejuv_avg, 
                   loc_error_rejuv,
                   loc_error_no_rejuv_base,
                   loc_error_no_rejuv_trial) %>%
  group_by(model, distances_nd) %>%
  summarise(
    mu = mean(localization_error),
    n = n(),
    conf.low = bsci(localization_error, 1000, 0.025),
    conf.high = bsci(localization_error, 1000, 0.975)

  )

png("output/model_loc_error.png", width = 600, height = 600)
loc_error %>%
  ggplot(aes(x = distances_nd, y = mu, color = model)) +
  geom_line(linetype = "solid", size = 1.5, show.legend = FALSE) +
  geom_point(show.legend = FALSE) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 2,
                 show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 150, by = 30)) +
  xlab("NN Dis. (pixels)") +
  ylab("Avg. Loc. Error (pixels)") +
  ggtitle("Loc. Error vs NN")
dev.off()

ttest <- rbind(loc_error_no_rejuv_avg, 
               loc_error_rejuv,
               loc_error_no_rejuv_base,
               loc_error_no_rejuv_trial) %>%
  filter(distances_nd == 9.375) %>%
  filter(model %in% c("Attention", "Trial Avg")) %>%
  t.test(localization_error ~ model, .)

sink("output/attention_trial_avg_ttest.txt")
print(ttest)
sink()

# iordanescu data

iordanescu_data <- read_csv("iordanescu_data.csv")
png("output/iordanescu_loc_error.png", width = 600, height = 600)
iordanescu_data %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "solid", size = 1.5) +
  scale_linetype_identity() +
  geom_point() + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 2) +
  scale_x_continuous(breaks = seq(0, 3.5, by = 0.5)) +
  xlab("NN Dis. (degrees)") +
  ylab("Avg. Loc. Error (degrees)") +
  ggtitle("Iordanescu 2016")
dev.off()

