sensitivity_df <- data.frame(
  tracker = c(1,2,3,4),
  sensitivity = c(-150, -30, -46, -150))
color_scheme <- tibble(tracker = c(1,2,3,4),
                       Colors = c("#D812E6", "#27BA42", "#394ED4",
                                  "#F7E80F"))

sensitivity_df %>%
  ggplot(aes(x = tracker, y = -1.0 / sensitivity, fill = factor(tracker))) +
  geom_col() +
  scale_fill_manual(values = color_scheme$Colors, aesthetics = "fill") +
  guides(fill = FALSE) +
  xlab("Tracker") +
  ylab("Sensitivity (-1/ln)")

model_perf_df <- data.frame(
  model = c("Base", "TrialAvg", "Attention"),
  accuracy = c(72.1, 74.3, 80.9),
  ymin = c(69.5, 72.0, 78.5),
  ymax = c(74.6, 76.6, 83.3)
)

model_perf_df %>%
  ggplot(aes(x = factor(model, 
                        levels = c("Base", "TrialAvg", "Attention")),
             y = accuracy, fill = model), position = "dodge",
         width = 0.8) + 
  geom_col(show.legend = FALSE) + 
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
                size = 0.9, width = 0.3) +
  xlab("") +
  ylab("Avg Acc.") +
  coord_cartesian(ylim = c(50, 100)) +
  ggtitle("Model Accuracy")

full_data %>%
  ggplot(aes(x = scale(compute), y = accuracy)) +
  geom_point(size = 5, color = "#56B4E9") +
  xlab("Model Compute") +
  ylab("Model Accuracy") 

full_data %>%
  ggplot(aes(x = avg_z_rating, y = avg_acc)) +
  geom_point(size = 5, color = "#56B4E9") +
  xlab("Model Compute") +
  ylab("Model Accuracy")
