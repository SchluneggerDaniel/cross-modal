# Plot psychometric curves

library(tidyverse)
library(viridis)
library(patchwork)
library(ggtext)

### === Create data === ###

# Define the psychometric function
psychometric_function <- function(x, slope, x0) {
  1 / (1 + exp(-slope * (x - x0)))
}

# Define the stimulus levels, negative means A, positive means B
stimulus_levels <- seq(-9, 9, length.out = 100)

# Define the slopes and mean shifts
slopes <- c(0.5, 1, 2, 3)
means <- c(-2, -1, -0.9, -0.1, 0, 0.1, 0.9, 1, 2)

# Initialize an empty list to store the tibbles
tibble_list <- list()

# Function to create a safe name
safe_name <- function(value) {
  if (value < 0) {
    return(paste("minus", abs(value), sep = ""))
  } else {
    return(as.character(value))
  }
}

# Generate tibbles for each combination of slope and mean
for (slope in slopes) {
  for (mean in means) {
    mean_name <- safe_name(mean)
    condition_name <- paste("slope", slope, "_x", mean_name, sep = "")
    tibble_list[[condition_name]] <- tibble(
      stimulus = stimulus_levels,
      response = psychometric_function(stimulus_levels, slope, mean),
      condition = condition_name
    )
  }
}


# Combine all data into one tibble
combined_data <- bind_rows(tibble_list, .id = "condition")

### === Create plots === ###

# Plot 1:

# Select the conditions you want to plot
selected_conditions_p1 <- c("slope0.5_x0", "slope1_x0")

# Filter the data for only those conditions
filtered_data_p1 <- combined_data |>  
  filter(condition %in% selected_conditions_p1)

# Plot 1: Same mean, different slope
p1 <- ggplot(filtered_data_p1, aes(x = stimulus, y = response, color = condition)) +
  geom_line() +
  theme_classic() + 
  xlab("") +
  ylab("P(<i>B</i>)") +
  ylim(0, 1) +
  labs(title = "A", subtitle = "Same preceding modality") +
  scale_color_manual(name = "Condition", 
                     labels = c("Different preceding \nstimulus identity", "Same preceding \nstimulus identity"),
                     values = c("#5DC863FF", "#3B528BFF")) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = c(0.95, 0.05),
        plot.subtitle = element_text(size=10),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.justification = c("right", "bottom")) 


p1




# Plot 2:

# Select the conditions you want to plot
selected_conditions_p2 <- c("slope0.5_xminus0.1", "slope0.5_x0.1")

# Filter the data for only those conditions
filtered_data_p2 <- combined_data |>  
  filter(condition %in% selected_conditions_p2)


p2 <- ggplot(filtered_data_p2, aes(x = stimulus, y = response, color = condition)) +
  geom_line() +
  theme_classic() + 
  xlab("") +
  ylab("") +
  ylim(0, 1) +
  labs(title = "B", subtitle = "Different preceding modality") +
  scale_color_manual(name = "Condition", 
                     labels = c("Different preceding \nstimulus identity", "Same preceding \nstimulus identity"),
                     values = c("#5DC863FF", "#3B528BFF")) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = c(0.95, 0.05),
        plot.subtitle = element_text(size=10),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.justification = c("right", "bottom")) 

p2


# Plot 3:
selected_conditions_p3 <- c("slope1_xminus1", "slope1_xminus0.1", "slope1_x0.1", "slope1_x1")

# Filter the data for only those conditions
filtered_data_p3 <- combined_data |>  
  filter(condition %in% selected_conditions_p3) |> 
  mutate(condition = as_factor(condition))

# Add a new column to specify line types
filtered_data_p3 <- filtered_data_p3 %>%
  mutate(linetype = case_when(
    condition %in% c("slope1_xminus0.1", "slope1_x0.1") ~ "solid", TRUE ~ "dashed"))


p3 <- ggplot(filtered_data_p3, aes(x = stimulus, y = response, color = condition, linetype = linetype)) +
  geom_line() +
  theme_classic() + 
  xlab("Stimulus strength") +
  ylab("P(<i>B</i>)") +
  ylim(0, 1) +
  labs(title = "C", subtitle = "Serial dependence within the same modality") +
  scale_color_manual(name = "Condition", 
                     labels = c("Preceding response \nsame modality B", 
                                "Preceding response \ndifferent modality B",
                                "Preceding response \ndifferent modality A",
                                "Preceding response \nsame modality A"),
                     values = c("#5DC863FF", "#5DC863FF","#3B528BFF","#3B528BFF")) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = c(0.95, 0.05),
        plot.subtitle = element_text(size=10),
        axis.title = element_text(size=12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.justification = c("right", "bottom")) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed", "solid"))),
         linetype = "none") # Remove the linetype legend

p3






# Plot 4:
selected_conditions_p4 <- c("slope1_xminus1", "slope1_xminus0.9", "slope1_x0.9", "slope1_x1")

# Filter the data for only those conditions
filtered_data_p4 <- combined_data |>  
  filter(condition %in% selected_conditions_p4) |> 
  mutate(condition = as_factor(condition))

# Add a new column to specify line types
filtered_data_p4 <- filtered_data_p4 %>%
  mutate(linetype = case_when(
    condition %in% c("slope1_xminus0.9", "slope1_x0.9") ~ "solid", TRUE ~ "dashed"))


p4 <- ggplot(filtered_data_p4, aes(x = stimulus, y = response, color = condition, linetype = linetype)) +
  geom_line() +
  theme_classic() + 
  xlab("Stimulus strength") +
  ylab("") +
  ylim(0, 1) +
  labs(title = "D", subtitle = "Sequential choice effects across modalities") +
  scale_color_manual(name = "Condition", 
                     labels = c("Preceding response \nsame modality B", 
                                "Preceding response \ndifferent modality B",
                                "Preceding response \ndifferent modality A",
                                "Preceding response \nsame modality A"),
                     values = c("#5DC863FF", "#5DC863FF","#3B528BFF","#3B528BFF")) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = c(0.95, 0.05),
        plot.subtitle = element_text(size=10),
        axis.title = element_text(size=12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.justification = c("right", "bottom")) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed", "solid"))),
         linetype = "none") # Remove the linetype legend

p4




 # Patchwork for publication
plotPsychometric <- (p1 | p2 | p3 | p4) +
  plot_layout(guides = "auto", nrow = 2)  +
  plot_annotation()


plotPsychometric


ggsave(filename = "Figure1.png",
       plot = plotPsychometric,
       width = 20,
       height = 18,
       path = "/Users/daniel.schlunegger/Library/CloudStorage/Dropbox/LaTeX/Thesis/images/",
       dpi = 300,
       bg = "transparent",
       units = "cm")
















