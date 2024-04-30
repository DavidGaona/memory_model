# Load required libraries using pacman
pacman::p_load(data.table, ggplot2, magick, ggpubr, gganimate, scales)

raw_data <- fread("src/data/runs/run_2024-04-19_10-59-33_609/Agent17580.csv")
# src/data/runs/run_2024-04-18_05-41-14_531/Agent1.csv
#raw_data[, isSpeaking := factor(isSpeaking, labels = c("Silent", "Speaking"))]
#raw_data[, agentName := factor(agentName)]

# getting the last time each agent spoke
setorder(raw_data, agentName, -round)

# Find the last round where 'isSpeaking' is TRUE for each agent
last_speaking <- raw_data[isSpeaking == TRUE, .SD[1], by = agentName]

# Optionally, reorder by 'agentName' if needed
setorder(last_speaking, agentName)


setorder(raw_data, round, agentName)
raw_data[, speaking_shape := factor(as.numeric(!isSpeaking) + 1)]

ggplot(raw_data[round < 75], aes(x = round, y = belief, group = agentName)) +
  geom_line(aes(color = agentName)) +  # Use linetype to indicate speaking status
  geom_point(aes(color = agentName, shape = speaking_shape), size = 3) +  # Add points with color by agent
  scale_y_continuous(limits = c(0, 1)) + # Ensure y-axis is between 0 and 1
  scale_color_manual(values = rainbow(length(unique(raw_data$agentName)))) + # Assign a unique color to each agent
  scale_shape_manual(values = c(16, 17), labels = c("Speaking", "Silent")) +  # Custom shapes with labels
  labs(title = "Evolution of Beliefs Over Time",
       x = "Round",
       y = "Belief",
       color = "Agent Name",
       shape = "Speaking Status") +
  theme_minimal() +
  theme(legend.position = "right")

# tol rad 0.25
# 2 out of 25k
# Directory created successfully at src/data/runs/run_2024-04-19_10-59-33_609
# Did no reach consensus Agent17580
# Did no reach consensus Agent16383
