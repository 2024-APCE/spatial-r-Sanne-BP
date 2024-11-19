#MADE WITH CHAT-GTP

# Load ggplot2
library(ggplot2)

# Define node positions (left-to-right hierarchy)
nodes <- data.frame(
  name = c("Rainfall", "Elevation", "Dist2River", "BurnFreq", "CEC", "TreeCover", "WoodyCover"),
  x = c(1, 1, 1, 2, 2, 2, 3), # x coordinates
  y = c(5, 4, 3, 4, 3, 2, 3)  # y coordinates
)

# Define edges (relationships)
edges <- data.frame(
  from = c("Rainfall", "Rainfall", "Elevation", "Elevation", 
           "Dist2River", "BurnFreq", "CEC", "TreeCover", 
           "BurnFreq", "CEC", "Rainfall", "Rainfall"),
  to = c("CEC", "TreeCover", "Rainfall", "CEC", 
         "WoodyCover", "WoodyCover", "TreeCover", "WoodyCover", 
         "TreeCover", "WoodyCover", "BurnFreq", "WoodyCover"),
  effect = c("Positive", "Positive", "Positive", "Positive", 
             "Positive", "Negative", "Positive", "Positive", 
             "Negative", "Positive", "Negative", "Positive")
)

# Merge edges with node coordinates
edges <- merge(edges, nodes, by.x = "from", by.y = "name")
edges <- merge(edges, nodes, by.x = "to", by.y = "name", suffixes = c("_from", "_to"))

# Create the plot
ggplot() +
  # Add nodes
  geom_point(data = nodes, aes(x = x, y = y), size = 5, color = "blue") +
  geom_text(data = nodes, aes(x = x, y = y, label = name), vjust = -1) +
  # Add arrows for edges
  geom_curve(data = edges, aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                               color = effect), curvature = 0.2, arrow = arrow(length = unit(0.3, "cm"))) +
  # Customize colors
  scale_color_manual(values = c("Positive" = "green", "Negative" = "red")) +
  # Add labels and theme
  labs(title = "Causal Web for Woody Cover Determinants (Left-to-Right Hierarchy)",
       color = "Effect") +
  theme_minimal()




#NEW MODEL
# Load necessary libraries
library(ggplot2)
library(ggforce)

# Create a data frame with relationships
causal_data <- data.frame(
  From = c("rainfall", "treecover", "burnfreq", "evaporation", "cec", "dist2river", "elevation", "elevation"),
  To = c("woody", "woody", "woody", "woody", "treecover", "woody", "treecover", "rainfall"),
  Type = c("Positive", "Positive", "Negative", "Negative", "Positive", "Positive", "Positive", "Positive")
)

# Define the positions for nodes (hierarchy: left to right with woody on the far right)
nodes <- data.frame(
  Name = unique(c(causal_data$From, causal_data$To)),
  x = c(0, 1, 2, 2, 1, 0, 0, 3),  # X-coordinates for hierarchy
  y = c(4, 4, 3, 2, 3, 2, 0, 3)   # Y-coordinates for neat layout
)

# Merge coordinates into the causal data
causal_data <- merge(causal_data, nodes, by.x = "From", by.y = "Name", all.x = TRUE)
colnames(causal_data)[c(4, 5)] <- c("xstart", "ystart")
causal_data <- merge(causal_data, nodes, by.x = "To", by.y = "Name", all.x = TRUE)
colnames(causal_data)[c(6, 7)] <- c("xend", "yend")

# Plot the causal web
ggplot() +
  geom_segment(data = causal_data, aes(x = xstart, y = ystart, xend = xend, yend = yend, color = Type),
               arrow = arrow(length = unit(0.2, "cm")), size = 1) +
  geom_point(data = nodes, aes(x = x, y = y), size = 5, color = "black") +
  geom_text(data = nodes, aes(x = x, y = y, label = Name), vjust = -1, size = 5) +
  scale_color_manual(values = c("Positive" = "green", "Negative" = "red")) +
  theme_void() +
  ggtitle("Causal Web of Woody Cover Drivers (7 Variables)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
