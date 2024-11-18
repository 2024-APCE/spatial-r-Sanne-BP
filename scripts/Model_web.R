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
