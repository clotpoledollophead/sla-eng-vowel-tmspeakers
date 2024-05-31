# Install necessary packages if not already installed
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

# Load necessary libraries
library(writexl)
library(ggplot2)
library(grid)

# Read CSV file
g3 <- read.csv(file.choose(), stringsAsFactors = TRUE)

# Display summary
print(summary(g3))

# Modify the phoneme column
g3$phoneme <- factor(g3$phoneme, labels = c("æ", "æ", "ɛ", "e"))
g3$phoneme <- factor(g3$phoneme, levels = c("e", "ɛ", "æ"))

# Create a summary table
summary_table <- summary(g3)

# Convert the summary table to a data frame if necessary
summary_df <- as.data.frame(summary_table)

# Export the summary table to an Excel file
write_xlsx(summary_df, path = "summary_table.xlsx")

# Plot F1_midpoint by phoneme
f1_plot <- ggplot(data = g3, aes(x = phoneme, y = F1_midpoint, color = frequency)) + 
  geom_boxplot(notch = TRUE)

# Plot F2_midpoint by phoneme
f2_plot <- ggplot(data = g3, aes(x = phoneme, y = F2_midpoint, color = frequency)) + 
  geom_boxplot(notch = TRUE)

# Multiple plot function
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  plots <- c(list(...), plotlist)
  numPlots <- length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                     ncol = cols, nrow = ceiling(numPlots / cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Display multiple plots
multiplot(f1_plot, f2_plot)

# Aggregated mean of F1_midpoint
f1_val <- aggregate(F1_midpoint ~ phoneme * frequency, g3, mean)

# Perform t-tests
t.test(F1_midpoint ~ frequency, g3)
g3_E <- subset(g3, phoneme == "ɛ")
t.test(F1_midpoint ~ frequency, g3_E)
t.test(F2_midpoint ~ frequency, g3)
t.test(F2_midpoint ~ frequency, g3_E)

# Histogram of F1_midpoint
hist(g3$F1_midpoint)

# Scatter plot with text labels
ggplot(data = g3, aes(x = F1_midpoint, y = F2_midpoint, label = phoneme, color = phoneme, fill = frequency)) + 
  geom_text() + 
  scale_x_reverse() + 
  guides(color = FALSE) + 
  xlab("mean_F2") + 
  ylab("mean_F1") + 
  theme_bw(base_size = 12) + 
  facet_grid(~frequency) + 
  theme(legend.position = "top")

# Scatter plot with ellipses
ggplot(g3, aes(x = F2_midpoint, y = F1_midpoint, color = phoneme, label = phoneme)) + 
  geom_point() + 
  stat_ellipse() + 
  scale_x_reverse() + 
  scale_y_reverse() + 
  scale_color_discrete() + 
  theme_minimal() + 
  facet_grid(~frequency) + 
  theme(legend.position = "top")
