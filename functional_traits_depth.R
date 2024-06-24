library(tidyverse)
library(readxl)
library(openxlsx)
library(ggplot2)
library(gridExtra)

# Read the data from Excel file
f_traits <- read_xlsx("Functional_traits_mean.xlsx")

summary(f_traits)


# Function to create line plots for each trait using Depth for x-axis
create_line_plot <- function(data, trait, title, y_label) {
  ggplot(data, aes(x = Depth, y = !!sym(trait), group = 1)) +
    geom_line() +
    theme_bw() +
    geom_point(size = 0.5) +
    coord_flip() +
    labs(title = title, x = "Depth (cm)", y = y_label)+
    scale_x_reverse(breaks = seq(min(data$Depth, na.rm = TRUE), max(60, na.rm = TRUE), by = 4))
}

# Line plot for A.flavum mean length
plot_flavum_length <- create_line_plot(f_traits, "A. flavum (mean length)", "A. flavum", "Mean Length (µm)")

# Line plot for A.muscorum mean length
plot_muscorum_length <- create_line_plot(f_traits, "A. muscorum (mean length)", "A. muscorum", "Mean Length (µm)")

# Line plot for D. pulex mean length
plot_pulex_length <- create_line_plot(f_traits, "D. pulex (mean length)", "D. pulex", "Mean Length (µm)")

# Line plot for A.flavum mean width
plot_flavum_width <- create_line_plot(f_traits, "A. flavum (mean width)", "A. flavum", "Mean Width (µm)")

# Line plot for A.muscorum mean width
plot_muscorum_width <- create_line_plot(f_traits, "A. muscorum (mean width)", "A. muscorum", "Mean Width (µm)")

# Line plot for D. pulex mean width
plot_pulex_width <- create_line_plot(f_traits, "D. pulex (mean width)", "D. pulex", "Mean Width (µm)")

# Line plot for A.flavum mean aperture
plot_flavum_aperture <- create_line_plot(f_traits, "A. flavum (mean aperture)", "A. flavum", "Mean Aperture (µm)")

# Line plot for A.muscorum mean aperture
plot_muscorum_aperture <- create_line_plot(f_traits, "A. muscorum (mean aperture)", "A. muscorum", "Mean Aperture (µm)")

# Line plot for D. pulex mean aperture
plot_pulex_aperture <- create_line_plot(f_traits, "D. pulex (mean aperture)", "D. pulex", "Mean Aperture (µm)")

# Arrange all plots together
m_length <- grid.arrange(plot_flavum_length, plot_muscorum_length, plot_pulex_length,
                         ncol = 3)

m_width <- grid.arrange(plot_flavum_width, plot_muscorum_width, plot_pulex_width,
                        ncol = 3)

m_aperture <- grid.arrange(plot_flavum_aperture, plot_muscorum_aperture, plot_pulex_aperture,
                           ncol = 3)

ggsave("mean_length_depth.png", plot = m_length, width = 12, height = 8, dpi = 300)
ggsave("mean_width_depth.png", plot = m_width, width = 12, height = 8, dpi = 300)
ggsave("mean_aperture_depth.png", plot = m_aperture, width = 12, height = 8, dpi = 300)

# Define a custom function to manually calculate z-scores
calculate_z_scores <- function(f_traits) {
  # Extract the sample names
  sample_names <- f_traits[, 1]
  
  # Store the original column names (excluding the sample names)
  original_colnames <- colnames(f_traits)[-1]
  
  # Apply a custom function to calculate z-scores manually to each numeric column
  z_scores <- as.data.frame(lapply(f_traits[, -1], function(x) {
    mean_x <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)
    z <- (x - mean_x) / sd(x, na.rm = TRUE)
    return(z)
  }))
  
  # Set the column names of the z-scores dataframe to the original names
  colnames(z_scores) <- original_colnames
  
  # Combine the sample names with the z-scores
  z_scores <- cbind(Sample_name = sample_names, z_scores)
  
  return(z_scores)
}

z_scores_traits <- calculate_z_scores(f_traits)

# Convert Depth to numeric if it's not already
z_scores_traits$Depth <- as.numeric(f_traits$Depth)

# Print the resulting dataframe with z-scores
print(z_scores_traits)

# Function to create line plots for each trait using Depth for x-axis
create_line_plot <- function(data, trait, title, y_label) {
  data <- data %>%
    drop_na(Depth, !!sym(trait))
  
  ggplot(data, aes(x = Depth, y = !!sym(trait), group = 1)) +
    geom_line() +
    geom_point(size = 0.5) +
    theme_bw() +
    coord_flip() +
    labs(title = title, x = "Depth (cm)", y = y_label) +
    scale_x_reverse(breaks = seq(0, 60, by = 4), limits = c(60, 0))
}

# Line plot for A. flavum mean length
z_flavum_length <- create_line_plot(z_scores_traits, "A. flavum (mean length)", "A. flavum", "Z-score for Mean Length")

# Line plot for A. muscorum mean length
z_muscorum_length <- create_line_plot(z_scores_traits, "A. muscorum (mean length)", "A. muscorum", "Z-score for Mean Length")

# Line plot for D. pulex mean length
z_pulex_length <- create_line_plot(z_scores_traits, "D. pulex (mean length)", "D. pulex", "Z-score for Mean Length")

# Line plot for A. flavum mean width
z_flavum_width <- create_line_plot(z_scores_traits, "A. flavum (mean width)", "A. flavum", "Z-score for Mean Width")

# Line plot for A. muscorum mean width
z_muscorum_width <- create_line_plot(z_scores_traits, "A. muscorum (mean width)", "A. muscorum", "Z-score for Mean Width")

# Line plot for D. pulex mean width
z_pulex_width <- create_line_plot(z_scores_traits, "D. pulex (mean width)", "D. pulex", "Z-score for Mean Width")

# Line plot for A. flavum mean aperture
z_flavum_aperture <- create_line_plot(z_scores_traits, "A. flavum (mean aperture)", "A. flavum", "Z-score for Mean Aperture")

# Line plot for A. muscorum mean aperture
z_muscorum_aperture <- create_line_plot(z_scores_traits, "A. muscorum (mean aperture)", "A. muscorum", "Z-score for Mean Aperture")

# Line plot for D. pulex mean aperture
z_pulex_aperture <- create_line_plot(z_scores_traits, "D. pulex (mean aperture)", "D. pulex", "Z-score for Mean Aperture")

# Arrange all plots together
z_length <- grid.arrange(z_flavum_length, z_muscorum_length, z_pulex_length, ncol = 3)
z_width <- grid.arrange(z_flavum_width, z_muscorum_width, z_pulex_width, ncol = 3)
z_aperture <- grid.arrange(z_flavum_aperture, z_muscorum_aperture, z_pulex_aperture, ncol = 3)

# Save the plots
ggsave("mean_length_z_depth.png", plot = z_length, width = 12, height = 8, dpi = 300)
ggsave("mean_width_z_depth.png", plot = z_width, width = 12, height = 8, dpi = 300)
ggsave("mean_aperture_z_depth.png", plot = z_aperture, width = 12, height = 8, dpi = 300)