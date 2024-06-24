# Load necessary libraries
library(readxl)
library(dplyr)

# Read the data
correlation <- read_xlsx("correlation.xlsx")


# Display the first few rows of the data
head(correlation)

# Length 
cor.test(correlation$`A. flavum (mean length)`, correlation$WTD, method="spearman")
cor.test(correlation$`A. muscorum (mean length)`, correlation$WTD, method="spearman")
cor.test(correlation$`D. pulex (mean length)`, correlation$WTD, method="spearman")

# Width
cor.test(correlation$`A. flavum (mean width)`, correlation$WTD, method="spearman")
cor.test(correlation$`A. muscorum (mean width)`, correlation$WTD, method="spearman")
cor.test(correlation$`D. pulex (mean width)`, correlation$WTD, method="spearman")

# Aperture
cor.test(correlation$`A. flavum (mean aperture)`, correlation$WTD, method="spearman")
cor.test(correlation$`A. muscorum (mean aperture)`, correlation$WTD, method="spearman")
cor.test(correlation$`D. pulex (mean aperture)`, correlation$WTD, method="spearman")


