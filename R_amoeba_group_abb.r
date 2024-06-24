library(tidyverse)
library(readxl)
library(openxlsx)

amoeba <- read_excel("Store_mosse_amoeba_table.xlsx")

species_groups_abb <-c("Alabasta militaris" = "NEB MIL",
                   "Amphitrema wrightianum" = "ARC WRI",
                   "Arcella hemispherica" = "ARC HEM",
                   "Archerella flavum" = "ARC FLA",
                   "Argynnia dentistoma" = "ARG DEN",
                   "Assulina muscorum" = "ASS MUS",
                   "Assulina seminulum" = "ASS SEM",
                   "Valkanovia elegans" = "ASS MUS",
                   "Bullinularia indica" = "BUL IND",
                   "Centropyxis aculeata type" = "CEN ACU",
                   "Centropyxis aerophila" = "CEN AER",
                   "Centropyxis cassis type" = "CEN AER",
                   "Centropyxis laevigata" = "CEN ECO",
                   "Centropyxis euristoma" = "CYC ARC",
                   "Centropyxis ecornis type" = "CEN ECO",
                   "Cryptodifflugia compressa" = "CRY OVI",
                   "Cryptodifflugia crenulata" = "CRY CRE",
                   "Cryptodifflugia minuta" = "CRY OVI",
                   "Cryptodifflugia oviformis" = "CRY OVI",
                   "Cryptodifflugia sacculus" = "CRY SAC",
                   "Cryptodifflugia operculata" = "CRY OPE",
                   "Cyclopyxis arcelloides type" = "CYC ARC",
                   "Cyclopyxis eurystoma" = "CYC ARC",
                   "Cyclopyxis kahli" = "CYC ARC",
                   "Centropyxis platystoma type" = "CEN AER",
                   "Difflugia lucida type" = "DIF LUC",
                   "Difflugia globulosa type" = "CYC ARC",
                   "Difflugia pristis type" = "DIF LUC",
                   "Difflugia pulex" = "CRY SAC",
                   "Difflugia rubescens" = "DIF OBL",
                   "Eughlypha ciliata" = "EUG CIL",
                   "Euglypha compressa" = "EUG CIL",
                   "Euglypha laevis" = "EUG ROT",
                   "Euglypha strigosa" = "EUG CIL",
                   "Euglypha tuberculata type" = "EUG CIL",
                   "Euglypha rotunda type" = "EUG ROT",
                   "Galeripora artocrea" =  "ARC ARE",
                   "Galeripora catinus" = "ARC ARE",
                   "Galeripora discoides type" = "ARC DIS",
                   "Habrotrocha angusticollis" = "HAB ANG",
                   "Heleopera petricola" = "HEL PET",
                   "Heleopera rosea" = "HEL ROS",
                   "Heleopera sphagni" = "HEL PET",
                   "Heleopera sylvatica" = "HEL SYL",
                   "Hyalosphenia elegans" = "HEL ELE",
                   "Hyalosphenia minuta" = "CRY SAC",
                   "Hyalosphenia papilio" = "HYA PAP",
                   "Hyalosphenia subflava" = "HYA SUB",
                   "Nebela collaris type" = "NEB COLL",
                   "Nebela bohemica" = "NEB TIN",
                   "Nebela flabellulum" = "NEB FLA",
                   "Nebela tincta" = "NEB TIN",
                   "Nebela carinata" = "PLA CAR",
                   "Nebela tincta galeata type" = "PLA CAR",
                   "Padaungiella wailesi type" = "PAD LAG",
                   "Phryganella acropodia" = "CYC ARC",
                   "Phryganella hemisphaerica" = "CYC ARC",
                   "Phryganella dissimulatoris" = "CYC ARC",
                   "Phryganella paradoxa" = "CYC ARC",
                   "Physochila griseola" = "PHY GRI",
                   "Placocista spinosa" = "PLA SPI",
                   "Pseudodifflugia fulva" = "DIF LUC",
                   "Pyxidicula operculata" = "PYX OPE",
                   "Tracheleuglypha dentata" = "TRA DEN",
                   "Trigonopyxis arcula type" = "TRI ARC",
                   "Trigonopyxis minuta type" = "TRI ARC",
                   "Trinema complanatum" = "COR TRI",
                   "Trinema lineare type" = "COR TRI",
                   "Corythion dubium" = "COR TRI",
                   "Wailasella erboracensis" = "WAI ERB"
)

# Identify numeric columns
numeric_cols <- sapply(amoeba, is.numeric)

# Replace NA values with 0 only in numeric columns
amoeba[numeric_cols][is.na(amoeba[numeric_cols])] <- 0


# Create an empty data frame with the same number of rows as amoeba
grouped_amoeba_abb <- data.frame(matrix(ncol = length(unique(species_groups_abb)), nrow = nrow(amoeba)))
names(grouped_amoeba_abb) <- unique(species_groups_abb)

# Convert columns to numeric before summation
for (group in unique(species_groups_abb)) {
  species_in_group <- names(species_groups_abb)[species_groups_abb == group]
  # Convert columns to numeric
  amoeba[species_in_group] <- lapply(amoeba[species_in_group], as.numeric)
  # Sum columns and assign to grouped_amoeba
  grouped_amoeba_abb[[group]] <- rowSums(amoeba[, species_in_group], na.rm = TRUE)
}


# Convert to percentages
grouped_amoeba_abb_percent <- apply(grouped_amoeba_abb, 1, function(x) (x / sum(x)) * 100)

grouped_amoeba_abb_percent <-t(grouped_amoeba_abb_percent)

# Since apply returns a matrix, convert it back to a data frame if necessary
grouped_amoeba_abb_percent <- as.data.frame(grouped_amoeba_abb_percent)

# Replace NaN with 0 in all numeric columns
grouped_amoeba_abb_percent[] <- lapply(grouped_amoeba_abb_percent, function(x) {
  if(is.numeric(x)) {  # Check if the column is numeric
    x[is.nan(x)] <- 0  # Replace NaN with 0
  }
  x  # Return the modified column
})


# Add Sample name and depth columns
grouped_amoeba_abb <- cbind(amoeba[c("Sample name", "Depth", "Volume")], grouped_amoeba_abb )
grouped_amoeba_abb_percent <- cbind(amoeba[c("Sample name", "Depth", "Volume")], grouped_amoeba_abb_percent)

# Create a new workbook
wb <- createWorkbook()

# Add sheets to the workbook
addWorksheet(wb, "Total count")
addWorksheet(wb, "Percentages")

# Write data frames to different sheets
writeData(wb, sheet = "Total count", x = grouped_amoeba_abb)
writeData(wb, sheet = "Percentages", x = grouped_amoeba_abb_percent)

# Save the workbook
saveWorkbook(wb, file = "grouped_amoeba_abb.xlsx", overwrite = TRUE)

