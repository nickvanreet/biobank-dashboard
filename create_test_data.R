library(writexl)
library(tidyverse)

# Create sample biobank data
test_data <- tibble(
  Numéro = sprintf("MM-%04d", 1:50),
  `Code-Barres KPS` = sprintf("KPS%06d", 1001:1050),
  Etude = sample(c("DA", "DP"), 50, replace = TRUE),
  `Structure sanitaire` = sample(c("CS Bipemba", "CS Kanshi", "CS Munzembe"), 50, replace = TRUE),
  `Zone de santé` = sample(c("Dipumba", "Tshofa", "Nkole"), 50, replace = TRUE),
  Province = rep("KASAI-ORIENTAL", 50),
  `Date de prélèvement` = seq(as.Date("2024-01-01"), by = "week", length.out = 50),
  `Date envoi vers CPLTHA` = seq(as.Date("2024-01-05"), by = "week", length.out = 50),
  `Date réception CPLTHA` = seq(as.Date("2024-01-08"), by = "week", length.out = 50),
  `Date envoi INRB` = seq(as.Date("2024-01-15"), by = "week", length.out = 50),
  `Age (année de naissance)` = sample(10:80, 50, replace = TRUE),
  Sexe = sample(c("M", "F"), 50, replace = TRUE),
  `Ancien cas` = sample(c("Oui", "Non", "Incertain"), 50, replace = TRUE),
  Traité = sample(c("Oui", "Non", "Incertain"), 50, replace = TRUE),
  `Stockage avant CPLTHA` = sample(c("Ambiante", "Frigo", "Congelateur"), 50, replace = TRUE),
  `Présence DRS` = sample(c("Oui", "Non"), 50, replace = TRUE),
  `Présence DBS` = sample(c("Oui", "Non"), 50, replace = TRUE),
  `Nombre DBS` = sample(1:5, 50, replace = TRUE)
)

# Write to Excel
write_xlsx(test_data, "/home/claude/data/biobank/test_biobank_data.xlsx")
cat("Test data created successfully!\n")
