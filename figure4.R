#Figure 4

library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(patchwork)

df <- read.csv("https://raw.githubusercontent.com/dave-love/biogas/main/agstar-livestock-ad-database_june2024.csv")

plot <- df %>%
  mutate(Year.Operational = as.numeric(Year.Operational),
         Animal.Farm.Type.s. = case_when(
           Animal.Farm.Type.s. %in% c("Broiler", "Poultry") ~ "Poultry",
           Animal.Farm.Type.s. == "Cattle" ~ "Beef cattle",
           Animal.Farm.Type.s. %in% c("Cattle; Dairy", "Cattle; Swine", 
                                      "Dairy; Poultry; Swine", "Dairy; Swine", "Swine;Poultry") ~ "Mixed",
           Animal.Farm.Type.s. == "Dairy" ~ "Dairy cows",
           Animal.Farm.Type.s. == "Swine" ~ "Swine",
           TRUE ~ Animal.Farm.Type.s.
         )) %>%
  filter(Year.Operational < 2025 | is.na(Year.Operational)) %>%
  select(City, County, State, Animal.Farm.Type.s., Year.Operational) %>%
  group_by(Animal.Farm.Type.s.) %>%
  summarize(n=n())

#plot made in ArcGIS