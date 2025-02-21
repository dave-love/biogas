#Figure 2
#original source of code: https://asmith.ucdavis.edu/news/cow-power-rising


library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(patchwork)

# Load data
df <- read.csv("https://raw.githubusercontent.com/dave-love/biogas/main/agstar-livestock-ad-database_june2024.csv")

# Define consistent animal categories
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
  filter(!is.na(Year.Operational), Year.Operational < 2025) %>%
  group_by(Year.Operational, Animal.Farm.Type.s.) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year.Operational = full_seq(Year.Operational, 1), 
           Animal.Farm.Type.s., fill = list(n = 0)) %>%
  group_by(Animal.Farm.Type.s.) %>%
  mutate(csum_n = cumsum(n)) %>%
  ungroup() %>%
  mutate(Animal.Farm.Type.s. = fct_relevel(Animal.Farm.Type.s., 
                                           "Dairy cattle", "Swine", "Poultry", "Beef cattle", "Mixed")) %>%
  filter(Year.Operational >= 2000)

# Plot 1: Cumulative Digesters Over Time
p1 <- ggplot(plot, aes(x = Year.Operational, y = csum_n, fill = Animal.Farm.Type.s.)) +
  geom_area() +
  scale_fill_brewer(palette = "PuRd") +
  labs(x = 'Year', y = 'Number of Digesters', fill = "Animal type") +
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="right",
        panel.background = element_blank(),
        panel.border = element_rect(colour="grey", fill = NA),
        strip.background =element_blank())

# Load shutdown data
shutdown <- read.csv("https://raw.githubusercontent.com/dave-love/biogas/main/agstar-livestock-ad-database_shutdown_june2024.csv")

# Aggregate data for operational and shutdown status
plot2 <- df %>%
  mutate(Year.Operational = as.numeric(Year.Operational)) %>%
  filter(!is.na(Year.Operational), Year.Operational < 2025) %>%
  group_by(Year.Operational, Status) %>%
  summarise(n = n(), .groups = "drop")

plot2_shutdown <- shutdown %>%
  mutate(Year.Operational = as.numeric(Year.Operational)) %>%
  filter(!is.na(Year.Operational), Year.Operational < 2025) %>%
  group_by(Year.Operational, Status) %>%
  summarise(n = -n(), .groups = "drop")  # Negative count for shutdowns

# Combine datasets and fix factor levels
plot2 <- bind_rows(plot2, plot2_shutdown) %>%
  filter(Year.Operational >= 2000) %>%
  mutate(Status = factor(Status, levels = c("Operational", "Construction", "Shut down")),
         Status = fct_recode(Status, 
                             "Newly operational" = "Operational",
                             "Under construction" = "Construction", 
                             "Shut down" = "Shut down"))

# Plot 2: Operational vs Shutdown Digesters Over Time
p2 <- ggplot(plot2, aes(x = Year.Operational, y = n, fill = Status)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(palette = "OrRd") +
  labs(x = "Year", y = "Number of Digesters", fill = "Status") +
  theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position="right",
              panel.background = element_blank(),
              panel.border = element_rect(colour="grey", fill = NA),
              strip.background =element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = .1)

# Combine Plots
p <- (p1 / p2) & theme(legend.text = element_text(size = 7))

print(p)
