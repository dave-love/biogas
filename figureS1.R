#Figure S1
#original source of code: https://asmith.ucdavis.edu/news/cow-power-rising

library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(patchwork)

# Load data
df <- read.csv("https://raw.githubusercontent.com/dave-love/biogas/main/agstar-livestock-ad-database_june2024.csv")

# Load shutdown data
shutdown <- read.csv("https://raw.githubusercontent.com/dave-love/biogas/main/agstar-livestock-ad-database_shutdown_june2024.csv")

# Aggregate data for operational and shutdown status
plot <- df %>%
  mutate(Year.Operational = as.numeric(Year.Operational)) %>%
  filter(!is.na(Year.Operational), Year.Operational < 2025) %>%
  group_by(Year.Operational, Status) %>%
  summarise(n = n(), .groups = "drop")

plot_shutdown <- shutdown %>%
  mutate(Year.Operational = as.numeric(Year.Operational)) %>%
  filter(!is.na(Year.Operational), Year.Operational < 2025) %>%
  group_by(Year.Operational, Status) %>%
  summarise(n = -n(), .groups = "drop")  # Negative count for shutdowns

# Combine datasets and fix factor levels
plot <- bind_rows(plot, plot_shutdown) %>%
  filter(Year.Operational >= 2000) %>%
  mutate(Status = factor(Status, levels = c("Operational", "Construction", "Shut down")),
         Status = fct_recode(Status, 
                             "Newly operational" = "Operational",
                             "Under construction" = "Construction", 
                             "Shut down" = "Shut down"))

# Plot: Operational vs Shutdown Digesters Over Time
p <- ggplot(plot, aes(x = Year.Operational, y = n, fill = Status)) +
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

print(p)
