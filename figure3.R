#Figure 3
#original source of code: https://asmith.ucdavis.edu/news/cow-power-rising


library(ggplot2)
library(dplyr)
library(forcats)
library(patchwork)

df <- read.csv("https://raw.githubusercontent.com/dave-love/biogas/main/agstar-livestock-ad-database_june2024.csv")

# Define key states
key_states <- c("CA", "NY", "PA", "WI")

# Prepare Data for Plot 1
plot_bar <- df %>%
  filter(!is.na(Year.Operational), 
         Animal.Farm.Type.s. == "Dairy", 
         Year.Operational < 2025) %>%
  mutate(Year.Operational = ifelse(Year.Operational > 1999, as.character(Year.Operational), "pre-2000"),
         State = ifelse(State %in% key_states, State, "Other"))

p1 <- ggplot(plot_bar, aes(x = factor(Year.Operational, levels = c("pre-2000", 2000:2024)), 
                           fill = factor(State, levels = c("Other","WI","PA","NY","CA")))) +
  geom_histogram(position = "stack", stat = "count") +
  scale_fill_brewer(palette = "GnBu") +
  labs(x = "Year", y = "Number of Digesters", fill = "State") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "grey", fill = NA),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("A")

# Prepare Data for Plot 2
plot_bar2 <- df %>%
  filter(!is.na(Biogas.End.Use.s.), 
         Animal.Farm.Type.s. == "Dairy",
         Year.Operational < 2025,
         Digester.Type != "") %>%
  mutate(Biogas.End.Use.s. = case_when(
    Biogas.End.Use.s. %in% c("CNG", "Electricity", "Cogeneration", "Pipeline Gas", 
                             "Boiler/Furnace fuel", "Flared Full-time") ~ Biogas.End.Use.s.,
    Biogas.End.Use.s. %in% c("Boiler/Furnace fuel; CNG", "Cogeneration; Boiler/Furnace fuel", 
                             "Cogeneration; CNG", "Cogeneration; Pipeline Gas", "Cogeneration; Refrigeration", 
                             "Electricity; Boiler/Furnace fuel", "Electricity; Boiler/Furnace fuel; CNG", 
                             "Electricity; CNG", "Electricity; Cogeneration", 
                             "Electricity; Cogeneration; Boiler/Furnace fuel", 
                             "Electricity; Pipeline Gas") ~ "Mixed",
    Biogas.End.Use.s. == "Pipeline to Electricity" ~ "Electricity"
  ),
  State = ifelse(State %in% key_states, State, "Other")) %>%
  mutate(Biogas.End.Use.s. = fct_relevel(Biogas.End.Use.s., "CNG", "Cogeneration", "Electricity", 
                                         "Pipeline gas", "Mixed", "Flared Full-time", "Boiler/Furnace fuel")) %>%
  filter(Digester.Type != "") %>%
  filter(!is.na(Biogas.End.Use.s.))


p2 <- ggplot(plot_bar2, aes(x = Biogas.End.Use.s., 
                            fill = factor(State, levels = c("Other","WI","PA","NY","CA")))) +
  geom_histogram(position = "stack", stat = "count") +
  scale_fill_brewer(palette = "GnBu") +
  labs(x = "Biogas End Use", y = "Number of Digesters", fill = "State") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "grey", fill = NA),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("B")

# Prepare Data for Plot 3
plot_bar3 <- df %>%
  filter(!is.na(Digester.Type), 
         Animal.Farm.Type.s. == "Dairy",
         Year.Operational < 2025,
         Digester.Type != "") %>%
  mutate(Digester.Type = case_when(
    Digester.Type %in% c("Mixed Plug Flow", "Plug Flow - Unspecified", "Horizontal Plug Flow", "Vertical Plug Flow") ~ "Plug flow",
    Digester.Type %in% c("Complete Mix", "Complete Mix Mini Digester") ~ "Complete mix",
    Digester.Type == "Covered Lagoon" ~ "Covered lagoon",
    TRUE ~ "Other"
  ),
  State = ifelse(State %in% key_states, State, "Other")) %>%
  mutate(Digester.Type = fct_relevel(Digester.Type, "Covered lagoon", "Plug flow", "Complete mix", "Other")) %>%
  filter(Digester.Type != "")

p3 <- ggplot(plot_bar3, aes(x = Digester.Type, 
                            fill = factor(State, levels = c("Other","WI","PA","NY","CA")))) +
  geom_histogram(position = "stack", stat = "count") +
  scale_fill_brewer(palette = "GnBu") +
  labs(x = "Digester type", y = "Number of Digesters", fill = "State") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "grey", fill = NA),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("C")

# Combine Plots
p <- p1 / (p2 + p3) + plot_layout(guides = "collect", heights = c(2, 1))

# Print final plot
print(p)
