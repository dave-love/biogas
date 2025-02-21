#Figure 5

#Data from Table 5-2:  Emissions from Agriculture (kt) on pg3 of the document
#https://www.epa.gov/system/files/documents/2024-04/us-ghg-inventory-2024-chapter-5-agriculture.pdf

df <- read.csv("https://raw.githubusercontent.com/dave-love/biogas/main/epa_2022_ag_emissions.csv") %>% 
  mutate(Gas = fct_relevel(Gas,"N2O", "CO2", "CH4"),
         Source = fct_reorder(Source, -Amount))


p <-  ggplot(df, aes(x=Source, y= Amount, fill=Gas)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="PuRd")+
  labs(x="Source",y="Emissions (MMT CO2 eq)")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="right",
        panel.background = element_blank(),
        panel.border = element_rect(colour="grey", fill = NA),
        strip.background =element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1))

print(p)