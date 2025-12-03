This explains how I cleaned, graphed, and analyzed the data

##Data Cleaning

#Library
library(tidyverse)
library(dplyr)
library(readxl)

#Load in data-specifically use the sheet from the Excel file that is titled "Master Renamed"
Shellfish_metadata_mastersheet_10Oct25 <- read_excel("Olivia-Data-Science-Final-Project/Shellfish metadata mastersheet 10Oct25.xlsx", sheet = "Master Renamed")

View(Shellfish_metadata_mastersheet_10Oct25)

#Cleaning
Shellfish_metadata_mastersheet_10Oct25 <- Shellfish_metadata_mastersheet_10Oct25 |>
  select(-...16, -...20, -...21, -Comments, -Tank, -Section, -"Ripe?", -"Dry_Shell_(g)", -BeeTag_ID, -Unique_ID) |> #remove columns
  filter(if_all(c(`Before_Weight_(g)`, `Before_Length_(mm)`, `Before_Width_(mm)`, `Before_Depth_(mm)`, `After_Weight_(g)`, `After_Length_(mm)`, `After_Width_(mm)`, `After_Depth_(mm)`), ~ !is.na(.x)))

##Calculate change in measurements
change_measurements <- Shellfish_metadata_mastersheet_10Oct25 |>
  mutate(weight_change = `After_Weight_(g)` - `Before_Weight_(g)`,
         width_change = `After_Width_(mm)` - `Before_Width_(mm)`,
         length_change = `After_Length_(mm)` - `Before_Length_(mm)`,
         depth_change = `After_Depth_(mm)` - `Before_Depth_(mm)`)
         
##Boxplot- change in weight across treatments
ggplot(change_measurements, aes(x = Treatment, y = weight_change, fill = Treatment)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("orange", "turquoise", "greenyellow", "orchid1")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none") +
  labs(
    y = "Weight (g)"
  )

##Boxplot- change in width across treatments
ggplot(change_measurements, aes(x = Treatment, y = width_change, fill = Treatment)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("orange", "turquoise", "greenyellow", "orchid1")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none") +
  labs(
    y = "Width (mm)"
  )  

##Boxplot- change in length across treatments
ggplot(change_measurements, aes(x = Treatment, y = length_change, fill = Treatment)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("orange", "turquoise", "greenyellow", "orchid1")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none") +
  labs(
    y = "Length (mm)"
  )

##Boxplot- change in depth across treatments
ggplot(change_measurements, aes(x = Treatment, y = depth_change, fill = Treatment)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("orange", "turquoise", "greenyellow", "orchid1")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none") +
  labs(
    y = "Depth (mm)"
  )

##Boxplot- change in weight across species
ggplot(change_measurements, aes(x = Species, y = weight_change, fill = Species)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("steelblue1", "yellow", "rosybrown1")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none") +
  labs(
    y = "Weight (g)"
  )

##Boxplot- change in width across species
ggplot(change_measurements, aes(x = Species, y = width_change, fill = Species)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("steelblue1", "yellow", "rosybrown1")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none") +
  labs(
    y = "Width (mm)"
  )

##Boxplot- change in length across species
ggplot(change_measurements, aes(x = Species, y = length_change, fill = Species)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("steelblue1", "yellow", "rosybrown1")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none") +
  labs(
    y = "Length (mm)"
  )

##Boxplot- change in depth across species
ggplot(change_measurements, aes(x = Species, y = depth_change, fill = Species)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("steelblue1", "yellow", "rosybrown1")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none") +
  labs(
    y = "Depth (mm)"
  )

##Statistical Tests - Change in measurements across treatments

#ANOVA - Change in weight vs treatments
Weight_t_aov <- aov(weight_change ~ Treatment, data = change_measurements)

summary(Weight_t_aov)
#significant

#Tukey test - Change in weight vs treatments
TukeyHSD(Weight_t_aov)
#Ambient-Acidified = significant

#ANOVA - Change in width vs treatments
Width_t_aov <- aov(width_change ~ Treatment, data = change_measurements)

summary(Width_t_aov)
#not significant

#ANOVA - Change in length vs treatments
Length_t_aov <- aov(length_change ~ Treatment, data = change_measurements)

summary(Length_t_aov)
#not significant

#ANOVA - Change in depth vs treatments
Depth_t_aov <- aov(depth_change ~ Treatment, data = change_measurements)

summary(Depth_t_aov)
#significant

#Tukey test - Change in depth vs treatments
TukeyHSD(Depth_t_aov)
#Warm-Ambient = significant
#Warm-Multi = significant

##Statistical Tests - Change in measurements across species

#ANOVA - Change in weight vs species
Weight_s_aov <- aov(weight_change ~ Species, data = change_measurements)

summary(Weight_s_aov)
#significant

#Tukey test - Change in weight vs species
TukeyHSD(Weight_s_aov)
#Oyster-Mussel = significant
#Scallop-Mussel = significant
#Scallop-Oyster = significant

#ANOVA - Change in width vs species
Width_s_aov <- aov(width_change ~ Species, data = change_measurements)

summary(Width_s_aov)
#significant

#Tukey test - Change in width vs species
TukeyHSD(Width_s_aov)
#Oyster-Mussel = significant
#Scallop-Mussel = significant
#Scallop-Oyster = significant

#ANOVA - Change in length vs species
Length_s_aov <- aov(length_change ~ Species, data = change_measurements)

summary(Length_s_aov)
#not significant

#ANOVA - Change in depth vs species
Depth_s_aov <- aov(depth_change ~ Species, data = change_measurements)

summary(Depth_s_aov)
#significant

#Tukey test - Change in depth vs species
TukeyHSD(Depth_s_aov)
#Oyster-Mussel = significant
#Scallop-Mussel = significant
