setwd("~/Desktop")
library(readr)
library(dplyr)

#Download filtered data to desktop as .csv. This is only PropertyType = C/I
LA_Data <- read_csv("Assessor_Parcels_Data_-_2006_thru_2019.csv")

# Make factors
LA_Data$GeneralUseType <- as.factor(LA_Data$GeneralUseType)
LA_Data$SpecificUseType <- as.factor(LA_Data$SpecificUseType)
levels(LA_Data$SpecificUseType)

#Subset the data to be only 2019
LA_Data_2019 <- LA_Data %>% 
  filter(RollYear == 2019)

library(ggplot2)
library(cowplot)

# Plot of Land Assessed Value by year grouped by General Use Type
ggplot(data = LA_Data_2019, aes(x=LandBaseYear, y = LandValue, fill = GeneralUseType)) + 
  geom_bar(stat = 'identity', position = "stack") + 
  xlim(1975,2020) +
  labs(x = "Land Assessment Year", y = "Land Assessment Value", fill = "General Use Type") +
  theme_cowplot() 

# Plot of Land Assessed Value by year grouped by Specific Use Type
ggplot(data = LA_Data_2019, aes(x=LandBaseYear, y = LandValue, fill = SpecificUseType)) + 
  geom_bar(stat = 'identity', position = "stack") + 
  xlim(1975,2020) +
  labs(x = "Land Assessment Year", y = "Land Assessment Value", fill = "Specific Use Type") +
  theme_cowplot() +
  theme(legend.title = element_text(size = 5), 
          legend.text = element_text(size = 5)) +
  guides(color = guide_legend(override.aes = list(size = 1))) 
  
