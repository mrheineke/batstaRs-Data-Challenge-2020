setwd("~/Desktop")
library(readr)
library(dplyr)
library(forcats)

### Data Import and Cleaning
#Download filtered data to desktop as .csv. This is only PropertyType = C/I
LA_Data <- read_csv("Assessor_Parcels_Data_-_2006_thru_2019.csv")

# Make factors
LA_Data$GeneralUseType <- as.factor(LA_Data$GeneralUseType)
LA_Data$SpecificUseType <- as.factor(LA_Data$SpecificUseType)
LA_Data$SpecificUseType <-  fct_explicit_na(LA_Data$SpecificUseType, na_level = "Missing")
levels(LA_Data$SpecificUseType)

#Subset the data to be only 2019
LA_Data_2019 <- LA_Data %>% 
  filter(RollYear == 2019)

library(ggplot2)
library(cowplot)

### Land Value
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

# Plot of Land Assessed Value by Specifc Use Type
LA_Data_2019_SpecUse <- LA_Data_2019 %>%
  group_by(SpecificUseType) %>%
  summarise(mean = mean(LandValue))

ggplot(data = LA_Data_2019_SpecUse, aes(x = reorder(SpecificUseType, mean), y = mean)) +
  geom_bar(stat = 'identity') +
  theme_cowplot() +
  labs(y = "Mean Land Value", x = "", title = "Mean Land Value Across Specific Use Types in LA County" ) +
  theme(axis.text.y = element_text(size = 6)) +
  coord_flip() 

### Net Taxable Value
# Plot of Net Taxable Value by year grouped by General Use Type
ggplot(data = LA_Data_2019, aes(x=LandBaseYear, y = netTaxableValue, fill = GeneralUseType)) + 
  geom_bar(stat = 'identity', position = "stack") + 
  xlim(1975,2020) +
  labs(x = "Land Assessment Year", y = "Net Taxable Value", fill = "General Use Type") +
  theme_cowplot() 

# Plot of Net Taxable Value by year grouped by Specific Use Type
ggplot(data = LA_Data_2019, aes(x=LandBaseYear, y = netTaxableValue, fill = SpecificUseType)) + 
  geom_bar(stat = 'identity', position = "stack") + 
  xlim(1975,2020) +
  labs(x = "Land Assessment Year", y = "Net Taxable Value (USD)", fill = "Specific Use Type", title = "Net Taxable Value by Year Assessed in LA County") +
  theme_cowplot() +
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5)) +
  guides(color = guide_legend(override.aes = list(size = 1))) 
ggsave("SpecUse_NetTaxValue.png", dpi = 320 )

# Plot of Net Taxable Value by Specifc Use Type
LA_Data_2019_SpecUse <- LA_Data_2019 %>%
  group_by(SpecificUseType) %>%
  summarise(mean = mean(netTaxableValue))

ggplot(data = LA_Data_2019_SpecUse, aes(x = reorder(SpecificUseType, mean), y = mean)) +
  geom_bar(stat = 'identity') +
  theme_cowplot() +
  labs(y = "Mean Net Taxable Value", x = "", title = "Mean Net Taxable Value Across Specific Use Types in LA County" ) +
  theme(axis.text.y = element_text(size = 6)) +
  coord_flip() 


  
