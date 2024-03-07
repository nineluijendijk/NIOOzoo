library(readxl)
library(tidyverse)
library(here)

dataraw <- read_excel(here("data_raw/Countingsheet.xlsx"), sheet = "Sheet2")



data <- dataraw %>% mutate("SmallJuvenileAbundance_indL" = (1000 / `SampleVolume(mL)`) * SmallJuvenile,
                "LargeJuvenileAbundance_indL" = (1000 / `SampleVolume(mL)`) * LargeJuvenile,
                "AdultAbundance_indL" = (1000 / `SampleVolume(mL)`) * Adult,
                "Total_indL" = SmallJuvenileAbundance_indL + LargeJuvenileAbundance_indL + AdultAbundance_indL)

summary <- data %>% group_by(Species, Medium) %>% summarize(`Small juvenile` = mean(SmallJuvenileAbundance_indL, na.rm = TRUE),
                   `Large juvenile` = mean(LargeJuvenileAbundance_indL, na.rm = TRUE),
                   `Adult` = mean(AdultAbundance_indL, na.rm = TRUE)) 

plotting <- pivot_longer(summary, cols = c(`Small juvenile`, `Large juvenile`, Adult),
            names_to = "Age",
            values_to = "indL")

ggplot(plotting, aes(x = as.numeric(interaction(Species,Medium)), y = indL, fill = Species, color=Age)) + 
  geom_bar(stat = "identity", size = 0.7) +
  scale_x_continuous(breaks=c(2.5, 6.5, 10.5, 14.5, 18.5),labels=c("ADaM","GW","KB", "KH", "KM"))+
  scale_y_continuous(breaks = seq(0, 1750, 250))+
  scale_color_manual(values = c("deeppink","darkorange","blue4")) +
  guides(color = guide_legend(override.aes = list(fill = "white")))+
  labs(y = "Individuals per liter", 
       title = "Mean number of individuals per liter",
       x = "Medium")+
  theme_minimal()

data %>%
  group_by(Medium, Species) %>%
  summarise(p.value.sw = shapiro.test(Total_indL)$p.value) 

species <- c("D. pulex", "D. pulicaria", "D. galeata", "D. ambigua")

for (i in species) {
  print(i)
dataF <- data %>% filter(Species == i)
print(leveneTest(Total_indL ~ Medium, data = dataF)) 

res_aov <- aov(Total_indL ~ Medium, data = dataF)
print(summary(res_aov))
print(TukeyHSD(res_aov))
}
