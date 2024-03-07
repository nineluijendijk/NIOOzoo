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

ggplot(plotting, aes(x = as.numeric(interaction(Medium,Species)), y = indL, fill = Medium, color=Age)) + 
  geom_bar(stat = "identity", size = 0.7) +
  scale_x_continuous(breaks=c(3, 8, 13, 18),labels=c("D. ambigua","D. galeata","D. pulex", "D. pulicaria"))+
  scale_y_continuous(breaks = seq(0, 1750, 250))+
  scale_color_manual(values = c("#30cf4e","#e7d718","#00168a"))+
  scale_fill_manual(values = c("#Ff9400", "#E28fd9", "#D9dee0", "#39a6d6", "#Dc1906"))+
  guides(color = guide_legend(override.aes = list(fill = "white")))+
  labs(y = "Individuals per liter", 
       title = "Mean number of individuals per liter",
       x = "Species")+
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
