library(readxl)
library(tidyverse)
library(here)
library(car)

dataraw <- read_excel(here("data_raw/Weighingsheet.xlsx"), range = "A1:E61", na = "NA")

data <- dataraw %>% mutate("Weight_mgL" = (WeightTotal - WeightFilter) * 2.5 / 1000)

ggplot(data = data, aes(x = Species, y = Weight_mgL, fill = Medium))+
  stat_boxplot(geom ='errorbar')+
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Biomass (mg/L)",
       title = "Dry weight per liter as a measure of biomass")+
  scale_fill_manual(values = c("#Ff9400", "#E28fd9", "#D9dee0", "#39a6d6", "#Dc1906"),
                               labels = c("ADaM", "Groundwater", "Aerated tap water", "Hay water", "Manure water"))+
  theme(axis.text.x = element_text(face="italic"))+
  stat_summary(geom = "errorbar", fun.min = mean, fun = mean, fun.max = mean, width = 0.75,
               linetype = "dotted", position = position_dodge())+
  stat_compare_means(label =  "p.signif", label.x = 1.5, hide.ns = TRUE, method = "anova")

data %>% filter(! Medium == "KM" | ! Species == "D. galeata") %>% group_by(Medium, Species) %>%
  summarise(p.value.sw = shapiro.test(Weight_mgL)$p.value) 

species <- c("D. pulex", "D. pulicaria", "D. galeata", "D. ambigua")

for (i in species) {
  print(i)
  dataF <- data %>% filter(Species == i)
  print(leveneTest(Weight_mgL ~ Medium, data = dataF)) 
  
  res_aov <- aov(Weight_mgL ~ Medium, data = dataF)
  print(summary(res_aov))
  print(TukeyHSD(res_aov))
}
