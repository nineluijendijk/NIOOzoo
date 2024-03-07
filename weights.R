library(readxl)
library(tidyverse)
library(here)

dataraw <- read_excel(here("data_raw/Weighingsheet.xlsx"), range = "A1:E61", na = "NA")

data <- dataraw %>% mutate("WeightIncrease_µg" = WeightTotal - WeightFilter)

summary <- data %>% group_by(Medium, Species) %>% summarize(Weight = mean(WeightIncrease_µg, na.rm = TRUE))

ggplot(data = summary, aes(x = Species, y = Weight, fill = Medium))+
  geom_col(position = "dodge")+
  theme_minimal()+
  labs(y = "Weight in µg",
       title = "Mean weight of the population")+
  scale_fill_manual(values = c("#Ff9400", "#E28fd9", "#D9dee0", "#39a6d6", "#Dc1906"))

data %>% filter(! Medium == "KM" | ! Species == "D. galeata") %>% group_by(Medium, Species) %>%
  summarise(p.value.sw = shapiro.test(WeightIncrease_µg)$p.value) 

species <- c("D. pulex", "D. pulicaria", "D. galeata", "D. ambigua")

for (i in species) {
  print(i)
  dataF <- data %>% filter(Species == i)
  print(leveneTest(WeightIncrease_µg ~ Medium, data = dataF)) 
  
  res_aov <- aov(WeightIncrease_µg ~ Medium, data = dataF)
  print(summary(res_aov))
  print(TukeyHSD(res_aov))
}

