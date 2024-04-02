library(readxl)
library(tidyverse)
library(here)
library(car)
library(flextable)

dataraw <- read_excel(here("data_raw/Weighingsheet.xlsx"), range = "A1:E61", na = "NA")

data <- dataraw %>% mutate("Weight_mgL" = (WeightTotal - WeightFilter) * 2.5 / 1000)

ggplot(data = data, aes(x = Species, y = Weight_mgL, fill = Medium))+
  stat_boxplot(geom ='errorbar')+
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Biomass (mg/L)",
       title = "Dry weight per liter")+
  scale_fill_manual(values = c("#Ff9400", "#E28fd9", "#D9dee0", "#39a6d6", "#Dc1906"),
                               labels = c("ADaM", "Groundwater", "Aerated tap water", "Hay water", "Manure water"))+
  theme(axis.text.x = element_text(face="italic"))+
  stat_summary(geom = "errorbar", fun.min = mean, fun = mean, fun.max = mean, width = 0.75,
               linetype = "dotted", position = position_dodge())

data %>% filter(! Medium == "KM" | ! Species == "D. galeata") %>% group_by(Medium, Species) %>%
  summarise(p.value.sw = shapiro.test(Weight_mgL)$p.value) 

species <- c("D. pulex", "D. pulicaria", "D. galeata", "D. ambigua")

for (i in species) {
  print(i)
  dataF <- data %>% filter(Species == i)
  print(leveneTest(Weight_mgL ~ Medium, data = dataF)) 
  
  res_aov <- aov(Weight_mgL ~ Medium, data = dataF)
  print(summary(res_aov))
  comb <- matrix()
  comb <- TukeyHSD(res_aov)[[1]][,4] %>% t() %>% insertRow(comb, 1, .)
  
  dataCor <- dataF %>% select(Species, Medium, Clone, Weight_mgL) %>% pivot_wider(names_from = Medium,
                                                                                             values_from = Weight_mgL)
  
  correlation <- tibble()
  
  cor(dataCor$ADaM, dataCor$GW) %>% rbind(correlation, .) -> correlation
  cor(dataCor$ADaM, dataCor$KB) %>% rbind(correlation, .) -> correlation
  cor(dataCor$GW, dataCor$KB) %>% rbind(correlation, .) -> correlation
  cor(dataCor$ADaM, dataCor$KH) %>% rbind(correlation, .) -> correlation
  cor(dataCor$GW, dataCor$KH) %>% rbind(correlation, .) -> correlation
  cor(dataCor$KB, dataCor$KH) %>% rbind(correlation, .) -> correlation
  cor(dataCor$ADaM, dataCor$KM) %>% rbind(correlation, .) -> correlation
  cor(dataCor$GW, dataCor$KM) %>% rbind(correlation, .) -> correlation
  cor(dataCor$KB, dataCor$KM) %>% rbind(correlation, .) -> correlation
  cor(dataCor$KH, dataCor$KM) %>% rbind(correlation, .) -> correlation
  colnames(correlation) <- "cor"
  
  combined <- cbind(head(comb, n = 10), correlation)
  combined <- insertCol(as.matrix(combined), 1, c("ADaM - GW", "ADaM - KB", "GW - KB", "ADaM - KH", "GW - KH", "KB - KH", "ADaM - KM", "GW - KM", "KB - KM", "KH - KM"))
  flextable(as.data.frame(combined)) %>% print()
  
}









for (i in species) {
  print(i)
  dataF <- data %>% filter(Species == i)
  print(leveneTest(Weight_mgL ~ Medium, data = dataF)) 
  
  print(kruskal.test(Weight_mgL ~ Medium, data = dataF))
  print(pairwise.wilcox.test(dataF$Weight_mgL, dataF$Medium,
                             p.adjust.method = "BH", exact = FALSE))
}
