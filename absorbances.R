library(readxl)
library(tidyverse)
library(here)
library(car)
library(FSA)
library(flextable)

dataraw <- read_excel(here("data_raw/OnderzoekMedium.xlsx"), sheet = "Absorbances", range = "A1:F61", na = "NA")
# datatidy <- mutate(dataraw, "MeanAbsorbance" = rowMeans(dataraw[ , c(4:6)], na.rm = TRUE), A1=NULL, A2=NULL, A3=NULL)
datatidy <- pivot_longer(dataraw, cols = c("A1", "A2", "A3"), names_to = "Clone", values_to = "Absorbance")

regression <- read_excel(here("data_raw/Carbon concentration (based on chemostat C).xlsx"), sheet = "Carbon regression line", range = "A1:C9")

model <- lm(`Carbon Content (mg/L)` ~ `Light absorption (λ)`, data = regression) # Calculate regression curve

data <- mutate(datatidy, "Carbon_mgL" = (datatidy$Absorbance *  model$coefficients[[2]] + model$coefficients[[1]]) / 5 )

ggplot(data = data, aes(y = Carbon_mgL, x = factor(Species), fill = Medium))+
  stat_boxplot(geom ='errorbar')+
  geom_boxplot()+
  theme_minimal()+
  theme(axis.text.x = element_text(face="italic"))+
  labs(y = "Carbon concentration (mg/L)", 
       title = "Carbon concentration of the medium after two days",
       x = "Species")+
  scale_fill_manual(values = c("#Ff9400", "#E28fd9", "#D9dee0", "#39a6d6", "#Dc1906"),
                                 labels = c("ADaM", "Groundwater", "Aerated tap water", "Hay water", "Manure water"))+
  stat_summary(geom = "errorbar", fun.min = mean, fun = mean, fun.max = mean, width = 0.75,
               linetype = "dotted", position = position_dodge())

data %>% filter(! Medium == "GW" | ! Species == "D. ambigua") %>%
  group_by(Medium, Species) %>%
  summarise(p.value.sw = shapiro.test(Carbon_mgL)$p.value) 

species <- c("D. pulex", "D. pulicaria", "D. galeata", "D. ambigua")
treatment <- unique(data$Medium)
treatment <- unique(data$Medium)

for (i in species) {
  print(i)
  dataF <- data %>% filter(Species == i)
  print(leveneTest(Carbon_mgL ~ Medium, data = dataF)) 
  
  print(kruskal.test(Carbon_mgL ~ Medium, data = dataF))
  comb <- dunnTest(Carbon_mgL ~ Medium,
                 data=dataF,
                 method="bonferroni")[[2]][c(1, 4)]
  
  dataCor <- dataF[, -5] %>% pivot_wider(names_from = Medium,
                                        values_from = Carbon_mgL)
  
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
 
 combined <- cbind(comb, correlation)
 flextable(combined) %>% print()  
}
