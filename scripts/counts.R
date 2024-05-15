library(readxl)
library(tidyverse)
library(here)
library(car)
library(FSA)
library(ggtext)
library(ggpubr)

dataraw <- read_excel(here("data_raw/Countingsheet.xlsx"), sheet = "Sheet2")

data <- dataraw %>% mutate("SmallJuvenileAbundance_indL" = (1000 / `SampleVolume(mL)`) * SmallJuvenile,
                "LargeJuvenileAbundance_indL" = (1000 / `SampleVolume(mL)`) * LargeJuvenile,
                "AdultAbundance_indL" = (1000 / `SampleVolume(mL)`) * Adult,
                "Total_indL" = SmallJuvenileAbundance_indL + LargeJuvenileAbundance_indL + AdultAbundance_indL)

summary <- data %>% group_by(Species, Medium) %>% summarize(`Small juvenile` = mean(SmallJuvenileAbundance_indL, na.rm = TRUE),
                   `Large juvenile` = mean(LargeJuvenileAbundance_indL, na.rm = TRUE),
                   `Adult` = mean(AdultAbundance_indL, na.rm = TRUE)) 

tot <- summary %>% mutate("Total" = `Small juvenile` + `Large juvenile` + Adult)
stdev <- data %>% group_by(Species, Medium) %>% summarize(sdev = sd(Total_indL, na.rm = TRUE)) 
h <- left_join(stdev, tot)

plotting <- pivot_longer(summary, cols = c(`Small juvenile`, `Large juvenile`, Adult),
            names_to = "Age",
            values_to = "indL")

ggplot() + 
  geom_bar(stat = "identity", size = 0.7, data = plotting,
           aes(x = as.numeric(interaction(Medium,Species)), y = indL, fill = Medium, color=Age))+
  geom_errorbar(data = h, aes(x=as.numeric(interaction(Medium,Species)),
                                               ymin=Total-sdev, ymax=Total+sdev))+
  scale_x_continuous(breaks=c(3, 8, 13, 18),labels=c("D. ambigua","D. galeata","D. pulex", "D. pulicaria"))+
  coord_cartesian(ylim = c(150, 3150))+
  scale_y_continuous(breaks = seq(0, 3250, 250))+
  scale_color_manual(values = c("#30cf4e","#e7d718","#00168a"))+
  scale_fill_manual(values = c("#Ff9400", "#E28fd9", "#D9dee0", "#39a6d6", "#Dc1906"),
                    labels = c("ADaM", "Groundwater", "Aerated tap water", "Hay water", "Manure water"))+
  guides(color = guide_legend(override.aes = list(fill = "white")))+
  labs(y = "Abundance (individuals/L)", 
       title = "Mean number of individuals per liter and age distribution within the population",
       x = "Species")+
  theme_minimal()+
  theme(axis.text.x = element_text(face="italic"))

ggplot(data, aes(x = Species, y = Total_indL, fill = Medium)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot() +
  scale_fill_manual(values = c("#Ff9400", "#E28fd9", "#D9dee0", "#39a6d6", "#Dc1906"),
                    labels = c("ADaM", "Groundwater", "Aerated tap water", "Hay water", "Manure water"))+
  labs(y = "Abundance (individuals/L)", 
       title = "Abundance in different media",
       x = "Species")+
  theme_minimal()+
  theme(axis.text.x = element_text(face="italic"))+
  stat_summary(geom = "errorbar", fun.min = mean, fun = mean, fun.max = mean, width = 0.75,
               linetype = "dotted", position = position_dodge())+
  coord_cartesian(ylim=c(0,3000))+
  stat_compare_means(label =  "p.signif", label.x = 1.5, hide.ns = TRUE, label.y = 3000)

  


data %>%
  group_by(Medium, Species) %>%
  summarise(p.value.sw = shapiro.test(Total_indL)$p.value) 

species <- c("D. pulex", "D. pulicaria", "D. galeata", "D. ambigua")


for (i in species) {
  print(i)
  dataF <- data %>% filter(Species == i)
  print(leveneTest(Total_indL ~ Medium, data = dataF)) 
  
  print(kruskal.test(Total_indL ~ Medium, data = dataF))
  print(dunnTest(Total_indL ~ Medium,
                 data=dataF,
                 method="bonferroni"))
  
  dataF %>% group_by(Medium) %>% summarise(mean = mean(Total_indL, na.rm = TRUE)) %>% as.data.frame() %>% print()
}

dataF %>% group_by(Medium) %>% summarise(mean = mean(Total_indL, na.rm = TRUE)) %>% as.data.frame()


