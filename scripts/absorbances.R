library(readxl)
library(tidyverse)
library(here)
library(car)
library(FSA)
library(ggstatsplot)
library(ggpubr)

dataraw <- read_excel(here("data_raw/OnderzoekMedium.xlsx"), sheet = "Absorbances", range = "A1:F61", na = "NA")
# datatidy <- mutate(dataraw, "MeanAbsorbance" = rowMeans(dataraw[ , c(4:6)], na.rm = TRUE), A1=NULL, A2=NULL, A3=NULL)
datatidy <- pivot_longer(dataraw, cols = c("A1", "A2", "A3"), names_to = "Clone", values_to = "Absorbance")

regression <- read_excel(here("data_raw/Carbon concentration (based on chemostat C).xlsx"), sheet = "Carbon regression line", range = "A1:C9")

model <- lm(`Carbon Content (mg/L)` ~ `Light absorption (λ)`, data = regression) # Calculate regression curve

data <- mutate(datatidy, "Carbon_mgL" = (datatidy$Absorbance *  model$coefficients[[2]] + model$coefficients[[1]]) / 5 ) 

data <- data %>% filter(Carbon_mgL > 0)

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
               linetype = "dotted", position = position_dodge())+
  stat_compare_means(label =  "p.signif", label.x = 1.5, hide.ns = TRUE)

data %>% filter(! Medium == "GW" | ! Species == "D. ambigua") %>%
  group_by(Medium, Species) %>%
  summarise(p.value.sw = shapiro.test(Carbon_mgL)$p.value) 

species <- c("D. pulex", "D. pulicaria", "D. galeata", "D. ambigua")


for (i in species) {
  print(i)
  dataF <- data %>% filter(Species == "D. pulex")
  print(leveneTest(Carbon_mgL ~ Medium, data = dataF)) 
  
  print(kruskal.test(Carbon_mgL ~ Medium, data = dataF))
  print(dunnTest(Carbon_mgL ~ Medium,
                 data=dataF,
                 method="bonferroni"))
  
  ggbetweenstats(data = dataF, y = Carbon_mgL, x = Medium, type = "nonparametric", p.adjust.method = "bonferroni")
  
}

dummydf <- data.frame(a = rep(c(1, 2), each=2), #create dummy dataframe for the dummy plot
                      b = c(1, 3, 4, 7),
                      Measure = rep(c("Median", "Mean"), each=2))

ggplot(dummydf, aes(x=a, y=b, group=Measure))+ #create dummy plot to obtain its legend
  geom_line(aes(linetype=Measure))+
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme_minimal()

dataF %>% group_by(Medium) %>% summarise(mean = mean(Carbon_mgL, na.rm = TRUE)) %>% as.data.frame()
