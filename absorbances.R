library(readxl)
library(tidyverse)
library(here)
library(multcomp)

dataraw <- read_excel(here("data_raw/OnderzoekMedium.xlsx"), sheet = "Absorbances", range = "A1:F61", na = "NA")
# datatidy <- mutate(dataraw, "MeanAbsorbance" = rowMeans(dataraw[ , c(4:6)], na.rm = TRUE), A1=NULL, A2=NULL, A3=NULL)
datatidy <- pivot_longer(dataraw, cols = c("A1", "A2", "A3"), names_to = "Sample", values_to = "Absorbance")


regression <- read_excel(here("data_raw/Carbon concentration (based on chemostat C).xlsx"), sheet = "Carbon regression line", range = "A1:C9")

model <- lm(`Carbon Content (mg/L)` ~ `Light absorption (λ)`, data = regression)

data <- mutate(datatidy, "Carbon_mgL" = (datatidy$Absorbance *  model$coefficients[[2]] + model$coefficients[[1]]) / 5 )

level_order <- c("KB", "KM", "KH", "GW", "ADaM") 

ggplot(data = data, aes(y = Carbon_mgL, x = factor(Medium, level = level_order), fill = Species))+
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Carbon concentration mg/L", 
       title = "Carbon concentration of the medium after two days",
       x = "Medium")

group_by(data, Medium) %>%
  summarise(count = n(),
            mean = mean(Carbon_mgL, na.rm = TRUE),
            sd = sd(Carbon_mgL, na.rm = TRUE))

res.aov <- aov(Carbon_mgL ~ Medium, data = data)
summary(res.aov)
TukeyHSD(res.aov)
