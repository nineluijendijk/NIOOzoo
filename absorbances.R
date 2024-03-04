library(readxl)
library(tidyverse)
library(here)
library(car)

dataraw <- read_excel(here("data_raw/OnderzoekMedium.xlsx"), sheet = "Absorbances", range = "A1:F61", na = "NA")
# datatidy <- mutate(dataraw, "MeanAbsorbance" = rowMeans(dataraw[ , c(4:6)], na.rm = TRUE), A1=NULL, A2=NULL, A3=NULL)
datatidy <- pivot_longer(dataraw, cols = c("A1", "A2", "A3"), names_to = "Clone", values_to = "Absorbance")

regression <- read_excel(here("data_raw/Carbon concentration (based on chemostat C).xlsx"), sheet = "Carbon regression line", range = "A1:C9")

model <- lm(`Carbon Content (mg/L)` ~ `Light absorption (λ)`, data = regression) # Calculate regression curve

data <- mutate(datatidy, "Carbon_mgL" = (datatidy$Absorbance *  model$coefficients[[2]] + model$coefficients[[1]]) / 5 )

level_order <- c("KB", "KM", "KH", "GW", "ADaM") 

ggplot(data = data, aes(y = Carbon_mgL, x = factor(Medium, level = level_order), fill = Species))+
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Carbon concentration mg/L", 
       title = "Carbon concentration of the medium after two days",
       x = "Medium")

dataF <- filter(data, Species == "D. pulex")
  
dataF %>%
  group_by(Medium) %>%
  summarise(p.value.sw = shapiro.test(Carbon_mgL)$p.value) 
  
leveneTest(Carbon_mgL ~ Medium, data = dataF)[3] # variances arent equal, p = 0.03612178
  
  summary <- group_by(dataF, Medium) %>%
    summarise(count = n(),
              mean = mean(Carbon_mgL, na.rm = TRUE),
              sd = sd(Carbon_mgL, na.rm = TRUE),
              var = var(Carbon_mgL, na.rm=TRUE))
  
max(summary$var) / min(summary$var) # 3.814713, since the max variance is not 4 times larger than the min I will still conduct a one-way ANOVA
  
res_aov <- aov(Carbon_mgL ~ Medium, data = dataF)
summary(res_aov)
TukeyHSD(res_aov)







#ttest <- pairwise.t.test(data$Carbon_mgL, data$Medium,
#                p.adjust.method = "bonf")

#o <- as.numeric(ttest[[3]]) %>% order()
#as.numeric(ttest[[3]])[o]
