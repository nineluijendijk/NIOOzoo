library(tidyverse)
library(readxl)
library(here)
library(car)
library(ggpmisc)

data <- read_excel(here("data/FlowThroughKalibreren_R.xlsx"))

data <- data %>% mutate("Speed" = WeightAfter - WeightBefore,
                "Speed" = Speed / (Time/60),
                "Unit" = "ml/min")

summary <- data %>% group_by(RPM) %>% summarize(SpeedM = mean(Speed),
                                                StDev = sd(Speed)) # Normally I'd use the mean, but using single values gives a different R2

summary %>% ggplot(aes(x = RPM, y = SpeedM))+
  geom_point()+
  geom_errorbar(aes(ymin = SpeedM - StDev, ymax = SpeedM + StDev), width = 0.2)+
  geom_smooth(method = "lm")+
  stat_poly_eq(use_label(c("eq", "R2")))+
  scale_x_continuous(breaks = c(seq(100, 900, 200)))+
  labs(y = "Flow in ml/min",
       title = "Calibration curve Flow Through pump")+
  theme_minimal()

# Using increase in volume

Far <- data[data$Distance == "Far",]
Middle <- data[data$Distance == "Middle",]
Close <- data[data$Distance == "Close",]
Far$Difference %>% shapiro.test() # p = 0.6787
Middle$Difference %>% shapiro.test() # p = 0.8958
Close$Difference %>% shapiro.test() # p = 0.9844

leveneTest(data$Difference, data$Distance) # p = 0.9035
leveneTest(data$Difference, data$Glass) # p = 0.9758

aov(Difference ~ Distance, data) %>% summary.aov() # p = 0.656
aov(Difference ~ Glass, data) %>% summary.aov() # p = 0.571

filtered <- data %>% filter(Glass == "D" | Glass == "H")
t.test(formula = filtered$Difference ~ filtered$Glass) # p = 0.1665

for (i in seq(100, 900, 200)) {
  filtered <- data %>% filter(RPM == i)
  aov(Difference ~ Distance, filtered) %>% summary.aov() %>% print() # p around 0.7
}


# Using flow speed


Far <- data[data$Distance == "Far",]
Middle <- data[data$Distance == "Middle",]
Close <- data[data$Distance == "Close",]
Far$Speed %>% shapiro.test() # p = 0.4568
Middle$Speed %>% shapiro.test() # p = 0.6826
Close$Speed %>% shapiro.test() # p = 0.879

leveneTest(data$Speed, data$Distance) # p = 0.8358
leveneTest(data$Speed, data$Glass) # p = 0.9242

aov(Speed ~ Distance, data) %>% summary.aov() # p = 0.793
aov(Speed ~ Glass, data) %>% summary.aov() # p = 0.876

filtered <- data %>% filter(Glass == "D" | Glass == "H")
t.test(formula = filtered$Speed ~ filtered$Glass) # p = 0.3038

for (i in seq(100, 900, 200)) {
  filtered <- data %>% filter(RPM == i)
  aov(Speed ~ Distance, filtered) %>% summary.aov() %>% print() # p around 0.7
}

