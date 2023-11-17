# install.packages("lme4")
# install.packages("lmerTest")
# https://www.youtube.com/watch?v=oI1_SV1Rpfc
library(lme4) # another package that works is nlme
library(lmerTest)
library(readxl)
library(here)
library(tidyverse)
library(sjPlot)

df <- read_excel(here("data/oefenenLMM.xlsx")) # get data

# lmer(variable of interest ~ variable it depends on + (0 or 1 to allow for unique intersect | variable with random slope), REML is set to false so we can calculate the likelihood later with ANOVA, data = df)

M1 <- lmer(Weight ~ Weeks + (1|Subjects), REML = T, data = df) # the 1 is used to show that all persons have their own intersect
summary(M1)
ranef(M1) # how much larger is the intercept than the overall intercept

M2 <- lmer(Weight ~ Weeks + (1 + Weeks|Subjects), REML = F, data = df) # random slope on "Weeks", because different individuals probably respond differently to diets, so each should have their own slope
summary(M2) # there is a negative correlation between slopes and intercepts (-0.88), which is expected as bigger people lose weight faster
ranef(M2) # how much larger is the intercept than the overall intercept and what is each person's slope

M3 <- lmer(Weight ~ Weeks + (0 + Weeks|Subjects), REML = F, data = df) # the 0 is used to show that all persons have the same intercept (which is not appropriate as they all have a different starting weight). This could be used however when the y axis is weight change rather than weight
summary(M3)
ranef(M3) # how much larger is the intercept than the overall intercept

M4 <- lmer(Weight ~ Weeks + Diet + (1|Subjects), REML = F, data = df) # the 0 is used to show that all persons have the same intercept (which is not appropriate as they all have a different starting weight). This could be used however when the y axis is weight change rather than weight
summary(M4) # the intercept gives the values for diet A, which then can be compared using the estimate for diet B
ranef(M4) # how much larger is the intercept than the overall intercept

M5 <- lmer(Weight ~ Weeks * Diet + (1|Subjects), REML = F, data = df) # we use * to check for significant difference in weight loss per week per diet
summary(M5) # the slope (Weeks) shows that people on diet A lose 3.4 kg per week, Weeks:DietB shows that this number is 0.85 higher for persons on diet B (which is this case would be -3.4+0.85= -2.55 kg per week). The P value is 0.066 and diet A and B are not significantly different
ranef(M5) # how much larger is the intercept than the overall intercept
# as people with higher starting weight lose more weight initially, it would be better to spread out the different diets per starting weight as well (rather than the 2 heaviest people on diet A and the 2 lightest people on diet B)

M6 <- lmer(Weight ~ Weeks + Diet + (1+ Weeks|Subjects), REML = F, data = df)
M7 <- lmer(Weight ~ Weeks * Diet + (1+ Weeks|Subjects), REML = F, data = df)
# This code does not work, as lmer doesn't allow for random slopes here, as the model is complicated enough as is

M6 <- lmer(Weight ~ Weeks * Diet + Gender + (1|Subjects), REML = F, data = df) # 
summary(M6) # the intercept gives the values for diet A / gender Female, which then can be compared using the estimate for diet B / gender Male. The Estimate of gender M shows the difference in average weight over time between Female and Male (shown below)
ranef(M6)

AF <- df %>% subset(Diet == "A" & Gender == "F") %>% pull(Weight) %>% mean()
AM <- df %>% subset(Diet == "A" & Gender == "M") %>% pull(Weight) %>% mean()
BF <- df %>% subset(Diet == "B" & Gender == "F") %>% pull(Weight) %>% mean()
BM <- df %>% subset(Diet == "B" & Gender == "M") %>% pull(Weight) %>% mean()

(AM - AF)/2 + (BM - BF)/2
# So its the averages of their weight per diet, the differences between these and then added up with the persons from the other diet to find the total

M0 <- lm(Weight ~ Weeks, data = df) # simple linear regression
summary(M0)
tab_model(M0)

anova(M4, M5)
# The first model is the null model, with simpler parameters. it shows that the log likelihood of the second model is higher, so this model is better fitted to the data. However, as the p value of the chi squared test is 0.06 (> 0.05), the difference is not statistically significant.
