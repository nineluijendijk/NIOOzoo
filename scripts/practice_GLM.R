#install.packages("sjPlot")
library(tidyverse)
library(lme4)
library(sjPlot)

# Logistic regression, where something is either true or false (1 or 0)
d <- mutate(iris, versicolor = as.numeric(Species == "versicolor"))

m <- glm(versicolor ~ Sepal.Length + Sepal.Width, family = binomial, data = d) # it works the same as the linear models, but the family has to be specified

summary(m)

tab_model(m) # This gives the odds ratios, which is multiplication rather than addition: for every unit increase in sepal length the odds of the dependent variable is multiplied by 1.14, rather than simply increasing by 1.14 

# With a linear regression, values would be calculated by using the formula of the fitted line. Because of the odds ratios, it works a little different here. When sepal lenght is 1 and sepal width is 2, rather than:

sl <- 1
sw <- 2

3270.76 + 1.14 * sl + 0.04 * sw # wrong

# You use

3270.76 * 1.14 ^ sl * 0.04 ^ sw # different variables are multiplied and the measurements are the power of the coefficients (odds ratio) rather than multiplied

5.965866 / 1 # 5.97 is the result, which means for every 5.9 times the species is versicolor there's 1 time it's not, these are the odds

#################################

odds <- 3270.76 * 1.14 ^ sl * 0.04 ^ sw

odds / (1 + odds) # probability is 0.8564428 , 86% that a flower with a sepal length of 1 and a sepal width of 2 is a versicolor 


#the predict function can also do this

predict(m, type = "response", newdata = data.frame(Sepal.Length = 1, Sepal.Width = 2))

# as seen with the tab_model() function, the odds ratio of the sepal width is 0.04. As it is far under 1, this means this effect is negative. This also means the higher the value of sepal width, the lower the probability that this flower is a versicolor



# There is a difference between the coefficients found in summary() and tab_model(): In tab model, the "raw" odds ratios are displayed. In summary, the actual odds ratios are displayed, which are the log of the raw ones. So in the summary, intercept is 8.0928, and in the tab_model its 3270.76

exp(8.0928)
log(3270.76)

# It is important to keep track of which type of odd ratio you're working with

m_base <- glm(versicolor ~ 1, family = binomial, data = d) # base model 
m1 <- glm(versicolor ~ Sepal.Length + Sepal.Width, family = binomial, data = d) # more complicated model
m2 <- glm(versicolor ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, family = binomial, data = d) # even more complicated

# We use anova to see if there's differences between these models to find the best one. Deviance is something similar to sum of squares, so we want it as low as possible

anova(m_base, m1, m2, test = "Chisq") # third model is the best and significantly so

tab_model(m_base, m1, m2) # as petal.width increases the probability of it being versicolor decreases

plot_model(m2, type = "pred") %>% plot_grid # see how variables influence the probability

# R2 does not exist, but there is a pseudo R2. This should be reported as well as the deviance
