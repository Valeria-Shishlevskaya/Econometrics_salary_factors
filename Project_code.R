library("readxl") # импорты библиотек оставьте активными, так, чтобы код был во
library("DescTools")
library("plotrix")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("lmtest")


df <- data31
summary(df)

cor(df$wage, df$educ, method = "pearson")
corr <- cor.test(df$wage, df$educ)
corr

cor(df$wage, df$exper, method = "pearson")
corr <- cor.test(df$wage, df$exper)
corr

model1 <- lm(wage ~ educ + exper + I(exper^2) + urban, data = df)
summary(model1) 

df <- data31
hist(df$wage, col = "lightblue")
hist(df$educ, col = "orange")
hist(df$exper, col = "pink")
hist(df$urban, col = "yellow")


model1 <- lm(wage ~ educ + exper + urban, data = df)
summary(model1) 
confint(model1, level=0.95)

model2 <- lm(wage ~ urban + I(exper - educ), data = df)
summary(model2)
confint(model2, level=0.95)

library(rstatix)

model.res = resid(model1)
plot(fitted(model1), model.res) 
abline(0,0) 

shapiro.test(model.res[1:5000]) 

qqPlot(model1, labels = row.names(df), simulate = TRUE, main = 'График Q-Q')

crPlots((model1))
dwtest(model1)

# Гомоскедастичность в данных: Тест Бройша-Пагана
bptest(model1)
spreadLevelPlot(model1)

dt <- dada30
summary(dt)


model2 = glm(formula = wage_above ~ exper + I(exper^2) + educ + urban, data = dt, family = binomial)
print(summary(model2))
exp(model2$coefficients) 
exp(confint(model2)) 

library(margins)

# Расчет предельных эффектов
marginal_effects <- margins(model2)

# Вывод предельных эффектов
summary(marginal_effects)

library('crch')
tr <- 0
model_tr <- crch( wage ~ exper + educ + urban + I(exper^2), 
                  data = df,
                  left = tr,
                  truncated  = TRUE,
                  dist  = "gaussian")

summary(model_tr)

rbind(linear = AIC(model2), poly = AIC(model1), 
      truncated = AIC(model_tr))

     