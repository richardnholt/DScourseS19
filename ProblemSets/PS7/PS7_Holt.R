library(dplyr)
library(mice)
library(stargazer)
library(tidyr)
library(HotDeckImputation)
library(magrittr)
table <- read.csv('wages.csv') %>%
  drop_na(tenure) %>%
  drop_na(hgc)
stargazer(table)

#Regression using complete cases
imp.deletion <- lm(logwage ~ hgc + tenure + I(tenure^2) + age + married, data=table, na.action = na.omit)
print(summary(imp.deletion))

#Mean imputation with missing logwages
table$mean_logwage <- table$logwage
table %<>% mutate(mean_logwage = logwage)
x <- mean(table$logwage,na.rm=TRUE)
table$mean_logwage[is.na(table$mean_logwage)] <- x

imp.mean <- lm(mean_logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=table, na.action=na.omit)
print(summary(imp.mean))

#Imputation from the complete cases regression
table$predict_logwage <- table$logwage
test <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=table, na.action = na.omit)
table$preds <- NA
table$preds[!is.na(table$hgc) & !is.na(table$tenure)] <- predict(test, table, na.action=na.exclude)
table$predict_logwage[is.na(table$logwage)] <- table$preds[is.na(table$logwage)]

imp.predict <- lm(predict_logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=table, na.action=na.omit)
print(summary(imp.predict))

#Mice package multiple imputation regression
imp.mice <- mice(table, seed = 123456)
fit = with(imp.mice, lm(logwage ~ hgc + tenure))
round(summary(pool(fit)),2)

# Final Stargazer table
stargazer(imp.deletion, imp.mean, imp.predict)
