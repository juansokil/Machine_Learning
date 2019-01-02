####Librerias####

library(readr)
library(broom)
library(dplyr)

####Base####


base <- read.delim2('./Machine_Learning/Regression/base.txt', sep='\t', encoding = 'Latin-1')

train <- base %>% filter (Year==2015)
test <- base %>% filter (Year==2016)








####MODELO INICIAL
###Modelo Lineal###
fit_lm <- lm(publicaciones ~ investigadores, data=train)

###coeficientes###
coef(fit_lm)

###valores predichos###
fitted.values(fit_lm)

# Mean of the residuals
###para que sirve??
mean(residuals(fit_lm))

# Compute RMSE
RMSE=sqrt(sum(residuals(fit_lm)^2) / df.residual(fit_lm))



# lm to data frame
lm_vars <- augment(fit_lm)

# Compute R-squared
lm_vars %>%
  summarize(var_y = var(publicaciones), var_e = var(.resid)) %>%
  mutate(R_squared = 1 -(var_e/var_y))
  
###Var_e = SSE
###Var_y = SST




#The leverage of an observation in a regression model is defined entirely in terms of the distance of that observation from the mean of the explanatory variable. That is, observations close to the mean of the explanatory variable

lm_vars %>%
arrange(desc(.hat))

#The influence of an observation depends not only on its leverage, but also on the magnitude of its residual.

lm_vars %>%
arrange(desc(.cooksd))

###ORDENAR POR AMBOS LOS MAS ALTOS DE LEVERAGE Y LOS MAS BAJOS DE COOK
lm_vars %>%
arrange(desc(.hat), .cooksd)


###Modelo Logineal###
fit_llm <- lm(log(publicaciones) ~ log(investigadores), data=train)


predict(fit, train)



