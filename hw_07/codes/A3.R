library(boot)

glm_model = glm(MannerOfDeath~., family = binomial(link = 'logit'), data = death_num)
summary(glm_model)


death_num <- death_num %>% select(-DayOfWeekOfDeath, -MonthOfDeath)
glm_model = glm(MannerOfDeath~., family = binomial(link = 'logit'), data = death_num)
summary(glm_model)

glm.diag.plots(glm_model, glmdiag = glm.diag(glm_model))