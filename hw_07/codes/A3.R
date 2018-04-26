glm_model = glm(MannerOfDeath~., family = binomial(link = 'logit'), data = death_num)
summary(glm_model)

death_num <- death_num %>% select(-MonthOfDeath, -DayOfWeekOfDeath, -MaritalStatus)

glm_model = glm(MannerOfDeath~., family = binomial(link = 'logit'), data = death_num)
summary(glm_model)
