iran_ec_m <- world_ec %>% filter(country == 'Iran, Islamic Rep.')
iran_ec_m[is.na(iran_ec_m)] = 0

glm_irn = glm(data = iran_ec_m[,3:23], formula = `2016`~.)
summary(glm_irn)

glm_irn = glm(data = iran_ec_m[,3:23], formula = `2016`~`2007` + `2014`)
summary(glm_irn)

glm_irn = glm(data = iran_ec_m[,3:23], formula = `2016`~`2007`)
summary(glm_irn)

lm_irn = lm(data = iran_ec_m[,3:23], formula = `2016`~.)
summary(lm_irn)
