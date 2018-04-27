library(h2o)

h2o.init()

y <- "MannerOfDeath"
x <- c("ResidentStatus", "edu", "AgeRecode27", "PlaceOfDeathAndDecedentsStatus", 
       "InjuryAtWork", "MannerOfDeath", "MethodOfDisposition", "MaritalStatus",
       "ActivityCode", "PlaceOfInjury", "Sex", "RaceRecode3")

h_death_num <- as.h2o(death_num)

hglm = h2o.glm(y = y, x = x,
                training_frame = h_death_num, family="binomial", link = "logit", lambda = 0,
               compute_p_values = TRUE, nfolds = 5)
hglm

