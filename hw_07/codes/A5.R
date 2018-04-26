index = sample(x= 1:nrow(death_num),size = round(0.8*nrow(death_num)),replace = F)
train = death_num[index,] 
test =  death_num[-index,]

glm_model = glm(MannerOfDeath~., family = binomial(link = 'logit'), data = train)

test$prediction  = predict(glm_model, newdata = test, type = "response")
cutoff = 0.5
test <- test %>% mutate(get = ifelse(prediction < cutoff, 0, 1))

P <- test %>% filter(MannerOfDeath == 1) %>% nrow()
cat("P: ", P)

N <- test %>% filter(MannerOfDeath == 0) %>% nrow()
cat("N: ", N)

TP <- test %>% filter(MannerOfDeath == 1 & get == 1) %>% nrow()
cat("TP: ", TP)

TN <- test %>% filter(MannerOfDeath == 0 & get == 0) %>% nrow()
cat("TN: ", TN)

FP <- test %>% filter(MannerOfDeath == 0 & get == 1) %>% nrow()
cat("FP: ", FP)

FN <- test %>% filter(MannerOfDeath == 1 & get == 0) %>% nrow()
cat("FN: ", FN)

ACC <- (TP + TN)/(P + N)
cat("Accuracy: ", ACC)

FPR <- 1 - (TN/N)
cat("False Positive Rate: ", FPR)

TPR <- TP/P
cat("True Positive Rate: ", TPR)
