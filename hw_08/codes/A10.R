dickens_data_all <- bind_rows(unigram_data_all %>% ungroup() %>% mutate(author = 0) %>% select(book, chapter, author, tf, index, n),
                              bigram_data_all %>% ungroup() %>% mutate(author = 0) %>% select(book, chapter, author, tf, index, n))

austen_data_all <- bind_rows(woolf_unigram_data_all %>% ungroup() %>% mutate(author = 1) %>% select(book, chapter, author, tf, index, n),
                              woolf_bigram_data_all %>% ungroup() %>% mutate(author = 1) %>% select(book, chapter, author, tf, index, n))

dickens_train<- dickens_data_all %>% filter(book != "Oliver Twist")
dickens_test <- dickens_data_all %>% filter(book == "Oliver Twist")

austen_train <- austen_data_all %>% filter(book != "Mansfield Park")
austen_test <- austen_data_all %>% filter(book == "Mansfield Park")

train <- bind_rows(dickens_train, austen_train)
test <- bind_rows(dickens_test, austen_test)

glm_model <- glm(author ~ tf + index + n + chapter,family = binomial(link = 'logit'), data = train)
summary(glm_model)

cutoff = 0.2
test$predict = predict(glm_model, newdata = test, type = 'response')
test <- test %>% mutate(get = ifelse(predict < cutoff, 0, 1))

P <- test %>% filter(author == 1) %>% nrow()
N <- test %>% filter(author == 0) %>% nrow()
TP <- test %>% filter(author == 1 & get == 1) %>% nrow()
TN <- test %>% filter(author == 0 & get == 0) %>% nrow()
FP <- test %>% filter(author == 0 & get == 1) %>% nrow()
FN <- test %>% filter(author == 1 & get == 0) %>% nrow()

ACC <- (TP + TN)/(P + N)
cat("Accuracy: ", ACC)

FPR <- 1 - (TN/N)
cat("False Positive Rate: ", FPR)

TPR <- TP/P
cat("True Positive Rate: ", TPR)

