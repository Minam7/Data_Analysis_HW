indexes <- indexes %>% mutate(profit = 100*(SP500 - lag(SP500))/lag(SP500))

indexes %>% 
  ggplot(aes(profit, fill=..count..)) + geom_histogram(binwidth = 1) +
  scale_fill_gradient(low="darkgreen", high="gold") + 
  ggtitle("SP500 Profit Distribution")

indexes <- indexes %>% mutate(loss_gain = ifelse(profit > 0, 1, 0))

pcas <- stock_pca$x %>% as.data.frame() %>% select(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10)

# indexes_pca <- sp500_pca %>% merge(pcas) %>% merge(indexes %>% merge(sp500_pca))
indexes_pca <- indexes %>% merge(sp500_pca, by = "Date") %>% merge(pcas)

glm = glm(data = indexes_pca, formula = loss_gain ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, family = binomial(link = 'logit'))
summary(glm)

indexes_pca$prediction  = predict(glm, newdata = indexes_pca, type = "response")
indexes_pca <- indexes_pca %>% mutate(get = ifelse(prediction < 0.5, 0, 1))
P <- indexes_pca %>% filter(loss_gain == 1) %>% nrow()
N <- indexes_pca %>% filter(loss_gain == 0) %>% nrow()
TP <- indexes_pca %>% filter(loss_gain == 1 & get == 1) %>% nrow()
TN <- indexes_pca %>% filter(loss_gain == 0 & get == 0) %>% nrow()
ACC <- (TP + TN)/(P + N)
cat("Accuracy: ", ACC)
