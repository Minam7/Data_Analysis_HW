appl_ohlcv <- read_csv("data/stock_dfs/AAPL.csv") %>% arrange(Date) %>% select(-Date)
appl <- appl %>% arrange(Date)

appl_pca = prcomp(appl_ohlcv, scale. = TRUE)
appl_pca1 = appl_pca$x %>% as.data.frame() %>% select(PC1)

appl$pca = appl_pca1$PC1

# k = 1
lm = lm(data = appl, formula = pca~k1)
ans <- data.frame(1, summary(lm)$r.squared)
names(ans)<-c("k","R Squared")

# k = 2
lm = lm(data = appl, formula = pca~k1+k2)
ans_1 <- data.frame(2, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 3
lm = lm(data = appl, formula = pca~k1+k2+k3)
ans_1 <- data.frame(3, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 4
lm = lm(data = appl, formula = pca~k1+k2+k3+k4)
ans_1 <- data.frame(4, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 5
lm = lm(data = appl, formula = pca~k1+k2+k3+k4+k5)
ans_1 <- data.frame(5, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 6
lm = lm(data = appl, formula = pca~k1+k2+k3+k4+k5+k6)
ans_1 <- data.frame(6, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 7
lm = lm(data = appl, formula = pca~k1+k2+k3+k4+k5+k6+k7)
ans_1 <- data.frame(7, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 8
lm = lm(data = appl, formula = pca~k1+k2+k3+k4+k5+k6+k7+k8)
ans_1 <- data.frame(8, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 9
lm = lm(data = appl, formula = pca~k1+k2+k3+k4+k5+k6+k7+k8+k9)
ans_1 <- data.frame(9, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 10
lm = lm(data = appl, formula = pca~k1+k2+k3+k4+k5+k6+k7+k8+k9+k10)
ans_1 <- data.frame(10, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

knitr::kable(ans)