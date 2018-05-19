appl <- sp500 %>% filter(stock_name == "AAPL") %>% arrange(desc(Date)) %>% 
  select(-stock_name, -Name, -Sector, -Volume, -Close)


# k = 1
appl$k1 = c(appl[-c(1),]$Open, 0)
lm = lm(data = appl, formula = Open~k1)
ans <- data.frame(1, summary(lm)$r.squared)
names(ans)<-c("k","R Squared")

# k = 2
appl$k2 = c(appl[-c(1,2),]$Open, 0,0)
lm = lm(data = appl, formula = Open~k1+k2)
ans_1 <- data.frame(2, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 3
appl$k3 = c(appl[-c(1,2,3),]$Open, 0,0,0)
lm = lm(data = appl, formula = Open~k1+k2+k3)
ans_1 <- data.frame(3, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 4
appl$k4 = c(appl[-c(1,2,3,4),]$Open, 0,0,0,0)
lm = lm(data = appl, formula = Open~k1+k2+k3+k4)
ans_1 <- data.frame(4, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 5
appl$k5 = c(appl[-c(1,2,3,4,5),]$Open, 0,0,0,0,0)
lm = lm(data = appl, formula = Open~k1+k2+k3+k4+k5)
ans_1 <- data.frame(5, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 6
appl$k6 = c(appl[-c(1,2,3,4,5,6),]$Open, 0,0,0,0,0,0)
lm = lm(data = appl, formula = Open~k1+k2+k3+k4+k5+k6)
ans_1 <- data.frame(6, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 7
appl$k7 = c(appl[-c(1,2,3,4,5,6,7),]$Open, 0,0,0,0,0,0,0)
lm = lm(data = appl, formula = Open~k1+k2+k3+k4+k5+k6+k7)
ans_1 <- data.frame(7, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 8
appl$k8 = c(appl[-c(1,2,3,4,5,6,7,8),]$Open, 0,0,0,0,0,0,0,0)
lm = lm(data = appl, formula = Open~k1+k2+k3+k4+k5+k6+k7+k8)
ans_1 <- data.frame(8, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 9
appl$k9 = c(appl[-c(1,2,3,4,5,6,7,8,9),]$Open, 0,0,0,0,0,0,0,0,0)
lm = lm(data = appl, formula = Open~k1+k2+k3+k4+k5+k6+k7+k8+k9)
ans_1 <- data.frame(9, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

# k = 10
appl$k10 = c(appl[-c(1,2,3,4,5,6,7,8,9,10),]$Open, 0,0,0,0,0,0,0,0,0,0)
lm = lm(data = appl, formula = Open~k1+k2+k3+k4+k5+k6+k7+k8+k9+k10)
ans_1 <- data.frame(10, summary(lm)$r.squared)
names(ans_1)<-c("k","R Squared")
ans <- ans %>% rbind(ans_1)

knitr::kable(ans)

