# make data ready
.data <- read_csv(textpath[1]) %>% select(Date, Open)
colnames(.data) =c("Date", name[1])
sp500_pca = .data

for(i in 2:length(name)){
  .data <- read_csv(textpath[i]) %>% select(Date, Open)
  colnames(.data) =c("Date", name[i])
  sp500_pca = merge(sp500_pca, .data)
}

# pca
stock_pca = prcomp(sp500_pca %>% select(-Date), scale. = TRUE)

plot(summary(stock_pca)$importance[3,], type="l",
     ylab="% variance explained", xlab="nth component (decreasing order)") + 
  abline(h=0.98,col="red");abline(v = 25,col="red",lty=3)

sum((stock_pca$sdev^2)[1:3])/sum((stock_pca$sdev^2))
