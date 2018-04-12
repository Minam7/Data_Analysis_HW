sample = sample(1:length(new_tops$SalePrice), size = length(new_tops$SalePrice)*0.8)

train_set = new_tops[sample,]
test_set = new_tops[-sample,]

train_reg_model = lm(SalePrice~., data = train_set)
summary(train_reg_model)

predicted = predict(train_reg_model , test_set)

ggplot(test_set, aes(x=test_set$SalePrice, y=predicted)) + geom_point(colour = "brown1") + 
  geom_abline(slope = 1) + 
  geom_smooth(method="lm", se=FALSE, colour = "green") +
  xlab("Real price") +
  ylab("Predicted price") + 
  ggtitle("New Predicted price vs. Real price with Cross Validation")
