predicted_price = predict(reg_model)

ggplot(house_numeric, aes(x=house_numeric$SalePrice, y=predicted_price)) + geom_point(colour = "brown3") + 
  geom_abline(slope = 1) + 
  geom_smooth(method="lm", se=FALSE, colour = "green") +
  xlab("Real price") +
  ylab("Predicted price") + 
  ggtitle("Predicted price vs. Real price")
