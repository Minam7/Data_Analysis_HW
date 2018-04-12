summary(reg_model)

# 0.05 error
new_tops <- tops %>% select(-TotRmsAbvGrd, -GarageArea)

new_reg_model <- lm(SalePrice~., data = new_tops)

summary(new_reg_model)

new_predicted_price = predict(new_reg_model)

ggplot(house_numeric, aes(x=house_numeric$SalePrice, y=new_predicted_price)) + geom_point(colour = "brown3") + 
  geom_abline(slope = 1) + 
  geom_smooth(method="lm", se=FALSE, colour = "green") +
  xlab("Real price") +
  ylab("Predicted price") + 
  ggtitle("New Predicted price vs. Real price")
