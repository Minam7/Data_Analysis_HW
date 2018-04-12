plot(residuals(new_reg_model))
par(mfrow=c(2,2))

plot(new_reg_model)
par(mfrow=c(1,1))

cor.test(house_numeric$SalePrice, residuals(new_reg_model))
