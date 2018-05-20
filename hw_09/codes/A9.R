library("EBImage")

pic = flip(readImage("images/stock.jpg"))
red_data <- imageData(pic)[,,1]
green_data <- imageData(pic)[,,2]
blue_data <- imageData(pic)[,,3]

# red component
pca.img_red = prcomp(red_data, scale=TRUE)
plot(summary(pca.img_red)$importance[3,], type="l",
     ylab="%variance explained", xlab="nth component (decreasing order)", main = "Red Component") + 
  abline(h=0.99,col="red");abline(v = 89,col="red",lty=3)
# to capture 99% of the variance, we need the first 89 components
sum((pca.img_red$sdev^2)[1:89])/sum((pca.img_red$sdev^2))
chosen.components = 1:89
feature.vector_red = pca.img_red$rotation[,chosen.components]
compact.data_red = t(feature.vector_red) %*% t(red_data)
approx.img_red = t(feature.vector_red %*% compact.data_red)
pic[ , , 1] = approx.img_red

# green component
pca.img_green = prcomp(green_data, scale=TRUE)
plot(summary(pca.img_green)$importance[3,], type="l",
     ylab="%variance explained", xlab="nth component (decreasing order)", main = "Green Component") + 
  abline(h=0.99,col="red");abline(v = 114,col="red",lty=3)
# to capture 99% of the variance, we need the first 114 components
sum((pca.img_green$sdev^2)[1:114])/sum((pca.img_green$sdev^2))
chosen.components = 1:114
feature.vector_green = pca.img_green$rotation[,chosen.components]
compact.data_green = t(feature.vector_green) %*% t(green_data)
approx.img_green = t(feature.vector_green %*% compact.data_green)
pic[ , , 2] = approx.img_green

# blue component
pca.img_blue = prcomp(blue_data, scale=TRUE)
plot(summary(pca.img_blue)$importance[3,], type="l",
     ylab="%variance explained", xlab="nth component (decreasing order)", main = "Blue Component") + 
  abline(h=0.99,col="red");abline(v = 120,col="red",lty=3)
# to capture 99% of the variance, we need the first 120 components
sum((pca.img_blue$sdev^2)[1:120])/sum((pca.img_blue$sdev^2))
chosen.components = 1:120
feature.vector_blue = pca.img_blue$rotation[,chosen.components]
compact.data_blue = t(feature.vector_blue) %*% t(blue_data)
approx.img_blue = t(feature.vector_blue %*% compact.data_blue)
pic[ , , 3] = approx.img_blue

plot(flip(pic))
