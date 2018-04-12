library(knitr)
library(corrplot)
library(psych)

house_numeric <- house %>% select_if(is.numeric)
all_cors = cor(house_numeric)
knitr::kable(all_cors)

corrplot(all_cors, method = "color", tl.cex = 0.75/par("cex"), cl.cex = 0.75/par("cex"))

knitr::kable(sort(corr.test(all_cors, adjust = "none", alpha = 0.05)$p[38,]))

knitr::kable(sort(abs(all_cors['SalePrice',]), decreasing = TRUE)[2:11])