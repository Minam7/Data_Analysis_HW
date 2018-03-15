characters = c("price", "design", "color")
male = c(301, 353, 558)
female = c(502, 155, 153)

product = data.frame(characters, male, female)
product_matrix <- product %>% .[,-1] %>% as.matrix() 

chisq.test(product_matrix)
