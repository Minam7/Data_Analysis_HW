city_size = c("small", "medium", "large")
always = c(151, 802, 753)
sometime = c(252, 603, 55)
never = c(603, 405, 408)

city_data = data.frame(city_size, always, sometime, never)
city_matrix <- city_data %>% .[,-1] %>% as.matrix() 

chisq.test(city_matrix)
