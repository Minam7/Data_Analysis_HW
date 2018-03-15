store_num = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5)
price = c(510, 925, 730, 720, 735, 745, 930, 753, 875, 754, 685, 610, 105)
color = c("white", "blue", "red", "white", "blue", "red", "white", "blue", "red", "white", "blue", "red", "white")
color_data = data.frame(store_num, price, color)

kruskal.test(color ~ price, data = color_data)

color_matrix = data.matrix(color_data)
friedman.test(color_matrix)
