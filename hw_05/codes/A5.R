tv = read_csv("data/tv.csv")
tv_matrix <- data.matrix(tv)
friedman.test(tv_matrix)
