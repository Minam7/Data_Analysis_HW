consumptions = read_csv("data/consumption.csv")

wilcox.test(consumptions$A, consumptions$B, paired= TRUE)
