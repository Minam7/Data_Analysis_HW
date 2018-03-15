# permutation test
stores = c(50, 50, 60, 70, 75, 80, 90, 85, 55, 75, 80, 90, 105, 65)
labels = factor(c(rep("classic", 8), rep("modern", 6)))
all_stores = data.frame(stores, labels)

t.test(stores~labels, data=all_stores, var.equal=TRUE) 

coin::oneway_test(stores~labels, data=all_stores, distribution="exact")

# Wilcoxon–Mann–Whitney rank-sum test
classic_stores = c(50, 50, 60, 70, 75, 80, 90, 85)
modern_stores = c(55, 75, 80, 90, 105, 65)

wilcox.test(classic_stores, modern_stores, alternative = "two.sided", exact = FALSE, correct = FALSE)
