# Wilcoxon paired signed-rank test
account_before = c(509, 517, 502, 629, 830, 911, 847, 803, 727, 853, 757, 730, 774, 718, 904)
account_after = c(517, 508, 523, 730, 821, 940, 818, 821, 842, 842, 709, 688, 787, 780, 901)

wilcox.test(account_before, account_after, alt= 'less', exact = FALSE, paired= TRUE)

# permutation test
accounts = c(509, 517, 502, 629, 830, 911, 847, 803, 727, 853, 757, 730, 774, 718, 904, 517, 508, 523, 730, 821, 940, 818, 821, 842, 842, 709, 688, 787, 780, 901)
labels = factor(c(rep("before", 15), rep("after", 15)))
all_accounts = data.frame(accounts, labels)

t.test(accounts~labels, data=all_accounts, alt = 'greater', var.equal=TRUE, paired= TRUE) 

coin::oneway_test(accounts~labels, data=all_accounts, alt= 'greater', distribution="exact")
