cor.test(worldwide$depth, worldwide$mag, method = 'spearman')

eq_matrix <- worldwide %>% select(depth, mag) %>% filter(depth > 0) %>% as.matrix() 

chisq.test(eq_matrix)
