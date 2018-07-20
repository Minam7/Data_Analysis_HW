library(ape)

ec_sixty = bind_cols(world_ec_cluster,world_ed_cluster,world_hc_cluster)
rownames(ec_sixty) = ec_sixty$country
ec_sixty  <- ec_sixty  %>% select(-index,-index1,-index2,-country,-country1,-country2, -cluster,-cluster1,-cluster2,-PC1,-PC2,-PC11,-PC12,-PC21,-PC22)

hcut = cutree(hclust(dist(ec_sixty, method = "euclidean"),method = "complete"),k=3)
cat(hcut['Iran, Islamic Rep.'])

plot(as.phylo(clus_ehe), type = "fan", cex = 0.3,label.offset = 0)