pca = prcomp(world_ec_cluster[,2:22], scale. = TRUE)
x = pca$x[,1:2]
world_ec_cluster <- world_ec_cluster %>%  bind_cols(x %>% as.data.frame())

clusters = kmeans(world_ec_cluster[,24:25], 3)
world_ec_cluster$cluster_no = as.integer(clusters$cluster)

iran_cluster_pca = world_ec_cluster %>% 
  filter(country == 'Iran, Islamic Rep.')

cat("Iran cluster:", iran_cluster[1,23,1])

str(clusters)
