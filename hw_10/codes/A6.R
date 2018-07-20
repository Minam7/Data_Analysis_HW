# cleaning data
world_ec_cluster <- world_ec
world_ec_cluster[is.na(world_ec)] = 0
patt = "Africa|America|Asia|dividend|Euro|income|members|Middle|only|Sub|total|World"

world_ec_cluster <- world_ec_cluster %>% group_by(country) %>% 
  filter(!grepl(patt,country)) %>% 
  summarise_at(c(2:22), mean)

set.seed(1234)
clusters = kmeans(world_ec_cluster[,2:22], 3)

world_ec_cluster$cluster_no = as.integer(clusters$cluster)

iran_cluster = world_ec_cluster %>% 
  filter(country == 'Iran, Islamic Rep.')

cat("Iran cluster:", iran_cluster[1,23,1])

str(clusters)
