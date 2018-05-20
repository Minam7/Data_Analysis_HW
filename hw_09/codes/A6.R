# data preparation
sector_value <- sp500 %>% group_by(Date, Sector) %>% summarise(Open = mean(Open)) %>% ungroup()

sector_names <- sector %>% ungroup() %>% select(Sector) %>% distinct()

.data = sector_value %>% filter(Sector == sector_names[[1]][1]) %>% select(-Sector)
colnames(.data) = c("Date", sector_names[[1]][1])
sector_pca = .data

for(i in 2:nrow(sector_names)){
  .data = sector_value %>% filter(Sector == sector_names[[1]][i]) %>% select(-Sector)
  colnames(.data) = c("Date", sector_names[[1]][i])
  sector_pca = merge(sector_pca, .data, by = "Date")
}
sector_pca = sector_pca %>% merge(indexes, by = "Date") %>% select(-Date)

# pca
sector_stock_pca = prcomp(sector_pca, scale. = TRUE)
biplot(sector_stock_pca, scale = 1, pc.biplot = TRUE)

library(ggbiplot)
ggbiplot(sector_stock_pca, obs.scale = 1, var.scale = 1,
         ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
