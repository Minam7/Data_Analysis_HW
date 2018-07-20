# health
# 5
world_hc <- wdi_data %>% filter(`Indicator Code`== 'SH.DYN.AIDS.FE.ZS') %>% 
  select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]')) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.DYN.AIDS.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.HIV.1524.MA.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.HIV.1524.FE.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.ANM.ALLW.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.PRG.ANEM') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.ANM.NPRG.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.ANM.CHLD.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.DTH.MORT') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.DTH.IMRT') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.VAC.TTNS.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.DYN.MORT') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.DYN.NMRT') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.STA.MMRT') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.MMR.RISK.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.XPD.GHED.PP.CD') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.XPD.GHED.PC.CD') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.XPD.CHEX.PC.CD') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.HIV.INCD') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.HIV.0014') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]')))

world_hc_m <- world_hc %>% 
  melt(id.vars=c('country', 'index')) %>% 
  filter(!is.na(value)) %>% 
  group_by(index)

plots <- world_hc_m %>% 
  do(
    plots = ggplot(.,aes(x = variable, y = value, color = index)) + geom_boxplot() +
      xlab("Year") + ggtitle("Worldwide Health vs. Iran") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_line(data = subset(.,country == 'Iran, Islamic Rep.'),aes(x = variable, y = value, group = 1, color = 'Iran, Islamic Rep.'))
  )
plots$plots

# 6
# cleaning data
world_hc_cluster <- world_hc
world_hc_cluster[is.na(world_hc)] = 0
patt = "Africa|America|Asia|dividend|Euro|income|members|Middle|only|Sub|total|World"

world_hc_cluster <- world_hc_cluster %>% group_by(country) %>% 
  filter(!grepl(patt,country)) %>% 
  summarise_at(c(2:22), mean)

set.seed(134)
clusters = kmeans(world_hc_cluster[,2:22], 3)

world_hc_cluster$cluster_no = as.integer(clusters$cluster)

iran_cluster = world_hc_cluster %>% 
  filter(country == 'Iran, Islamic Rep.')

cat("Iran cluster:", iran_cluster[1,23,1])

str(clusters)

# 7
pca = prcomp(world_hc_cluster[,2:22], scale. = TRUE)
x = pca$x[,1:2]
world_hc_cluster <- world_hc_cluster %>%  bind_cols(x %>% as.data.frame())

clusters = kmeans(world_hc_cluster[,24:25], 3)
world_hc_cluster$cluster_no = as.integer(clusters$cluster)

iran_cluster_pca = world_hc_cluster %>% 
  filter(country == 'Iran, Islamic Rep.')

cat("Iran cluster:", iran_cluster[1,23,1])

str(clusters)

# 8
iran_hc_m <- world_hc %>% filter(country == 'Iran, Islamic Rep.')
iran_hc_m[is.na(iran_hc_m)] = 0

lm_irn = lm(data = iran_hc_m[,3:23], formula = `2016`~.)
summary(lm_irn)

# education
# 5
world_ed <- wdi_data %>% filter(`Indicator Code`== 'SE.ENR.PRIM.FM.ZS') %>% 
  select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]')) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.PRM.DURS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.XPD.TERT.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.XPD.PRIM.Z') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.XPD.SECO.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.PRM.ENRR') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.PRM.TCHR') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.PRM.ENRL') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.PRE.ENRL.TC.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.PRE.ENRR') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.SEC.PROG.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.SEC.ENRL.GC') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.PRM.PRSL.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.SEC.AGES') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.PRE.ENRL.TC.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.PRM.TENR.FE') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.SEC.ENRL') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.PRM.NENR') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.SEC.DURS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SE.PRM.AGES') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]')))

world_ed_m <- world_ed %>% 
  melt(id.vars=c('country', 'index')) %>% 
  filter(!is.na(value)) %>% 
  group_by(index)

plots <- world_ed_m %>% 
  do(
    plots = ggplot(.,aes(x = variable, y = value, color = index)) + geom_boxplot() +
      xlab("Year") + ggtitle("Worldwide Education vs. Iran") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_line(data = subset(.,country == 'Iran, Islamic Rep.'),aes(x = variable, y = value, group = 1, color = 'Iran, Islamic Rep.'))
  )
plots$plots

# 6
# cleaning data
world_ed_cluster <- world_ed
world_ed_cluster[is.na(world_ed)] = 0
patt = "Africa|America|Asia|dividend|Euro|income|members|Middle|only|Sub|total|World"

world_ed_cluster <- world_ed_cluster %>% group_by(country) %>% 
  filter(!grepl(patt,country)) %>% 
  summarise_at(c(2:22), mean)

set.seed(134)
clusters = kmeans(world_ed_cluster[,2:22], 3)

world_ed_cluster$cluster_no = as.integer(clusters$cluster)

iran_cluster = world_ed_cluster %>% 
  filter(country == 'Iran, Islamic Rep.')

cat("Iran cluster:", iran_cluster[1,23,1])

str(clusters)

# 7
pca = prcomp(world_ed_cluster[,2:22], scale. = TRUE)
x = pca$x[,1:2]
world_ed_cluster <- world_ed_cluster %>%  bind_cols(x %>% as.data.frame())

clusters = kmeans(world_ed_cluster[,24:25], 3)
world_ed_cluster$cluster_no = as.integer(clusters$cluster)

iran_cluster_pca = world_ed_cluster %>% 
  filter(country == 'Iran, Islamic Rep.')

cat("Iran cluster:", iran_cluster[1,23,1])

str(clusters)

# 8
iran_ed_m <- world_ed %>% filter(country == 'Iran, Islamic Rep.')
iran_ed_m[is.na(iran_ed_m)] = 0

lm_irn = lm(data = iran_ed_m[,3:23], formula = `2016`~.)
summary(lm_irn)

