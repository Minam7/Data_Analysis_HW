world_ec <- wdi_data %>% filter(`Indicator Code`== 'NE.EXP.GNFS.CD') %>% 
  select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]')) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'NY.GDP.PCAP.CD') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'NY.GNP.MKTP.CD') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'NE.GDI.FTOT.KD.ZG') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'BX.KLT.DINV.CD.WD') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'BM.GSR.GNFS.CD') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'BN.GSR.FCTY.CD') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'FP.CPI.TOTL.ZG') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'FM.LBL.BMNY.GD.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'NV.AGR.TOTL.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'NE.CON.TETC.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'NE.CON.GOVT.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'NV.IND.MANF.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'MS.MIL.XPND.GD.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'TG.VAL.TOTL.GD.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'NY.GDP.TOTL.RT.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'NE.TRD.GNFS.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'FS.AST.DOMS.GD.ZS') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'NY.GNP.MKTP.PP.CD') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]'))) %>% 
  rbind(wdi_data %>% filter(`Indicator Code`== 'SH.UHC.NOP1.TO') %>% 
          select(country = `Country Name`, index = `Indicator Name`, matches('^199[6-9]|^20[0-1][0-9]')))

world_ec_m <- world_ec %>% 
  melt(id.vars=c('country', 'index')) %>% 
  filter(!is.na(value)) %>% 
  group_by(index)
  


plots <- world_ec_m %>% 
  do(
    plots = ggplot(.,aes(x = variable, y = value, color = index)) + geom_boxplot() +
      xlab("Year") + ggtitle("Worldwide Economic vs. Iran") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_line(data = subset(.,country == 'Iran, Islamic Rep.'),aes(x = variable, y = value, group = 1, color = 'Iran, Islamic Rep.'))
  )
plots$plots
  