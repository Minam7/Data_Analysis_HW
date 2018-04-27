library(ggplot2)
library(ggthemes)
library(boot)

death_num <- na.omit(death_num)

table(death_num$MannerOfDeath,ifelse(fitted(glm_model)>0.5,1,0)) %>% 
  plot(xlab = "prediction", ylab = "data") %>% 
  title(main = "GLM Perfomance: Real Value vs Predicted Value")

death_num_pred = death_num %>% mutate(preds = predict(glm_model, type = 'response'))
ggplot( death_num_pred, aes( preds, color = as.factor(MannerOfDeath))) + 
  geom_density( size = 1 ) +
  ggtitle( "Training Set's Predicted Score" ) + 
  scale_color_economist( name = "Manner of Death", labels = c( "suicide", "death" ) )

source(file = "codes/unbalanced_functions.R")

cm_info = ConfusionMatrixInfo( data = death_num_pred, predict = "preds", 
                               actual = "MannerOfDeath", cutoff = 0.5 )
cm_info$plot
