library(tidyverse)
library(meta)


filenames = list.files('results/glm_RR',
                       pattern ='.xlsx',full.names = TRUE )

filenames %>%
  map_dfr(~readxl::read_excel(.x)) -> data

#meta_plot
meta_plot = function(lag,out_come){
  data %>% 
    subset(y==out_come) %>% 
    filter(pm==lag) -> temp
  
  m.gen <- metagen(TE = Estimate,
                   seTE =  `Std. Error`,
                   studlab = hospital,
                   data = temp,
                   sm = "OR",
                   fixed = TRUE,
                   random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE,
                   title = 'pm&outcome')
  # filed = str_c('results/pm&outcome/20230822-log-RR/glm-tap-only pm/meta/meta_plot/',lag,'-',out_come,'.png',sep = '')
  filed = str_c('results/glm_meta/',lag,'-',out_come,'.png',sep = '')
  # filed = str_c('results/pm&outcome/20231103-hospital/20231104-logRR/glm-tap/meta/meta_plot/',lag,'-',out_come,'.png',sep = '')
  png(filename = filed,
      width = 2000,
      height = 400)
  forest(m.gen,digits = 3)
  dev.off()
}


tibble(out_comes=c('biochemical_pregnancy')) %>% 
  mutate(lags = list(unique(data$pm))) %>% 
  unnest(lags)  -> df_model 


walk2(df_model$lags,df_model$out_comes,meta_plot)




#get meta effect
meta_effect = function(lag,out_come){
  data %>% 
    subset(y==out_come) %>% 
    filter(pm==lag) -> temp
  
  m.gen <- metagen(TE = Estimate,
                   seTE =  `Std. Error`,
                   studlab = hospital,
                   data = temp,
                   sm = "RR",
                   fixed = TRUE,
                   random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE,
                   title = 'pm&outcome')
  
  s = summary(m.gen)
  s$random %>% 
    as.data.frame()  -> res
  res$pm =lag
  res$y = out_come
  return(res)
}
map2_dfr(df_model$lags,df_model$out_comes,meta_effect) -> res


res %>% 
  mutate(OR = exp(TE*10),
         OR_LOWER = exp(lower*10),
         OR_UPPER = exp(upper*10)) -> res


writexl::write_xlsx(res,
                    path='results/glm_meta/meta-results.xlsx')
