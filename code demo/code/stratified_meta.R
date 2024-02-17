library(tidyverse)
library(meta)


filenames = list.files('results/glm_stratified_RR',
                       pattern ='.xlsx',full.names = TRUE )
filenames %>% 
  map_dfr(~readxl::read_excel(.x)) -> data


tibble(out_comes=c('biochemical_pregnancy')) %>% 
  mutate(lag_gs = list(unique(data$rowname))) %>% 
  unnest(lag_gs)-> df_model 

#get meta effect
meta_effect = function(lag_g,out_come){
  data %>% 
    subset(y==out_come) %>% 
    filter(rowname==lag_g) -> temp
  
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
  res$pm =lag_g
  res$y = out_come
  return(res)
}
map2_dfr(df_model$lag_gs,df_model$out_comes,meta_effect) -> res


res %>% 
  mutate(OR = exp(TE*10),
         OR_LOWER = exp(lower*10),
         OR_UPPER = exp(upper*10)) -> res

res %>% 
  separate(pm,into = c('pm','group'),sep = ':')  %>% 
  mutate(group_name = str_remove_all(group,'\\d|.发|.胚')) -> res

#Ztest effect difference
res %>% 
  group_by(group_name,y) %>% 
  mutate(TE_d = abs(TE[1]-TE[2])) %>% 
  mutate(TE_d_se= sqrt(seTE[1]^2+seTE[2]^2)) %>% 
  mutate(z=TE_d/TE_d_se) %>% 
  mutate(P_NEW =2*(1-pnorm(z))) -> res


writexl::write_xlsx(res,'results/glm_stratified_meta/stratified_meta.xlsx')
