# load packages ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(splines)

#load data
data = readxl::read_excel('data/data.xlsx')
data %>% 
  mutate(age_cat=ifelse(age>=35,1,0))  -> data

data %>% 
  mutate(age_cat=factor(age_cat)) -> data


ys= c('biochemical_pregnancy')

pms = list(c('P6B'))

covs=list(c("age_cat+BMI_cat+education+employee+fresh_frozen+Infertility_duration+
            Infertility_type+N_embryo+Stimulation_protocol+Fertilization_method+
            Male_factor+Endometriosis+Tubal_factor+
            Ovulation_disorder+Diminished_ovarian_reserve+ns(transplant_month,4)+ns(transplant_year,2)+
            ns(t_P6B,3)")) 



groups = list(c('age_cat','fresh_frozen', 'Infertility_type',
                'N_embryo','Endometriosis','Tubal_factor',
                'Ovulation_disorder','Diminished_ovarian_reserve'))

tibble(y=ys) %>% 
  mutate(pm=pms) %>% 
  unnest(pm) %>% 
  mutate(cov=covs) %>% 
  unnest(cov) %>% 
  mutate(group = groups) %>% 
  unnest(group) %>% 
  mutate(model=str_c(y,'~',pm,':',group,'+',cov,sep = ''))  ->df_model


data  %>% 
  dplyr::filter(!is.na(biochemical_pregnancy))  %>% 
  dplyr::mutate(biochemical_pregnancy = as.numeric(as.character(biochemical_pregnancy))) -> data_1

data_1 %>%
  dplyr::mutate(weight = 0.9999) -> tmp1

data_1 %>%
  dplyr::mutate(weight = 0.0001) %>%
  dplyr::mutate(biochemical_pregnancy=1-biochemical_pregnancy) -> tmp2

rbind(tmp1, tmp2) -> data_2


stratified_function = function(x){
  data_2 %>% 
    filter(hospital==x) -> data_f
  
  df_model %>% 
    mutate(fit = map(model,~glm(as.formula(.x),
                                data=data_f,
                                weights = data_f$weight,
                                start = c(log(mean(data_f$biochemical_pregnancy)), rep(0,31)),
                                maxit = 1000, #增加迭代次数，减少不收敛情况
                                family=binomial(link = "log")))) -> df_model
  
  
  df_model %>% 
    mutate(coe=map(fit,~{
      s_fit = summary(.x)
      return(s_fit$coefficients %>%
               as.data.frame() %>% 
               rownames_to_column() %>% 
               as_tibble() %>% 
               filter(str_detect(rowname,'^P6B')))
    })) -> temp
  
  temp %>% 
    unnest(coe)  -> temp
  
  temp %>% 
    select(-fit) %>% 
    mutate(RR=round(exp(Estimate),8)) %>% 
    mutate(ci_low=round(exp(Estimate-1.96*`Std. Error`),8)) %>% 
    mutate(ci_up=round(exp(Estimate+1.96*`Std. Error`),8)) %>% 
    mutate(ci=str_c(ci_low,'~',ci_up,sep = '')) %>% 
    mutate(P=`Pr(>|z|)`) %>% 
    mutate(hospital=x)-> temp
  
  
  filename = paste('results/glm_stratified_RR/biopregnancy_stratified_',x,'.xlsx',sep = '')
  writexl::write_xlsx(temp,filename)
}

walk(unique(data$hospital),stratified_function)

