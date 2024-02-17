# load packages ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(gee)
library(splines)

#load data
data = readxl::read_excel('data/data.xlsx')

#analysis
ys= c('biochemical_pregnancy')

pms = list(c('P6B','P5B','P4B','P3B','P2B','P1B','D0',
             'P1A','P2A','P3A','P4A','P5A','P6A','P13'))

covs=list(c("age+BMI_cat+education+employee+fresh_frozen+Infertility_duration+
            Infertility_type+N_embryo+Stimulation_protocol+Fertilization_method+
            Male_factor+Endometriosis+Tubal_factor+
            Ovulation_disorder+Diminished_ovarian_reserve+ns(transplant_month,4)+ns(transplant_year,2)")) 

tibble(y=ys) %>% 
  mutate(pm=pms) %>% 
  unnest(pm) %>% 
  mutate(cov=covs) %>% 
  unnest(cov) %>% 
  mutate(model=str_c(y,'~',pm,'+',cov,'+ns(','t_',pm,',3)',sep = ''))  %>% 
  mutate(model=str_remove(model,'\\+NNNN')) -> df_model


data  %>% 
  dplyr::filter(!is.na(biochemical_pregnancy))  %>% 
  dplyr::mutate(biochemical_pregnancy = as.numeric(as.character(biochemical_pregnancy))) -> data_1

data_1 %>%
  dplyr::mutate(weight = 0.9999) -> tmp1

data_1 %>%
  dplyr::mutate(weight = 0.0001) %>%
  dplyr::mutate(biochemical_pregnancy=1-biochemical_pregnancy) -> tmp2

rbind(tmp1, tmp2) -> data_2


analysis = function(x){
  data_2 %>% 
    filter(hospital==x) -> data_s
  
  df_model %>% 
    mutate(fit = map(model,~glm(as.formula(.x),
                                data=data_s,
                                weights = data_s$weight,
                                start = c(log(mean(data_s$biochemical_pregnancy)), rep(0,30)),
                                maxit = 1000, 
                                family=binomial(link = "log")))) -> df_model
  
  df_model %>% 
    mutate(coe=map(fit,~{
      sfit = summary(.x)
      return(sfit$coefficients[2,])
    })) -> temp
  
  temp %>% 
    unnest_wider(coe)  -> temp
  
  temp %>% 
    select(-fit) %>% 
    mutate(RR=round(exp(Estimate),8)) %>% 
    mutate(ci_low=round(exp(Estimate-1.96*`Std. Error`),8)) %>% 
    mutate(ci_up=round(exp(Estimate+1.96*`Std. Error`),8)) %>% 
    mutate(ci=str_c(ci_low,'~',ci_up,sep = '')) %>% 
    mutate(P=`Pr(>|z|)`) %>% 
    mutate(hospital=x)-> temp
  
  filename = paste('results/glm_RR/glm_log_biochemical pregnancy_',x,'.xlsx',sep = '')
  writexl::write_xlsx(temp,filename)
}

walk(unique(data$hospital), analysis)
