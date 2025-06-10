###### load packages ######
library(lme4)
library(tidyverse)
library(effects)
library(effectsize)
library(simr)
library(emmeans)
library(blme)
library(readr)
library(tidyverse)
library(readxl)
library(writexl)
library(emmeans)
library(car)
library(dplyr)
library(multcomp)
library(dplyr)
library(openxlsx)
library(lmerTest)
library(tidyverse)
library(effects)
library(effectsize)
library(simr)
library(emmeans)
library(blme)
library(lmerTest)
library(tidyverse)
library(effects)
library(effectsize)
library(simr)
library(emmeans)
library(blme)
library(nnet)
library(mediation)
library(mediationsens)
library(stargazer)
library(bruceR) #用于很多个中介模型，循环
library(lavaan)
#### demo for correlation analysis

 ##### 1: remitting delta Hipp_L的相关性------
data <- read_csv("A_ALL_Beahvioral_smri_6389_SubCaseControl_Med_NEW.csv")
data <- data[!is.na(data$Status_Pure_Shaw), ]
ABCD_T1_ECM_uncorrtced <- data
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)
del1 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 0 )
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del1, ]
del2 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 2 )
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del2, ]
del3 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 3 )
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del3, ]
del4 <- which(ABCD_T1_ECM_uncorrtced$imgincl_t1w_include.x == 0)
del5 <- which(ABCD_T1_ECM_uncorrtced$imgincl_t1w_include.y == 0)
del6 <- union(del4,del5)
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del6, ]
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)

result_thick_ECM_C <- c()
for (i in 1090){
  fit <- lm (Total_delta_y ~ scale(ABCD_T1_ECM_uncorrtced[,i])  +  cov_demo_comb_income_v2 +cov_demo_sex_v2+ All_Med +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5  + cov_smri_vol_scs_intracranialv_delta
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

result_thick_ECM_C <- c()
for (i in 1090){
  fit <- lm (Ittention_delta_y ~ scale(ABCD_T1_ECM_uncorrtced[,i]) + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7  + All_Med +
               + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21  +
               cov_race2 + cov_race3 + cov_race4 +cov_race5  + cov_smri_vol_scs_intracranialv_delta
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta  ,ADHD_Inattention_BL 
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

result_thick_ECM_C <- c()
for (i in 1090){
  fit <- lm (Hyper_delta_y ~ scale(ABCD_T1_ECM_uncorrtced[,i]) + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7  + All_Med +
               + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21  +
               cov_race2 + cov_race3 + cov_race4 +cov_race5  + cov_smri_vol_scs_intracranialv_delta
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta  ,ADHD_Inattention_BL 
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

################################------------#############################
##### 3: All ADHD 组海马发育与症状【total score delta; inattention delta; hyper-delta】的关系------
data <- read_csv("A_ALL_Beahvioral_smri_6389_SubCaseControl_Med_NEW.csv")
data <- data[!is.na(data$Status_Pure_Shaw), ]
ABCD_T1_ECM_uncorrtced <- data
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)
del1 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 0 )
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del1, ]
# del2 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 1 )
# ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del2, ]
# del3 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 2 )
# ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del3, ]
del4 <- which(ABCD_T1_ECM_uncorrtced$imgincl_t1w_include.x == 0)
del5 <- which(ABCD_T1_ECM_uncorrtced$imgincl_t1w_include.y == 0)
del6 <- union(del4,del5)
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del6, ]
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)

result_thick_ECM_C <- c()
for (i in 1090){
  fit <- lm (scale(Total_delta_y) ~ scale(ABCD_T1_ECM_uncorrtced[,i])  + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 + ALl_Med
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + cov_race2 + cov_race3 + cov_race4 +cov_race5  + scale(cov_smri_vol_scs_intracranialv_delta)
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

result_thick_ECM_C <- c()
for (i in 1090){
  fit <- lm (scale(Ittention_delta_y) ~ scale(ABCD_T1_ECM_uncorrtced[,i]) +cov_demo_sex_v2+ cov_interview_age  + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5   + scale(cov_smri_vol_scs_intracranialv_delta)
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

result_thick_ECM_C <- c()
for (i in 1090){
  fit <- lm (scale(Hyper_delta_y) ~ scale(ABCD_T1_ECM_uncorrtced[,i])  + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + cov_race2 + cov_race3 + cov_race4 +cov_race5  + scale(cov_smri_vol_scs_intracranialv_delta)
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

# 3: All ADHD 组PCC发育与症状【 total score delta; inattention delta; hyper-delta】的关系------
data <- read_csv("A_ALL_Beahvioral_smri_6389_SubCaseControl_Med_NEW.csv")
data <- data[!is.na(data$Status_Pure_Shaw), ]
ABCD_T1_ECM_uncorrtced <- data
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)
del1 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 0 )
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del1, ]
# del2 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 1 )
# ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del2, ]
# del3 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 2 )
# ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del3, ]
del4 <- which(ABCD_T1_ECM_uncorrtced$imgincl_t1w_include.x == 0)
del5 <- which(ABCD_T1_ECM_uncorrtced$imgincl_t1w_include.y == 0)
del6 <- union(del4,del5)
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del6, ]
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)

result_thick_ECM_C <- c()
for (i in 1072){
  fit <- lm (scale(Total_delta_y) ~ scale(ABCD_T1_ECM_uncorrtced[,i])  +cov_demo_sex_v2+ cov_interview_age  + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5  + scale(cov_smri_thick_cdk_mean_delta)  #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

result_thick_ECM_C <- c()
for (i in 1072){
  fit <- lm (scale(Ittention_delta_y) ~ scale(ABCD_T1_ECM_uncorrtced[,i]) +cov_demo_sex_v2+ cov_interview_age  + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5  + scale(cov_smri_thick_cdk_mean_delta)
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

result_thick_ECM_C <- c()
for (i in 1072){
  fit <- lm (scale(Hyper_delta_y) ~ scale(ABCD_T1_ECM_uncorrtced[,i]) +cov_demo_sex_v2+ cov_interview_age  + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5   + scale(cov_smri_thick_cdk_mean_delta) #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### 3: emergent 组PCC发育与症状【 total score delta; inattention delta; hyper-delta】的关系------
data <- read_csv("A_ALL_Beahvioral_smri_6389_SubCaseControl_Med_NEW.csv")
data <- data[!is.na(data$Status_Pure_Shaw), ]
ABCD_T1_ECM_uncorrtced <- data
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)
del1 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 0 )
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del1, ]
del2 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 1 )
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del2, ]
del3 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw == 3 )
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del3, ]
del4 <- which(ABCD_T1_ECM_uncorrtced$imgincl_t1w_include.x == 0)
del5 <- which(ABCD_T1_ECM_uncorrtced$imgincl_t1w_include.y == 0)
del6 <- union(del4,del5)
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del6, ]
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)

result_thick_ECM_C <- c()
for (i in 1038){
  fit <- lm (scale(Total_delta_y) ~ scale(ABCD_T1_ECM_uncorrtced[,i]) + All_Med  +  ADHD_Total_BL+ cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7   
             + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17    
             + cov_site18  + cov_site20  +          
               cov_race2 + cov_race3  +cov_race5  + cov_smri_thick_cdk_mean_delta#cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

result_thick_ECM_C <- c()
for (i in 1038){
  fit <- lm (scale(Ittention_delta_y) ~ scale(ABCD_T1_ECM_uncorrtced[,i]) + All_Med  + ADHD_Total_BL+ cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7   
             + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17    
             + cov_site18  + cov_site20  +          
               cov_race2 + cov_race3  +cov_race5  + cov_smri_thick_cdk_mean_delta
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

result_thick_ECM_C <- c()
for (i in 1038){
  fit <- lm (scale(Hyper_delta_y) ~ scale(ABCD_T1_ECM_uncorrtced[,i]) + All_Med +ADHD_Total_BL+ cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7   
             + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17    
             + cov_site18  + cov_site20  +          
               cov_race2 + cov_race3  +cov_race5  + cov_smri_thick_cdk_mean_delta #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[4] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

####### All ADHD PSM加权匹配推断从hipp_delta 到inattention_delta的方向？【remitting/all ADHD】  ？？？ #######--------
#### demo for PSM analysis
library(WeightIt)
A_Beahvioral <- read_csv("A_ALL_Beahvioral_smri_6389_SubCaseControl_Med_NEW.csv")
A_Beahvioral <- A_Beahvioral[!is.na(A_Beahvioral$Status_Pure_Shaw), ]
A_Beahvioral <- as.data.frame(A_Beahvioral)
#删除因变量的NA的行
rows_with_all_complete_x <- complete.cases(A_Beahvioral[c("imgincl_t1w_include.x")])
A_Beahvioral <- A_Beahvioral[rows_with_all_complete_x, ]
rows_with_all_complete_y <- complete.cases(A_Beahvioral[c("imgincl_t1w_include.y")])
A_Beahvioral <- A_Beahvioral[rows_with_all_complete_y, ]
del1 <- which(A_Beahvioral$imgincl_t1w_include.x == 0)
del2 <- which(A_Beahvioral$imgincl_t1w_include.y == 0)
del3 <- union(del1,del2)
A_Beahvioral <- A_Beahvioral[-del3, ]

### 删除control
del4 <- which(A_Beahvioral$Status_Pure_Shaw== 0)
A_Beahvioral <- A_Beahvioral[-del4, ]
### 删除late-onset
#del6 <- which(A_Beahvioral$Status_Pure_Shaw == 2)
#A_Beahvioral <- A_Beahvioral[-del6, ] smri_thick_cdk_ptcaterh_delta_y smri_vol_scs_hpuslh_delta_y
PSM = weightit(smri_thick_cdk_ptcaterh_delta_y ~ cov_interview_age + cov_demo_sex_v2 + 
                 cov_demo_comb_income_v2 + cov_demo_prnt_ed_v2 +  ADHD_Total_BL +
                 cov_site_id_l + as.factor(cov_race_ethnicity) +
                 cov_smri_vol_scs_intracranialv_delta ,
               data = A_Beahvioral, 
               method = "glm", 
               estimand = "ATT") #ATT

result_thick_ECM_C <- c()
for (i in c(1072)){ #641 FU2 Hipp;1090 delta
  fit <- lm (Ittention_delta_y ~ A_Beahvioral[,i] + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5 + ADHD_Total_BL + scale(cov_smri_thick_cdk_mean_delta )
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral, weights = PSM$weights)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))

confint(fit, level = 0.95)
effectsize::eta_squared(fit)

####### remitting组服药不服药对症状以及school social sleep 等功能以及为其他精神症状的影响？ #######--------
A_Beahvioral <- read_csv("A_ALL_Beahvioral_smri_6389_SubCaseControl_Med_NEW_Visit.csv")
A_Beahvioral <- A_Beahvioral[!is.na(A_Beahvioral$Status_Pure_Shaw), ]
A_Beahvioral <- as.data.frame(A_Beahvioral)
#删除因变量的NA的行
#rows_with_all_complete_x <- complete.cases(A_Beahvioral[c("imgincl_t1w_include.x")])
#A_Beahvioral <- A_Beahvioral[rows_with_all_complete_x, ]
#rows_with_all_complete_y <- complete.cases(A_Beahvioral[c("imgincl_t1w_include.y")])
#A_Beahvioral <- A_Beahvioral[rows_with_all_complete_y, ]
#del1 <- which(A_Beahvioral$imgincl_t1w_include.x == 0)
#A_Beahvioral <- A_Beahvioral[-del1, ]
# del2 <- which(A_Beahvioral$imgincl_t1w_include.y == 0)
# A_Beahvioral <- A_Beahvioral[-del2, ]
# del3 <- union(del1,del2)
# A_Beahvioral <- A_Beahvioral[-del3, ]
### 删除contro0
del4 <- which(A_Beahvioral$Status_Pure_Shaw== 2)
A_Beahvioral <- A_Beahvioral[-del4, ]
# FEP_fes_p_ss_fc_BL
# FEP_neighborhood_crime_y_BL
# FEP_srpf_y_ss_ses_BL
# FEP_ple_p_ss_total_number_BL
# FEP_ple_p_ss_total_bad_BL
# FEP_ple_p_ss_affected_bad_sum_BL
# FEP_nsc_p_ss_mean_3_items_BL
# FEP_ple_p_ss_affect_sum_BL
# Define the model formula

##### ADHD symptoms Baseline 
result_thick_ECM_C <- c()
for (i in c(17:19)){
  fit <- lm (A_Beahvioral[,i] ~  All_Med + scale(cov_interview_age) + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### Sleep problem Baseline 
result_thick_ECM_C <- c()
for (i in c(17:19)){
  fit <- lm (A_Beahvioral[,i] ~  All_Med + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### Sleep problem FU1
result_thick_ECM_C <- c()
for (i in c(1466:1472)){
  fit <- lm (A_Beahvioral[,i] ~  All_Med + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### ADHD symptoms FU2 
result_thick_ECM_C <- c()
for (i in c(23:25)){
  fit <- lm (A_Beahvioral[,i] ~  All_Med + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5  + ADHD_Total_BL
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### ADHD symptoms FU3 
result_thick_ECM_C <- c()
for (i in c(26:28)){
  fit <- lm (A_Beahvioral[,i] ~  All_Med  + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5+ ADHD_Total_BL
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### social school sleep function FU2
result_thick_ECM_C <- c() #100,102,104:105,1425:1431,175:186,131:139,1111
for (i in c(100,102,104:105,1425:1431,175:186,131:139,1111,1273:1285,1110:1111)){
  fit <- lm (A_Beahvioral[,i] ~  All_Med  + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +   cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5 + ADHD_Total_BL
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### social school function emotion FU3 
result_thick_ECM_C <- c() # 1432:1438,1408:1409,1411:1412,1286:1293,1107,1100:1111
for (i in c(1432:1438,1408:1409,1411:1412,1286:1293,1107,1100:1111)){
  fit <- lm (A_Beahvioral[,i] ~  All_Med  + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +   cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5+ ADHD_Total_BL
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

####### remitting组 cumulatieve Dosage的问题？ #######--------
A_Beahvioral <- read_csv("A_ALL_Beahvioral_smri_6389_SubCaseControl_Med_NEW_Visit.csv")
A_Beahvioral <- A_Beahvioral[!is.na(A_Beahvioral$Status_Pure_Shaw), ]
A_Beahvioral <- as.data.frame(A_Beahvioral)
#删除因变量的NA的行
#rows_with_all_complete_x <- complete.cases(A_Beahvioral[c("imgincl_t1w_include.x")])
#A_Beahvioral <- A_Beahvioral[rows_with_all_complete_x, ]
#rows_with_all_complete_y <- complete.cases(A_Beahvioral[c("imgincl_t1w_include.y")])
#A_Beahvioral <- A_Beahvioral[rows_with_all_complete_y, ]
#del1 <- which(A_Beahvioral$imgincl_t1w_include.x == 0)
#A_Beahvioral <- A_Beahvioral[-del1, ]
# del2 <- which(A_Beahvioral$imgincl_t1w_include.y == 0)
# A_Beahvioral <- A_Beahvioral[-del2, ]
# del3 <- union(del1,del2)
# A_Beahvioral <- A_Beahvioral[-del3, ]
### 删除contro0
del4 <- which(A_Beahvioral$Status_Pure_Shaw== 3)
A_Beahvioral <- A_Beahvioral[-del4, ]

del5 <- which(A_Beahvioral$All_Med_New== 0)
A_Beahvioral <- A_Beahvioral[-del5, ]

# 兴奋剂
##### ADHD symptoms Baseline 
result_thick_ECM_C <- c()
for (i in c(17:19)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_Stimu + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### Sleep problem Baseline 
result_thick_ECM_C <- c()
for (i in c(1418:1424)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_Stimu + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### ADHD symptoms FU2 
result_thick_ECM_C <- c()
for (i in c(23:25)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_Stimu + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5 + ADHD_Total_BL
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### ADHD symptoms FU3 
result_thick_ECM_C <- c()
for (i in c(26:28)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_Stimu  + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5 + scale(ADHD_Total_BL)
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### social school sleep function FU2 
result_thick_ECM_C <- c() #100,102,104:105,1425:1431,175:186,131:139,1111
for (i in c(100,102,104:105,1425:1431,175:186,131:139,1111,1273:1285,1110:1111)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_Stimu  + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +   cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5 
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### social school function emotion FU3 
result_thick_ECM_C <- c() # 1432:1438,1408:1409,1411:1412,1286:1293,1107,1100:1111
for (i in c(1432:1438,1408:1409,1411:1412,1286:1293,1107,1100:1111)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_Stimu  + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +   cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5 + ADHD_Total_BL
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

#非兴奋剂
##### ADHD symptoms Baseline 
result_thick_ECM_C <- c()
for (i in c(17:19)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_NonStimu + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### Sleep problem Baseline 
result_thick_ECM_C <- c()
for (i in c(1418:1424)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_NonStimu + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### ADHD symptoms FU2 
result_thick_ECM_C <- c()
for (i in c(23:25)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_NonStimu + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 + cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5 
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### ADHD symptoms FU3 
result_thick_ECM_C <- c()
for (i in c(26:28)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_NonStimu  + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### social school sleep function FU2 
result_thick_ECM_C <- c() #100,102,104:105,1425:1431,175:186,131:139,1111
for (i in c(100,102,104:105,1425:1431,175:186,131:139,1111,1273:1285,1110:1111)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_NonStimu  + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +   cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

##### social school function emotion FU3 
result_thick_ECM_C <- c() # 1432:1438,1408:1409,1411:1412,1286:1293,1107,1100:1111
for (i in c(1432:1438,1408:1409,1411:1412,1286:1293,1107,1100:1111)){
  fit <- lm (A_Beahvioral[,i] ~  Cumulatieve_NonStimu  + cov_interview_age + cov_demo_sex_v2 + cov_demo_comb_income_v2 +
               cov_demo_prnt_ed_v2 +  cov_site2 + cov_site3 + cov_site4 + cov_site5 + cov_site6 + cov_site7 
             + cov_site8 + cov_site9 + cov_site10 + cov_site11 + cov_site12 + cov_site13 + cov_site14 + cov_site15 + cov_site16 + cov_site17 
             + cov_site18 + cov_site19 + cov_site20 + cov_site21 + 
               cov_race2 + cov_race3 + cov_race4 +cov_race5
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = A_Beahvioral)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(A_Beahvioral)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

####### remitting组 长期服药和不服药的轨迹差异问题？ #######--------
ADHD_remitting_long_No <- read_excel("Remitting Long_No Trajectory.xlsx")
# cov_race2 + cov_race3 + cov_race4 +cov_race5 
ADHD_remitting_long_No <- as.data.frame(ADHD_remitting_long_No)
ADHD_remitting_long_No[2:77] <- lapply(ADHD_remitting_long_No[2:77], as.numeric)

# ADHD symptoms sleep grade 6:16
result_area_ECM_E <- c()
for (i in 6:16){
  fit <- lmer(ADHD_remitting_long_No[,i] ~ Visit*Long_Non + cov_interview_age +
                cov_demo_sex_v2 + cov_demo_comb_income_v2 + 
                cov_demo_prnt_ed_v2 + 
                cov_race2 + cov_race3 + cov_race4 +cov_race5 + 
                (1|cov_site_id_l/src_subject_id),data = ADHD_remitting_long_No)
  result_area_ECM_E <- rbind(result_area_ECM_E,
                             c(colnames(ADHD_remitting_long_No)[i], coef(summary(fit))[2,c(1,4,5)])) #将目标变量(2代表emotion)的β, t, p存储
}
result_area_ECM_E <- as.data.frame(result_area_ECM_E)
colnames(result_area_ECM_E)[4] = 'pvalue'
result_area_ECM_E$P.Adj <- p.adjust(result_area_ECM_E$pvalue, method = 'fdr', n = length(result_area_ECM_E$pvalue))


fit <- lmer(sds_p_ss_total_BL ~ Visit*Long_Non + cov_interview_age + 
              cov_demo_sex_v2 + cov_demo_comb_income_v2 + 
              cov_demo_prnt_ed_v2 + 
              cov_race2 + cov_race3 + cov_race4 +cov_race5 +  
              (1|cov_site_id_l/src_subject_id),data = ADHD_remitting_long_No)
summary(fit)

summary(fit)$coef
analysis_model_impul1 <- eta_squared(summary(fit)$coef)[,2]
performance::r2(model_impul)
fixef(model_impul)

