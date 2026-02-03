#### IMAGEN (一)remitting
  ####### subcortical volume #####
data <- read_csv("A_IMAGEN_Behaviour_sMRI_New_IQ_75.csv")
ABCD_T1_ECM_uncorrtced <- data
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)
del1 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw_7 == 0 )
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del1, ]
del2 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw_7 == 2 )
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del2, ]
del3 <- which(ABCD_T1_ECM_uncorrtced$Status_Pure_Shaw_7 == 3 )
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del3, ]
ABCD_T1_ECM_uncorrtced <- filter(ABCD_T1_ECM_uncorrtced, !is.na(Status_Pure_Shaw_7))
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)
result_thick_ECM_C <- c()
for (i in 42){
  fit <- lm (ABCD_T1_ECM_uncorrtced[,i]  ~ DK_Subcortical_Left_Hippocampus_delta_y + cov_Age_BL + cov_Sex 
             + SDQ_Hyperactivity_Youth_BL +
               cov_site_2 + cov_site_3 + cov_site_4  + cov_site_6 + cov_site_7  + cov_site_8  + cov_ICV_delta
             #cov_Puberty_ALL, cov_BMI,cov_site_id_l, cov_race_ethnicity, cov_smri_vol_scs_intracranialv_delta    
             ,data = ABCD_T1_ECM_uncorrtced)
  result_thick_ECM_C <- rbind(result_thick_ECM_C,
                              c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,2,3,4)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_thick_ECM_C <- as.data.frame(result_thick_ECM_C)
colnames(result_thick_ECM_C)[5] = 'pvalue'
result_thick_ECM_C$P.Adj <- p.adjust(result_thick_ECM_C$pvalue, method = 'fdr', n = length(result_thick_ECM_C$pvalue))
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

5: #### IMAGEN (一)remitting PSM
  library(WeightIt)
A_Beahvioral <- read_csv("A_IMAGEN_Behaviour_sMRI_New_IQ_75.csv")
A_Beahvioral <- A_Beahvioral[!is.na(A_Beahvioral$Status_Pure_Shaw_7), ]
A_Beahvioral <- A_Beahvioral[!is.na(A_Beahvioral$DK_Subcortical_Left_Hippocampus_delta_y), ]
A_Beahvioral <- as.data.frame(A_Beahvioral)

### 删除control
del4 <- which(A_Beahvioral$Status_Pure_Shaw_7== 0)
A_Beahvioral <- A_Beahvioral[-del4, ]
del5 <- which(A_Beahvioral$Status_Pure_Shaw_7== 2)
A_Beahvioral <- A_Beahvioral[-del5, ]
del6 <- which(A_Beahvioral$Status_Pure_Shaw_7== 3)
A_Beahvioral <- A_Beahvioral[-del6, ]

PSM = weightit(DK_Subcortical_Left_Hippocampus_delta_y ~  cov_Age_BL + cov_Sex + SDQ_Hyperactivity_Youth_BL +
                 cov_site_2 + cov_site_3 + cov_site_4  + cov_site_6 + cov_site_7  + cov_site_8  + scale(cov_ICV_delta),
               data = A_Beahvioral, 
               method = "glm",
               estimand = "ATE") #ATT

result_thick_ECM_C <- c()
for (i in c(402)){ #641 FU2 Hipp;1086 delta
  fit <- lm (SDQ_Hyperactivity_Youth_delta_y ~ A_Beahvioral[,i] + cov_Age_BL + cov_Sex + SDQ_Hyperactivity_Youth_BL +
               cov_site_2 + cov_site_3 + cov_site_4  + cov_site_6 + cov_site_7  + cov_site_8  + scale(cov_ICV_delta) 
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


####### 复旦儿童医院验证 服药剂量与症状以及睡眠轨迹的关系？ #######--------
Fudan_ADHD <- read_excel("Meregd_All_year_ output file.xlsx")
# cov_race2 + cov_race3 + cov_race4 +cov_race5 
Fudan_ADHD <- as.data.frame(Fudan_ADHD)
Fudan_ADHD[2:60] <- lapply(Fudan_ADHD[2:60], as.numeric)

# 两年内
Fudan_ADHD <- read_excel("Meregd_All_year_ output file.xlsx")
# cov_race2 + cov_race3 + cov_race4 +cov_race5 
Fudan_ADHD <- as.data.frame(Fudan_ADHD)
Fudan_ADHD[2:60] <- lapply(Fudan_ADHD[2:60], as.numeric)

Fudan_ADHD <- Fudan_ADHD %>% filter(Visit_Month < 4)
# SANP total
fit <- lmer(总分 ~ Visit_Month*Dosage_New + Age + Sex  +#疑问 使用FL2的Age还是FL3的Age
              (1|咨询号),data = Fudan_ADHD)
summary(fit)
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

# SANP inattention
fit <- lmer(注意力不足 ~ Visit_Month*Dosage_New + Age + Sex  +#疑问 使用FL2的Age还是FL3的Age
              (1|咨询号),data = Fudan_ADHD)
summary(fit)
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

# SANP hyper
fit <- lmer(多动 ~ Visit_Month*Dosage_New + Age + Sex +#疑问 使用FL2的Age还是FL3的Age
              (1|咨询号),data = Fudan_ADHD)
summary(fit)
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

# Sleep
fit <- lmer(Sleep ~ Visit_Month*Dosage_New + Age + Sex + SES +#疑问 使用FL2的Age还是FL3的Age
              (1|咨询号),data = Fudan_ADHD)
summary(fit)

# 三年及以上
Fudan_ADHD <- read_excel("Meregd_All_year_ output file.xlsx")
# cov_race2 + cov_race3 + cov_race4 +cov_race5 
Fudan_ADHD <- as.data.frame(Fudan_ADHD)
Fudan_ADHD[2:60] <- lapply(Fudan_ADHD[2:60], as.numeric)

Fudan_ADHD <- Fudan_ADHD %>% filter(Visit_Month > 3)
# SANP total
fit <- lmer(总分 ~ Visit_Month*Dosage_New + Age + Sex  +#疑问 使用FL2的Age还是FL3的Age
              (1|咨询号),data = Fudan_ADHD)
summary(fit)
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

# SANP inattention
fit <- lmer(注意力不足 ~ Visit_Month*Dosage_New + Age + Sex + SES +#疑问 使用FL2的Age还是FL3的Age
              (1|咨询号),data = Fudan_ADHD)
summary(fit)
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

# SANP hyper
fit <- lmer(多动 ~ Visit_Month*Dosage_New + Age + Sex + SES +#疑问 使用FL2的Age还是FL3的Age
              (1|咨询号),data = Fudan_ADHD)
summary(fit)
confint(fit, level = 0.95)
effectsize::eta_squared(fit)

# Sleep
fit <- lmer(Sleep ~ Visit_Month*Dosage_New + Age + Sex + SES +#疑问 使用FL2的Age还是FL3的Age
              (1|咨询号),data = Fudan_ADHD)
summary(fit)

