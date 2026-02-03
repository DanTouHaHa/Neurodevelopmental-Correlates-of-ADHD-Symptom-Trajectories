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
library(data.table)

#Example: ANCOVA analysis for behavioural measures
###### delta-y ----
A_Beahvioral <- read_csv("A_ALL_Beahvioral_6389_SubCaseControl_Med.csv")
A_Beahvioral <- as.data.frame(A_Beahvioral)
# 将case1转换为一个因子变量，并确保人数最多的类别作为参考类别,  Persistent 比 remitting
A_Beahvioral$Status_Pure_Shaw <- factor(A_Beahvioral$Status_Pure_Shaw, levels = names(sort(table(A_Beahvioral$Status_Pure_Shaw), decreasing = TRUE)))
# 提取所有因变量的名称
response_vars <- names(A_Beahvioral)[grep("^COM.*._delta_y$", names(A_Beahvioral))] 
# 提取所有协变量的名称
covariate_vars <- names(A_Beahvioral)[grepl("^cov", names(A_Beahvioral))]
# 或者使用逻辑索引去除特定协变量
#. for sensitivity analysis, including BMI, puberty, and ADHD medication as additional covariates
variables_to_remove_cov <- c("cov_Puberty_ALL", "cov_BMI", "cov_site_id_l", "cov_race_ethnicity", "cov_PC1","cov_PC2","cov_PC3",
                             "cov_PC4","cov_PC5","cov_PC6","cov_PC7","cov_PC8","cov_PC9","cov_PC10","cov_rel_family_id")
covariate_vars <- covariate_vars[!covariate_vars %in% variables_to_remove_cov]
# 构建基础公式，包含所有协变量
base_formula <- paste("Status_Pure_Shaw", paste(covariate_vars, collapse=" + "), sep=" + ")
# 保存ANOVA的p值
p_values <- numeric(length(response_vars))
# 对每个因变量进行线性模型拟合和ANOVA检验
for (i in seq_along(response_vars)) {
  # 构建完整公式，加入当前的因变量
  formula <- as.formula(paste(response_vars[i], "~", base_formula))
  # 拟合模型
  model <- lm(formula, data = A_Beahvioral)
  # 进行ANOVA检验
  anova_result <- anova(model)
  # 获取group变量的ANOVA p值
  group_anova_pval <- anova_result["Status_Pure_Shaw", "Pr(>F)"]
  p_values[[i]] <- group_anova_pval
}

# 对p值进行FDR校正
# 对那些显著的因变量进行Tukey HSD事后检验
p_adjusted <- p.adjust(p_values, method = "fdr")
# 对每个显著的因变量进行Tukey HSD事后检验
tukey_results <- list()
for (i in which(p_adjusted < 1)) {
  # 构建完整公式，加入当前显著的因变量
  formula <- as.formula(paste(response_vars[i], "~", base_formula))
  # 拟合模型
  model <- aov(formula, data = A_Beahvioral)
  # Tukey HSD事后检验
  tukey <- TukeyHSD(model, "Status_Pure_Shaw")
  # 将结果存储在列表中
  tukey_results[[response_vars[i]]] <- tukey
  # 打印结果
  #print(paste(response_vars[i], "的Tukey HSD事后检验:"))
  print(tukey_results)
}
# 如果每个列表元素都可以被转换为一个向量，您可以这样操作
my_list <- lapply(tukey_results, unlist)
# 然后尝试将其转换为tibble
output <- tibble::as_tibble(my_list)
output_row_names <- c("3-0diff", "1-0diff", "2-0diff", "1-3diff", "2-3diff", "2-1diff", 
                      "3-0lwr", "1-0lwr", "2-0lwr", "1-3lwr", "2-3lwr", "2-1lwr", 
                      "3-0upr", "1-0upr", "2-0upr", "1-3upr", "2-3upr", "2-1upr", 
                      "3-0padj", "1-0padj", "2-0padj","1-3padj", "2-3padj", "2-1padj")
rownames(output) <- output_row_names
output <- t(output)
output <- as.data.frame(output)



#Example: ANCOVA analysis for neuroimaging measures
A_Beahvioral <- read_csv("A_ALL_Beahvioral_smri_6389_SubCaseControl_Med_NEW.csv")
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
A_Beahvioral$Status_Pure_Shaw <- factor(A_Beahvioral$Status_Pure_Shaw, levels = names(sort(table(A_Beahvioral$Status_Pure_Shaw), decreasing = TRUE)))
df_clean <- A_Beahvioral
# 提取所有因变量的名称
response_vars <- names(df_clean)[grep("^smri_thick_cdk.*\\_delta_y$", names(df_clean))]
# 提取所有协变量的名称
covariate_vars <- names(df_clean)[grepl("^cov", names(df_clean))]
# 使用逻辑索引去除特定协变量
#. for sensitivity analysis, including BMI, puberty, and ADHD medication as additional covariates
 variables_to_remove_cov <- c("cov_Puberty_ALL", "cov_BMI","cov_site_id_l", "cov_race_ethnicity","cov_PC1","cov_PC2","cov_PC3",
                             "cov_PC4","cov_PC5","cov_PC6","cov_PC7","cov_PC8","cov_PC9", "cov_rel_family_id" ,
                             "cov_PC10","cov_smri_thick_cdk_mean.x","cov_smri_area_cdk_total.x" ,"cov_smri_vol_scs_intracranialv.x",
                             "cov_smri_area_cdk_total.y","cov_smri_vol_scs_intracranialv.y","cov_smri_thick_cdk_mean.y",
                             "cov_smri_vol_scs_intracranialv_delta",
                             "cov_smri_area_cdk_total_delta", "cov_smri_area_cdk_total_delta_y",     
                             "cov_smri_thick_cdk_mean_delta_y"  , "cov_smri_vol_scs_intracranialv_delta_y"
)
covariate_vars <- covariate_vars[!covariate_vars %in% variables_to_remove_cov]

base_formula <- paste("Status_Pure_Shaw", paste(covariate_vars, collapse=" + "), sep=" + ")

p_values <- numeric(length(response_vars))

for (i in seq_along(response_vars)) {
  
  formula <- as.formula(paste(response_vars[i], "~", base_formula))
  model   <- lm(formula, data = df_clean)
  
  anova_result <- anova(model)
  group_anova_pval <- anova_result["Status_Pure_Shaw", "Pr(>F)"]
  
  p_values[i] <- group_anova_pval
}

p_adjusted <- p.adjust(p_values, method = "fdr")

#事后检验
library(emmeans)
emmeans_results <- list()

sig_idx <- which(p_adjusted < 1 & !is.na(p_adjusted))  

for (i in sig_idx) {
  formula <- as.formula(paste(response_vars[i], "~", base_formula))
  model   <- lm(formula, data = df_clean)
  emm      <- emmeans(model, ~ Status_Pure_Shaw)
  pair_res <- pairs(emm, adjust = "tukey")
  emmeans_results[[response_vars[i]]] <- as.data.frame(pair_res)
}

#输出
my_list <- lapply(emmeans_results, unlist)
output  <- tibble::as_tibble(my_list)
output  <- as.data.frame(t(output))

