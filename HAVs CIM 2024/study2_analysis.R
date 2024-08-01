# libraries---------------------------------------------------------------------
lib_names <- c("semTools", "vroom", "tidyverse", "manymome", 
               "rstatix")
lapply(lib_names, library, character.only = T)

# loading data------------------------------------------------------------------
setwd("C:/Users/khick/Dropbox/PC/Desktop/Research/dissertation_studies2_3/data")
df <- vroom("study2_data.csv")
factor_labs <- c("PUB", "LOC", "PF", "FOS", "PPPOA", "PI")
# First CFA Model---------------------------------------------------------------
m_1 <- m_1
fit_cfa_1 <- cfa(model = m_1, data = df, ordered = colnames(df), std.lv = T, 
                estimator = "DWLS")
summary(fit_cfa_1)
fitmeasures(fit_cfa_1, fit.measures = c("chisq", "df", "pvalue", "rmsea", "srmr",
                                       "cfi", "tli"))
est_1 <- parameterestimates(fit_cfa_1, standardized = T)
est_1[,4:9] <- round(est_1[,4:9],2)
est_1 %>% filter(op == "=~")
est_1 %>% filter(lhs %in% factor_labs) %>% filter(op == "~~") %>% 
  filter(est != 1.00)
k_local(fit_cfa_1)
# Second CFA Model no potentiality6---------------------------------------------
m_2 <- m_2
fit_cfa_2 <- cfa(model = m_2, data = df, ordered = colnames(df), std.lv = T, 
                 estimator = "DWLS")
fitmeasures(fit_cfa_2, fit.measures = c("chisq", "df", "pvalue", "rmsea", "srmr",
                                        "cfi", "tli"))
est_2 <- parameterestimates(fit_cfa_2)
est_2[,4:9] <- round(est_2[,4:9],2)
est_2 %>% filter(op == "=~")
est_2 %>% filter(lhs %in% factor_labs) %>% filter(op == "~~") %>% 
  filter(est != 1.00)
k_local(fit_cfa_2)
# Third CFA Model no public2----------------------------------------------------
m_3 <- m_3
fit_cfa_3 <- cfa(model = m_3, data = df, ordered = colnames(df), std.lv = T, 
                 estimator = "DWLS")
summary(fit_cfa_3)
fitmeasures(fit_cfa_3, fit.measures = c("chisq", "df", "pvalue", "rmsea", "srmr",
                                        "cfi", "tli"))
est_3 <- parameterestimates(fit_cfa_3)
est_3[,4:9] <- round(est_3[,4:9],2)
est_3 %>% filter(op == "=~")
est_3 %>% filter(lhs %in% factor_labs) %>% filter(op == "~~") %>% 
  filter(est != 1.00)

k_local(fit_cfa_3)
# 4th CFA Model---------------------------------------------------------------

setwd("C:/Users/khick/Dropbox/PC/Desktop/Research/dissertation_studies2_3/results/k_diss_study2_exports")
m_4 <- m_4
fit_cfa_4 <- cfa(model = m_4, data = df, ordered = colnames(df), std.lv = T, 
                 estimator = "DWLS")
summary(fit_cfa_4)
fitmeasures(fit_cfa_4, fit.measures = c("chisq", "df", "pvalue", "rmsea", "srmr",
                                        "cfi", "tli"))
est_4 <- parameterestimates(fit_cfa_4,standardized = T)
est_4[,4:9] <- round(est_4[,4:9],2)
est_4 %>% filter(op == "=~")
est_4 %>% filter(!lhs %in% factor_labs)%>% filter(op == "~~")
reliability(fit_cfa_4)

r <- est_4 %>% filter(lhs %in% factor_labs) %>% filter(op == "~~") %>% 
  filter(est != 1.00)
write.csv(r, "study2_correlations.csv", row.names = F)

k_rel <- round(reliability(fit_cfa_4),2)
k_rel <- t(k_rel)
k_rel
write.csv(k_rel, "study_2_reliability.csv")

k_local(fit_cfa_4)
htmt(est_4, data = df)

# Structural Model---------------------------------------------------------------
# R should be 520000 for ~ 5k bootstraps
m_5 <- m_5
set.seed(1234)
fit_sem_5 <- sem(model = m_5, data = df, ordered = colnames(df), std.lv = T, 
                 estimator = "DWLS", se = "boot", bootstrap = 32000,
                 parallel = "snow", ncpus = 12)
summary(fit_sem_5)
anova(fit_cfa_4, fit_sem_5)


fitmeasures(fit_sem_5, fit.measures = c("chisq", "df", "pvalue", "rmsea", "srmr",
                                        "cfi", "tli"))

est_5 <- parameterestimates(fit_sem_5, standardized = T, boot.ci.type = "perc")
est_5[,5:13] <- round(est_5[,5:13],2)

est_5 <- est_5 %>% filter(op == "~" | op ==":=") %>% 
  filter(est != 1.00)
est_5 <- est_5[,-c(11,13)]

setwd("C:/Users/khick/Dropbox/PC/Desktop/Research/dissertation_studies2_3/results/k_diss_study2_exports")
write.csv(est_5, "study_2_regression2.csv", row.names = F)

# Mediation
all_paths <- all_indirect_paths(fit = fit_sem_5)
all_paths
setwd("C:/Users/khick/Dropbox/PC/Desktop/Research/dissertation_studies2_3/scripts/study_2_scripts")
save.image("study_2_analysis_data.Rdata")

summary(as.factor(df$gender_youngest))

summary(df$`Number of children`)
summary(as.factor(df$Ethnicity))
summary(df$`Duration (in seconds)`)

