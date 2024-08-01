# Libraries --------------------------------------------------------------------
lapply(lib_names, library, character.only = T)

# Loading Data------------------------------------------------------------------
setwd("C:/Users/khick/Dropbox/PC/Desktop/Research/dissertation_studies2_3/data")
diss_survey <- vroom("diss_survey_data.csv")
diss_survey <- diss_survey[-(1:2),]
diss_survey2 <- diss_survey
diss_survey2$id_num <- 1:nrow(diss_survey2)
diss_survey2 <- diss_survey2[order(diss_survey2$prolific_id, decreasing = TRUE), ]
head(diss_survey2[,c(19,21,22,30:35)])
as.data.frame(colnames(diss_survey2))
diss_survey2$age_youngest_1 <- as.numeric(diss_survey2$age_youngest_1)
diss_survey2$age_oldest_1 <- as.numeric(diss_survey2$age_oldest_1)
diss_survey2$`Duration (in seconds)` <- as.numeric(diss_survey2$`Duration (in seconds)`)
diss_survey2[,30:95] <- lapply(diss_survey2[,30:95],as.factor)

diss_demographic <- read_csv("diss_demographic_data.csv")
diss_demographic <- diss_demographic[1:325,] # 326 and on returned or timed out
as.data.frame(colnames(diss_demographic))
diss_demographic[,c(14:18, 20:27)] <- lapply(diss_demographic[,c(14:18, 20:27)], 
                                             as.factor)
diss_demographic[,c(12,13,19)] <- lapply(diss_demographic[,c(12,13,19)], as.numeric)

# Filtering IDs-----------------------------------------------------------------
ids_demographic <- diss_demographic$`Participant id`
matched_survey <- diss_survey2 %>% filter(prolific_id %in% ids_demographic)

duplicates <- as.data.frame(summary(as.factor(matched_survey$prolific_id))
                            #,row.names = T
                            )

duplicates$id <- row.names(duplicates)
dup_ids <- duplicates[1:3,2]

duplicates2 <- matched_survey %>% filter(prolific_id %in% dup_ids)
duplicates2 <- duplicates2[,c(5:7,19,97)]


matched_survey <- matched_survey %>% 
  filter(id_num != 353 & id_num != 252 & id_num != 367)
summary(matched_survey$prolific_id)
class(matched_survey$prolific_id)
# Combining demographics with survey data---------------------------------------
as.data.frame(colnames(matched_survey))
as.data.frame(colnames(diss_demographic))

matched_survey <- matched_survey[,-c(1:4, 8:17, 24, 26, 96)]
matched_survey <- matched_survey[order(matched_survey$prolific_id, 
                                       decreasing = TRUE), ]

diss_demographic <- diss_demographic[,-c(3:11)]
diss_demographic <- diss_demographic[order(diss_demographic$`Participant id`, 
                                       decreasing = TRUE), ]
pre_careless <- cbind(diss_demographic, matched_survey)
as.data.frame(colnames(pre_careless))
# Attention Checks--------------------------------------------------------------
attFail <- pre_careless %>% 
  filter(attention1 != "Somewhat agree" & attention2 != "Monday")
attFail2 <- pre_careless %>% filter(attention1 != "Somewhat agree")
summary(attFail2$attention2)
attFail3 <- pre_careless %>% filter(attention2 != "Monday")
summary(attFail3$attention1)
pre_careless <- pre_careless[,-c(54, 80, 1, 7, 11:17, 98)]
# Careless Responses------------------------------------------------------------
pre_careless[, 29:86] <- likert2int(pre_careless[, 29:86], 
                                                     agree_disagree_labs)
pre_careless[, 23:28] <- likert2int(pre_careless[, 23:28], 
                                    freq_labs)
pre_careless <- cbind(irv(pre_careless[, c(23:86)], split = TRUE, num.split = 4),
                      pre_careless)
post_careless <- k_filter(data = pre_careless, column = pre_careless$irvTotal)
nrow(pre_careless) - nrow(post_careless)

# Descriptive Statistics--------------------------------------------------------
post_careless$young_age_pro <- 2024 - post_careless$`Year of birth of youngest child`
post_careless$`Duration (in seconds)` <- post_careless$`Duration (in seconds)`/60
post_time <- k_filter(post_careless,post_careless$`Duration (in seconds)`/60)
summary(post_time$`Duration (in seconds)`/60)
nrow(post_careless) - nrow(post_time)

as.data.frame(colnames(post_time))
summary(post_time$young_age_pro)
hist(post_time$young_age_pro)

summary(post_time$age_youngest_1)
hist(post_time$age_youngest_1)

youg_age_comp <- post_time[,c(6, 13, 7, 21, 22,92)]
youg_age_comp$age_diff <- youg_age_comp$age_oldest_1 - youg_age_comp$age_youngest_1
youg_age_comp$reporting_diff <- youg_age_comp$age_youngest_1 - youg_age_comp$young_age_pro
# Study 2 demographics----------------------------------------------------------
summary(post_time$`Number of children`)
summary(post_time$Ethnicity)
summary(post_time$`Highest education level completed`)
post_time$Gender <- factor(post_time$Gender, levels = c("Man (including Trans Male/Trans Man)",
                                                        "Non-binary (would like to give more detail)",
                                                        "Woman (including Trans Female/Trans Woman)"),
                           labels = c("Man", "Non_Binary", "Woman"))
summary(post_time$Gender)
summary(post_time$Age)
summary(post_time$`Duration (in seconds)`)
hist(post_time$`Duration (in seconds)`)
# study 3 demographics----------------------------------------------------------

post_time %>% filter(Gender != "Non_Binary")  %>% 
  summarise(num_children = mean(`Number of children`))
post_time %>% filter(Gender != "Non_Binary")  %>% 
  summarise(num_children = sd(`Number of children`))
post_time %>% filter(Gender != "Non_Binary")  %>% 
  count(Ethnicity)
post_time %>% filter(Gender != "Non_Binary")  %>% 
  count(Gender)
summary(post_time$Ethnicity)
summary(post_time$`Highest education level completed`)
summary(post_time$Gender)
summary(post_time$Age)
summary(post_time$`Duration (in seconds)`)
hist(post_time$`Duration (in seconds)`)
# separating days into study 2 and study 3 data---------------------------------
as.data.frame(colnames(post_time))
study2 <- post_time[,c(6,8,9,11,12,13,16,23,92,28:54, 85:91)]
study3 <- post_time[,c(6,8,9,11,12,13,16,23,92,50:74,85:91)]

write.csv(study2, "study2_data.csv", row.names = F)
write.csv(study3, "study3_data.csv", row.names = F)









