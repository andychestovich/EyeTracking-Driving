
library(tidyverse)
library(RColorBrewer)

# reading auc results files in 
auc_df <- read.csv("auc_results_comb.csv")
auc_df_eye <- read.csv("auc_results_eye.csv")
auc_df_veh <- read.csv("auc_results_veh.csv")

# binding 
auc_df_final <- rbind(auc_df, auc_df_eye, auc_df_veh)
auc_df_final

# calculating 2SE for standard error graph
auc_df_final <- auc_df_final %>%
  mutate(SE.SE = 2*(sd/sqrt(10)))


# graph with aucs and standard errors 
ggplot(auc_df_final, aes(x = time, y = auc, color = predictors))+
  facet_wrap(~model)+
  geom_point(aes(shape = predictors))+
  geom_line()+
  geom_errorbar(aes(ymin = auc - (sd/sqrt(10)), ymax = auc + (sd/sqrt(10))))+
  labs(x = "Window Length", y = "AUC value for model")+
  # title = "AUC for models of predictors over different interval splits")
  scale_x_continuous(breaks = c(15,30,45,60,75,90)) +
  scale_y_continuous(breaks = c(0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95))

# calculating ranges for aucs over models and predictors
auc_ranges <- auc_df_final %>%
  group_by(model, predictors)%>%
  summarize(
    range_auc = max(auc) - min(auc)
  )

# calculating ranges for aucs over models and times
auc_ranges_time <- auc_df_final %>%
  group_by(model, time)%>%
  summarize(
    range_auc = max(auc) - min(auc)
  )


# reading the max true positive rate files
max_tpr_df <- read.csv("max_tpr_df.csv")
max_tpr_df_eye <- read.csv("max_tpr_df_eye.csv")
max_tpr_df_comb <- read.csv("max_tpr_df_comb.csv")

# binding all of them 
max_tpr_all <- NULL
max_tpr_all <- rbind(max_tpr_df, max_tpr_df_eye,max_tpr_df_comb)


# boxplot over window lengths for max tprs
ggplot(max_tpr_df_comb, aes(x = time, y = max_tpr)) +
  facet_wrap(~model) +
  geom_boxplot(aes(group = time)) +
  labs(x = "Window Length", y = "Max True Positive Rate",
       title = "Max TPR for models of predictors over different interval splits") +
  scale_x_continuous(breaks = c(15,30,45,60,75,90)) +
  theme_classic()



# Final auc results graph without error bars
auc_df_final$model <- factor(auc_df_final$model, levels = c("Random Forest", "XGBoost", "ROCKET"))
ggplot(auc_df_final, aes(x = time, y = auc, color = predictors))+
  facet_wrap(~model)+
  geom_point(aes(shape = predictors))+
  geom_line()+
  #geom_errorbar(aes(ymin = auc - 2*(sd/sqrt(10)), ymax = auc + 2*(sd/sqrt(10))))+
  labs(x = "Window Length", y = "AUC value for model", title = "AUC for models of predictors over different interval splits", color = "", shape = "")+
  scale_x_continuous(breaks = c(15,30,45,60,75,90)) +
  scale_y_continuous(breaks = c(0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95))+
  scale_color_discrete(labels = c("comb" = "Combined", 
                                  "eye" = "Eye", 
                                  "vehicle" = "Vehicle")) +
  scale_shape_discrete(labels = c("comb" = "Combined", 
                                  "eye" = "Eye", 
                                  "vehicle" = "Vehicle")) +
  theme_classic(base_family = "serif", base_line_size = 7/11)




# function to extract all features and sort them by the importance parameter
get_all_features <- function(dir, sorting_var){
  top5_feat_imp <- NULL
  feat_imp <- list.files(dir)
  for(i in 1:length(feat_imp)){
    list_files <- list.files(file.path(dir,feat_imp[i]))
    for(j in 1:length(list_files)){
      data = read.csv(file.path(dir, feat_imp[i], list_files[j]))
      data_top_5 <- data %>%
        arrange(desc(!!sym(sorting_var)))%>%
        mutate(time = substr(feat_imp[i],24,25))
      top5_feat_imp <- rbind(top5_feat_imp,data_top_5)
    }
  }
  return(top5_feat_imp)
}


top5_feat_xgb_veh <- get_all_features(dir2, "Importance_XGB")

# finding the mean, and range for each feature and get top 5 most important features
get_top_features <- function(df, t) {
  temp <- df %>%
    filter(time == t) %>%
    group_by(Feature) %>%
    summarize(
      time = t,
      avg_imp_xgb = mean(Importance_XGB),
      max_imp_xgb = max(Importance_XGB),
      min_imp_xgb = min(Importance_XGB)
    ) %>%
    arrange(desc(avg_imp_xgb)) %>%
    head(5)
  return(temp)
}

# getting top 5s for all time intervals
# Vehicle
top_feat_15s <- get_top_features(top5_feat_xgb_veh,15)
top_feat_30s <- get_top_features(top5_feat_xgb_veh,30)
top_feat_45s <- get_top_features(top5_feat_xgb_veh,45)
top_feat_60s <- get_top_features(top5_feat_xgb_veh,60)
top_feat_75s <- get_top_features(top5_feat_xgb_veh,75)
top_feat_90s <- get_top_features(top5_feat_xgb_veh,90)

# function to get the final table of feature importances
summarize_table <- function(data) {
  final_table <- tibble(
    time = unique(data$time),
    most_imp = paste0(data$Feature[1], round(data$avg_imp_xgb[1], 3), "(", round(data$max_imp_xgb[1],3), "," , round(data$min_imp_xgb[1],3),  ")"),
    second_most_imp = paste0(data$Feature[2], round(data$avg_imp_xgb[2], 3), "(", round(data$max_imp_xgb[2],3), "," , round(data$min_imp_xgb[2],3),  ")"),
    third_most_imp = paste0(data$Feature[3], round(data$avg_imp_xgb[3], 3), "(", round(data$max_imp_xgb[3],3), "," , round(data$min_imp_xgb[3],3),  ")"),
    fourth_most_imp = paste0(data$Feature[4], round(data$avg_imp_xgb[4], 3), "(", round(data$max_imp_xgb[4],3), "," , round(data$min_imp_xgb[4],3),  ")"),
    fifth_most_imp =paste0(data$Feature[5], round(data$avg_imp_xgb[5], 3), "(", round(data$max_imp_xgb[5],3), "," , round(data$min_imp_xgb[5],3),  ")"))
  return(final_table)   
}

final_15_veh <- summarize_table(top_feat_15s)
final_30_veh <- summarize_table(top_feat_30s)
final_45_veh <- summarize_table(top_feat_45s)
final_60_veh <- summarize_table(top_feat_60s)
final_75_veh <- summarize_table(top_feat_75s)
final_90_veh <- summarize_table(top_feat_90s)

# binding individual rowsof the final table 
final_df_veh <- rbind(final_15_veh,final_30_veh,final_45_veh,final_60_veh,final_75_veh,final_90_veh)
final_df_veh <- final_df_veh %>%
  mutate(time = paste0(time, "s Vehicle"))
final_df_veh


top5_feat_xgb_eye <- get_all_features(dir, "Importance_XGB")
# Eye
top_feat_15s_eye <- get_top_features(top5_feat_xgb_eye,15)
top_feat_30s_eye <- get_top_features(top5_feat_xgb_eye,30)
top_feat_45s_eye <- get_top_features(top5_feat_xgb_eye,45)
top_feat_60s_eye <- get_top_features(top5_feat_xgb_eye,60)
top_feat_75s_eye <- get_top_features(top5_feat_xgb_eye,75)
top_feat_90s_eye <- get_top_features(top5_feat_xgb_eye,90)

final_15_eye <- summarize_table(top_feat_15s_eye)
final_30_eye <- summarize_table(top_feat_30s_eye)
final_45_eye <- summarize_table(top_feat_45s_eye)
final_60_eye <- summarize_table(top_feat_60s_eye)
final_75_eye <- summarize_table(top_feat_75s_eye)
final_90_eye <- summarize_table(top_feat_90s_eye)

final_df_eye <- rbind(final_15_eye,final_30_eye,final_45_eye,final_60_eye,final_75_eye,final_90_eye)
final_df_eye <- final_df_eye %>%
  mutate(time = paste0(time, "s Eye"))
final_df_eye

# Combined
top5_feat_xgb_comb <- get_all_features(dir3, "Importance_XGB")

top_feat_15s_comb <- get_top_features(top5_feat_xgb_comb,15)
top_feat_30s_comb <- get_top_features(top5_feat_xgb_comb,30)
top_feat_45s_comb <- get_top_features(top5_feat_xgb_comb,45)
top_feat_60s_comb <- get_top_features(top5_feat_xgb_comb,60)
top_feat_75s_comb <- get_top_features(top5_feat_xgb_comb,75)
top_feat_90s_comb <- get_top_features(top5_feat_xgb_comb,90)

final_15_comb <- summarize_table(top_feat_15s_comb)
final_30_comb <- summarize_table(top_feat_30s_comb)
final_45_comb <- summarize_table(top_feat_45s_comb)
final_60_comb <- summarize_table(top_feat_60s_comb)
final_75_comb <- summarize_table(top_feat_75s_comb)
final_90_comb <- summarize_table(top_feat_90s_comb)

final_df_comb <- rbind(final_15_comb,final_30_comb,final_45_comb,final_60_comb,final_75_comb,final_90_comb)
final_df_comb <- final_df_comb %>%
  mutate(time = paste0(time, "s Combined"))
final_df_comb


# binding sub-fnal dataframes to final results table
final_df <- rbind(final_df_eye, final_df_veh, final_df_comb)
final_df <- final_df %>%
  arrange(time)
final_df


