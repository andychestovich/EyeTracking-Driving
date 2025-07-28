

library(tidyverse)
library(pROC)

# Directory path
dir <- "path/30svehicle"
dir2 <- "path2/30svehicle"
# empty data frames to fill
stacked_results_rf <- NULL
stacked_results_xgb <- NULL
stacked_results_rocket <- NULL

# getting rocket results and binding them
for (i in 1:10) {
  stacked_results_rocket <- bind_rows(stacked_results_rocket, read_csv(file.path(dir2, paste0("rocketresults_vehicle", i, ".csv"))))
}

for (i in 1:10) {
  # getting rf and xgboost results to bind
  stacked_results_rf <- bind_rows(stacked_results_rf, read_csv(file.path(dir, paste0("rf_results", i, ".csv"))))
  stacked_results_xgb <- bind_rows(stacked_results_xgb, read_csv(file.path(dir, paste0("xgb_results", i, ".csv"))))
}

# calculating the roc and auc
# random forest
roc_rf <- roc(stacked_results_rf$target, stacked_results_rf$predicted_prob)
auc_rf <- round(auc(roc_rf),2)

# XGBoost
roc_xgb <- roc(stacked_results_xgb$target, stacked_results_xgb$predicted_prob)
auc_xgb <- round(auc(roc_xgb),2)

# ROCKET
roc_rocket <- roc(stacked_results_rocket$target, stacked_results_rocket$predicted_prob)
auc_rocket <- round(auc(roc_rocket),2)

# get the results dataframe 
roc_df_veh_30 <- bind_rows(
  data.frame(
    FPR = 1 - roc_rf$specificities,
    TPR = roc_rf$sensitivities,
    Model_auc =  paste0("RF (AUC = ", auc_rf, ")"),
    Model = "Random Forest",
    Predictor = "Vehicle",
    Time = 30
  ),
  data.frame(
    FPR = 1 - roc_xgb$specificities,
    TPR = roc_xgb$sensitivities,
    Model_auc = paste0("XGB (AUC = ", auc_xgb, ")"),
    Model = "XGBoost",
    Predictor = "Vehicle",
    Time = 30
  ),
  data.frame(
    FPR = 1 - roc_rocket$specificities,
    TPR = roc_rocket$sensitivities,
    Model_auc = paste0("ROCKET (AUC = ", auc_rocket, ")"),
    Model = "ROCKET",
    Predictor = "Vehicle",
    Time = 30
  )
)



# Directory path
dir <- "path/60s_vehicle"
dir2 <- "path2/60s_vehicle"

# empty data frames to fill
stacked_results_rf <- NULL
stacked_results_xgb <- NULL
stacked_results_rocket <- NULL

# getting rocket results and binding them
for (i in 1:10) {
  stacked_results_rocket <- bind_rows(stacked_results_rocket, read_csv(file.path(dir2, paste0("rocketresults_vehicle", i, ".csv"))))
}

for (i in 1:10) {
  # getting rf and xgboost results to bind
  stacked_results_rf <- bind_rows(stacked_results_rf, read_csv(file.path(dir, paste0("rf_results", i, ".csv"))))
  stacked_results_xgb <- bind_rows(stacked_results_xgb, read_csv(file.path(dir, paste0("xgb_results", i, ".csv"))))
}

# calculating the roc and auc
# random forest
roc_rf <- roc(stacked_results_rf$target, stacked_results_rf$predicted_prob)
auc_rf <- round(auc(roc_rf),2)

# XGBoost
roc_xgb <- roc(stacked_results_xgb$target, stacked_results_xgb$predicted_prob)
auc_xgb <- round(auc(roc_xgb),2)

# ROCKET
roc_rocket <- roc(stacked_results_rocket$target, stacked_results_rocket$predicted_prob)
auc_rocket <- round(auc(roc_rocket),2)

# get the results dataframe
roc_df_veh_60 <- bind_rows(
  data.frame(
    FPR = 1 - roc_rf$specificities,
    TPR = roc_rf$sensitivities,
    Model_auc =  paste0("RF (AUC = ", auc_rf, ")"),
    Model = "Random Forest",
    Predictor = "Vehicle",
    Time = 60
  ),
  data.frame(
    FPR = 1 - roc_xgb$specificities,
    TPR = roc_xgb$sensitivities,
    Model_auc = paste0("XGB (AUC = ", auc_xgb, ")"),
    Model = "XGBoost",
    Predictor = "Vehicle",
    Time = 60
  ),
  data.frame(
    FPR = 1 - roc_rocket$specificities,
    TPR = roc_rocket$sensitivities,
    Model_auc = paste0("ROCKET (AUC = ", auc_rocket, ")"),
    Model = "ROCKET",
    Predictor = "Vehicle",
    Time = 60
  )
)


# Directory path
dir <- "path/vehicle90s"
dir2 <- "path2/vehicle90s"
# empty data frames to fill
stacked_results_rf <- NULL
stacked_results_xgb <- NULL
stacked_results_rocket <- NULL

# getting rocket results and binding them
for (i in 1:10) {
  stacked_results_rocket <- bind_rows(stacked_results_rocket, read_csv(file.path(dir2, paste0("rocketresults_vehicle", i, ".csv"))))
}

for (i in 1:10) {
  # getting rf and xgboost results to bind
  stacked_results_rf <- bind_rows(stacked_results_rf, read_csv(file.path(dir, paste0("rf_results", i, ".csv"))))
  stacked_results_xgb <- bind_rows(stacked_results_xgb, read_csv(file.path(dir, paste0("xgb_results", i, ".csv"))))
}

# calculating the roc and auc
# random forest
roc_rf <- roc(stacked_results_rf$target, stacked_results_rf$predicted_prob)
auc_rf <- round(auc(roc_rf),2)

# XGBoost
roc_xgb <- roc(stacked_results_xgb$target, stacked_results_xgb$predicted_prob)
auc_xgb <- round(auc(roc_xgb),2)

# ROCKET
roc_rocket <- roc(stacked_results_rocket$target, stacked_results_rocket$predicted_prob)
auc_rocket <- round(auc(roc_rocket),2)

# get the results dataframe
roc_df_veh_90 <- bind_rows(
  data.frame(
    FPR = 1 - roc_rf$specificities,
    TPR = roc_rf$sensitivities,
    Model_auc =  paste0("RF (AUC = ", auc_rf, ")"),
    Model = "Random Forest",
    Predictor = "Vehicle",
    Time = 90
  ),
  data.frame(
    FPR = 1 - roc_xgb$specificities,
    TPR = roc_xgb$sensitivities,
    Model_auc = paste0("XGB (AUC = ", auc_xgb, ")"),
    Model = "XGBoost",
    Predictor = "Vehicle",
    Time = 90
  ),
  data.frame(
    FPR = 1 - roc_rocket$specificities,
    TPR = roc_rocket$sensitivities,
    Model_auc = paste0("ROCKET (AUC = ", auc_rocket, ")"),
    Model = "ROCKET",
    Predictor = "Vehicle",
    Time = 90
  )
)



# Directory path
dir <- "path/eye_30s"
dir2 <- "path2/eye_30s"
# empty data frames to fill
stacked_results_rf <- NULL
stacked_results_xgb <- NULL
stacked_results_rocket <- NULL

# getting rocket results and binding them
for (i in 1:10) {
  stacked_results_rocket <- bind_rows(stacked_results_rocket, read_csv(file.path(dir2, paste0("rocketresults_vehicle", i, ".csv"))))
}

for (i in 1:10) {
  # getting rf and xgboost results to bind
  stacked_results_rf <- bind_rows(stacked_results_rf, read_csv(file.path(dir, paste0("rf_results", i, ".csv"))))
  stacked_results_xgb <- bind_rows(stacked_results_xgb, read_csv(file.path(dir, paste0("xgb_results", i, ".csv"))))
}

# calculating the roc and auc
# random forest
roc_rf <- roc(stacked_results_rf$target, stacked_results_rf$predicted_prob)
auc_rf <- round(auc(roc_rf),2)

# XGBoost
roc_xgb <- roc(stacked_results_xgb$target, stacked_results_xgb$predicted_prob)
auc_xgb <- round(auc(roc_xgb),2)

# ROCKET
roc_rocket <- roc(stacked_results_rocket$target, stacked_results_rocket$predicted_prob)
auc_rocket <- round(auc(roc_rocket),2)

# get the results dataframe
roc_df_eye_30 <- bind_rows(
  data.frame(
    FPR = 1 - roc_rf$specificities,
    TPR = roc_rf$sensitivities,
    Model_auc =  paste0("RF (AUC = ", auc_rf, ")"),
    Model = "Random Forest",
    Predictor = "Eye",
    Time = 30
  ),
  data.frame(
    FPR = 1 - roc_xgb$specificities,
    TPR = roc_xgb$sensitivities,
    Model_auc = paste0("XGB (AUC = ", auc_xgb, ")"),
    Model = "XGBoost",
    Predictor = "Eye",
    Time = 30
  ),
  data.frame(
    FPR = 1 - roc_rocket$specificities,
    TPR = roc_rocket$sensitivities,
    Model_auc = paste0("ROCKET (AUC = ", auc_rocket, ")"),
    Model = "ROCKET",
    Predictor = "Eye",
    Time = 30
  )
)

# Directory path
dir <- "path1/eye_60s"
dir2 <- "path2/eye_60s"
# empty data frames to fill
stacked_results_rf <- NULL
stacked_results_xgb <- NULL
stacked_results_rocket <- NULL

# getting rocket results and binding them
for (i in 1:10) {
  stacked_results_rocket <- bind_rows(stacked_results_rocket, read_csv(file.path(dir2, paste0("rocketresults_vehicle", i, ".csv"))))
}

for (i in 1:10) {
  # getting rf and xgboost results to bind
  stacked_results_rf <- bind_rows(stacked_results_rf, read_csv(file.path(dir, paste0("rf_results", i, ".csv"))))
  stacked_results_xgb <- bind_rows(stacked_results_xgb, read_csv(file.path(dir, paste0("xgb_results", i, ".csv"))))
}

# calculating the roc and auc
# random forest
roc_rf <- roc(stacked_results_rf$target, stacked_results_rf$predicted_prob)
auc_rf <- round(auc(roc_rf),2)

# XGBoost
roc_xgb <- roc(stacked_results_xgb$target, stacked_results_xgb$predicted_prob)
auc_xgb <- round(auc(roc_xgb),2)

# ROCKET
roc_rocket <- roc(stacked_results_rocket$target, stacked_results_rocket$predicted_prob)
auc_rocket <- round(auc(roc_rocket),2)

# get the results dataframe
roc_df_eye_60 <- bind_rows(
  data.frame(
    FPR = 1 - roc_rf$specificities,
    TPR = roc_rf$sensitivities,
    Model_auc =  paste0("RF (AUC = ", auc_rf, ")"),
    Model = "Random Forest",
    Predictor = "Eye",
    Time = 60
  ),
  data.frame(
    FPR = 1 - roc_xgb$specificities,
    TPR = roc_xgb$sensitivities,
    Model_auc = paste0("XGB (AUC = ", auc_xgb, ")"),
    Model = "XGBoost",
    Predictor = "Eye",
    Time = 60
  ),
  data.frame(
    FPR = 1 - roc_rocket$specificities,
    TPR = roc_rocket$sensitivities,
    Model_auc = paste0("ROCKET (AUC = ", auc_rocket, ")"),
    Model = "ROCKET",
    Predictor = "Eye",
    Time = 60
  )
)


# Directory path
dir <- "path/eye_90s"
dir2 <- "path2/eye_90s"
# empty data frames to fill
stacked_results_rf <- NULL
stacked_results_xgb <- NULL
stacked_results_rocket <- NULL

# getting rocket results and binding them
for (i in 1:10) {
  stacked_results_rocket <- bind_rows(stacked_results_rocket, read_csv(file.path(dir2, paste0("rocketresults_vehicle", i, ".csv"))))
}

for (i in 1:10) {
  # getting rf and xgboost results to bind
  stacked_results_rf <- bind_rows(stacked_results_rf, read_csv(file.path(dir, paste0("rf_results", i, ".csv"))))
  stacked_results_xgb <- bind_rows(stacked_results_xgb, read_csv(file.path(dir, paste0("xgb_results", i, ".csv"))))
}

# calculating the roc and auc
# random forest
roc_rf <- roc(stacked_results_rf$target, stacked_results_rf$predicted_prob)
auc_rf <- round(auc(roc_rf),2)

# XGBoost
roc_xgb <- roc(stacked_results_xgb$target, stacked_results_xgb$predicted_prob)
auc_xgb <- round(auc(roc_xgb),2)

# ROCKET
roc_rocket <- roc(stacked_results_rocket$target, stacked_results_rocket$predicted_prob)
auc_rocket <- round(auc(roc_rocket),2)

# get the results dataframe
roc_df_eye_90 <- bind_rows(
  data.frame(
    FPR = 1 - roc_rf$specificities,
    TPR = roc_rf$sensitivities,
    Model_auc =  paste0("RF (AUC = ", auc_rf, ")"),
    Model = "Random Forest",
    Predictor = "Eye",
    Time = 90
  ),
  data.frame(
    FPR = 1 - roc_xgb$specificities,
    TPR = roc_xgb$sensitivities,
    Model_auc = paste0("XGB (AUC = ", auc_xgb, ")"),
    Model = "XGBoost",
    Predictor = "Eye",
    Time = 90
  ),
  data.frame(
    FPR = 1 - roc_rocket$specificities,
    TPR = roc_rocket$sensitivities,
    Model_auc = paste0("ROCKET (AUC = ", auc_rocket, ")"),
    Model = "ROCKET",
    Predictor = "Eye",
    Time = 90
  )
)


# Directory path
dir <- "path/comb_30s"
dir2 <- "path/comb_30s"
# empty data frames to fill
stacked_results_rf <- NULL
stacked_results_xgb <- NULL
stacked_results_rocket <- NULL

# getting rocket results and binding them
for (i in 1:10) {
  stacked_results_rocket <- bind_rows(stacked_results_rocket, read_csv(file.path(dir2, paste0("rocketresults_vehicle", i, ".csv"))))
}

for (i in 1:10) {
  # getting rf and xgboost results to bind
  stacked_results_rf <- bind_rows(stacked_results_rf, read_csv(file.path(dir, paste0("rf_results", i, ".csv"))))
  stacked_results_xgb <- bind_rows(stacked_results_xgb, read_csv(file.path(dir, paste0("xgb_results", i, ".csv"))))
}

# calculating the roc and auc
# random forest
roc_rf <- roc(stacked_results_rf$target, stacked_results_rf$predicted_prob)
auc_rf <- round(auc(roc_rf),2)

# XGBoost
roc_xgb <- roc(stacked_results_xgb$target, stacked_results_xgb$predicted_prob)
auc_xgb <- round(auc(roc_xgb),2)

# ROCKET
roc_rocket <- roc(stacked_results_rocket$target, stacked_results_rocket$predicted_prob)
auc_rocket <- round(auc(roc_rocket),2)

# get the results dataframe
roc_df_comb_30 <- bind_rows(
  data.frame(
    FPR = 1 - roc_rf$specificities,
    TPR = roc_rf$sensitivities,
    Model_auc =  paste0("RF (AUC = ", auc_rf, ")"),
    Model = "Random Forest",
    Predictor = "Combined",
    Time = 30
  ),
  data.frame(
    FPR = 1 - roc_xgb$specificities,
    TPR = roc_xgb$sensitivities,
    Model_auc = paste0("XGB (AUC = ", auc_xgb, ")"),
    Model = "XGBoost",
    Predictor = "Combined",
    Time = 30
  ),
  data.frame(
    FPR = 1 - roc_rocket$specificities,
    TPR = roc_rocket$sensitivities,
    Model_auc = paste0("ROCKET (AUC = ", auc_rocket, ")"),
    Model = "ROCKET",
    Predictor = "Combined",
    Time = 30
  )
)


# Directory path
dir <- "path/_comb_60s"
dir2 <- "path2/comb_60s"
# empty data frames to fill
stacked_results_rf <- NULL
stacked_results_xgb <- NULL
stacked_results_rocket <- NULL

# getting rocket results and binding them
for (i in 1:10) {
  stacked_results_rocket <- bind_rows(stacked_results_rocket, read_csv(file.path(dir2, paste0("rocketresults_vehicle", i, ".csv"))))
}

for (i in 1:10) {
  # getting rf and xgboost results to bind
  stacked_results_rf <- bind_rows(stacked_results_rf, read_csv(file.path(dir, paste0("rf_results", i, ".csv"))))
  stacked_results_xgb <- bind_rows(stacked_results_xgb, read_csv(file.path(dir, paste0("xgb_results", i, ".csv"))))
}

# calculating the roc and auc
# random forest
roc_rf <- roc(stacked_results_rf$target, stacked_results_rf$predicted_prob)
auc_rf <- round(auc(roc_rf),2)

# XGBoost
roc_xgb <- roc(stacked_results_xgb$target, stacked_results_xgb$predicted_prob)
auc_xgb <- round(auc(roc_xgb),2)

# ROCKET
roc_rocket <- roc(stacked_results_rocket$target, stacked_results_rocket$predicted_prob)
auc_rocket <- round(auc(roc_rocket),2)

# get the results dataframe
roc_df_comb_60 <- bind_rows(
  data.frame(
    FPR = 1 - roc_rf$specificities,
    TPR = roc_rf$sensitivities,
    Model_auc =  paste0("RF (AUC = ", auc_rf, ")"),
    Model = "Random Forest",
    Predictor = "Combined",
    Time = 60
  ),
  data.frame(
    FPR = 1 - roc_xgb$specificities,
    TPR = roc_xgb$sensitivities,
    Model_auc = paste0("XGB (AUC = ", auc_xgb, ")"),
    Model = "XGBoost",
    Predictor = "Combined",
    Time = 60
  ),
  data.frame(
    FPR = 1 - roc_rocket$specificities,
    TPR = roc_rocket$sensitivities,
    Model_auc = paste0("ROCKET (AUC = ", auc_rocket, ")"),
    Model = "ROCKET",
    Predictor = "Combined",
    Time = 60
  )
)


# Directory path
dir <- "path/comb_90s"
dir2 <- "path2/comb_90s"
# empty data frames to fill
stacked_results_rf <- NULL
stacked_results_xgb <- NULL
stacked_results_rocket <- NULL

# getting rocket results and binding them
for (i in 1:10) {
  stacked_results_rocket <- bind_rows(stacked_results_rocket, read_csv(file.path(dir2, paste0("rocketresults_vehicle", i, ".csv"))))
}

for (i in 1:10) {
  # getting rf and xgboost results to bind
  stacked_results_rf <- bind_rows(stacked_results_rf, read_csv(file.path(dir, paste0("rf_results", i, ".csv"))))
  stacked_results_xgb <- bind_rows(stacked_results_xgb, read_csv(file.path(dir, paste0("xgb_results", i, ".csv"))))
}

# calculating the roc and auc
# random forest
roc_rf <- roc(stacked_results_rf$target, stacked_results_rf$predicted_prob)
auc_rf <- round(auc(roc_rf),2)

# XGBoost
roc_xgb <- roc(stacked_results_xgb$target, stacked_results_xgb$predicted_prob)
auc_xgb <- round(auc(roc_xgb),2)

# ROCKET
roc_rocket <- roc(stacked_results_rocket$target, stacked_results_rocket$predicted_prob)
auc_rocket <- round(auc(roc_rocket),2)

# get the results dataframe
roc_df_comb_90 <- bind_rows(
  data.frame(
    FPR = 1 - roc_rf$specificities,
    TPR = roc_rf$sensitivities,
    Model_auc =  paste0("RF (AUC = ", auc_rf, ")"),
    Model = "Random Forest",
    Predictor = "Combined",
    Time = 90
  ),
  data.frame(
    FPR = 1 - roc_xgb$specificities,
    TPR = roc_xgb$sensitivities,
    Model_auc = paste0("XGB (AUC = ", auc_xgb, ")"),
    Model = "XGBoost",
    Predictor = "Combined",
    Time = 90
  ),
  data.frame(
    FPR = 1 - roc_rocket$specificities,
    TPR = roc_rocket$sensitivities,
    Model_auc = paste0("ROCKET (AUC = ", auc_rocket, ")"),
    Model = "ROCKET",
    Predictor = "Combined",
    Time = 90
  )
)

# binding all individual results to a master dataframe
master_df_results <- bind_rows(roc_df_veh_30,roc_df_veh_60,roc_df_veh_90, roc_df_eye_30,roc_df_eye_60, roc_df_eye_90, roc_df_comb_30,roc_df_comb_60,roc_df_comb_90)

# 9panel code
ggplot(master_df_results, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.1) +
  geom_abline(linetype = "dashed", alpha = 0.6) +
  facet_grid(Time~Predictor) +
  labs(
       x = "False Positive Rate",
       y = "True Positive Rate",
       color = "") +
  theme_bw(base_family = "serif")


