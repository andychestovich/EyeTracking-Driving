
library(tidyverse)
library(readxl)

# change as needed
dir <- "Results_XGB_RF_Veh"
dir2 <- "Results_XGB_RF_Eye"
dir3 <- "Results_XGB_RF_Comb"

files <- list.files(dir)
files2 <- list.files(dir2)
files3 <- list.files(dir3)

# getting rocket vehicle results
rocket_veh <- NULL
for(i in 1:6){
  list_files <- list.files(file.path(dir,files[i]))
  for(j in 1:length(list_files)){
    data = read.csv(file.path(dir, files[i], list_files[j]))
    data <- data %>%
      mutate(time = 15*i,
             sample_ID = paste0(id1,j))
    rocket_veh <- rbind(rocket_veh, data)
  }
}

# rocket eye
rocket_eye <- NULL
for(i in 7:12){
  list_files2 <- list.files(file.path(dir2,files2[i]))
  for(j in 1:length(list_files2)){
    if(i == 11){
      data = read.csv(file.path(dir2, files2[i], list_files2[j]))
      data <- data %>%
        mutate(time = 15*i - 90,
               id1 = sample_ID,
               sample_ID = paste0(sample_ID,j))
    }
    else{
      data = read.csv(file.path(dir2, files2[i], list_files2[j]))
      data <- data %>%
        mutate(time = 15*i - 90,
               sample_ID = paste0(id1,j))
    }
    rocket_eye <- rbind(rocket_eye, data)
  }
}

#rocket combined
rocket_comb <- NULL
for(i in 7:length(files3)){
  list_files3 <- list.files(file.path(dir3,files3[i]))
  for(j in 1:length(list_files3)){
    data = read.csv(file.path(dir3, files3[i], list_files3[j]))
    data <- data %>%
      mutate(time = 15*i - 90,
             sample_ID = paste0(id1,j))
    rocket_comb <- rbind(rocket_comb, data)
  }
}


# combined random forest
comb_results_rf <- NULL
for(i in 1:6){
  list_files3 <- list.files(file.path(dir3,files3[i]))
  for(j in 1:10){
    data = read.csv(file.path(dir3, files3[i], list_files3[j]))
    data <- data %>%
      mutate(time = 15*i)
    comb_results_rf <- rbind(comb_results_rf, data)
  }
}

# combined xgboost
comb_results_xgb <- NULL
for(i in 1:6){
  list_files3 <- list.files(file.path(dir3,files3[i]))
  for(j in 11:length(list_files3)){
    data = read.csv(file.path(dir3, files3[i], list_files3[j]))
    data <- data %>%
      mutate(time = 15*i)
    comb_results_xgb <- rbind(comb_results_xgb, data)
  }
}

# rf and xgb vehicle 
veh_results <- NULL
for(i in 7:length(files)){
  list_files <- list.files(file.path(dir,files[i]))
  for(j in 1:length(list_files)){
    data = read.csv(file.path(dir, files[i], list_files[j]))
    data <- data %>%
      mutate(time = 15*i,
             sample_ID = paste0(sample_ID,j))
    veh_results <- rbind(veh_results, data)
  }
}

# rf and xgb eye 
eye_results <- NULL
for(i in 1:6){
  list_files2 <- list.files(file.path(dir2,files2[i]))
  for(j in 1:length(list_files2)){
    data = read.csv(file.path(dir2, files2[i], list_files2[j]))
    data <- data %>%
      mutate(time = 15*i,sample_ID = paste0(sample_ID,j))
    eye_results <- rbind(eye_results, data)
  }
}

# rf and xgb combined
comb_results <- NULL
for(i in 1:6){
  list_files3 <- list.files(file.path(dir3,files3[i]))
  for(j in 1:length(list_files3)){
    data = read.csv(file.path(dir3, files3[i], list_files3[j]))
    data <- data %>%
      mutate(time = 15*i)
    comb_results <- rbind(comb_results, data)
  }
}

# calculating proportion of correct predictions when prob predicted > 0.995
# vehicle
rocket_results_thresh <- rocket_veh %>%
  filter(predicted_prob >= 0.995) %>%
  mutate(correct = ifelse(target == pred, 1, 0)) %>%
  group_by(time)%>%
  summarize(
    prop_correct = sum(correct)/n(),
    n = n(),
    predictor = "rocket_vehicle"
  )

# eye
rocket_eye_thresh <- rocket_eye %>%
  filter(predicted_prob >= 0.995) %>%
  mutate(correct = ifelse(target == pred, 1, 0)) %>%
  group_by(time)%>%
  summarize(
    prop_correct = sum(correct)/n(),
    n = n(),
    predictor = "rocket_eye"
  )

# combined
rocket_comb_thresh <- rocket_comb %>%
  filter(predicted_prob >= 0.995) %>%
  mutate(correct = ifelse(target == pred, 1, 0)) %>%
  group_by(time)%>%
  summarize(
    prop_correct = sum(correct)/n(),
    n = n(),
    predictor = "ROCKET"
  )

# combined random forest and xgboost
comb_results_thresh_rf <- comb_results_rf%>%
  filter(predicted_prob >= 0.995) %>%
  mutate(correct = ifelse(target == pred, 1, 0)) %>%
  group_by(time)%>%
  summarize(
    prop_correct = sum(correct)/n(),
    n = n(),
    predictor = "Random Forest"
  )

comb_results_thresh_xgb <- comb_results_xgb%>%
  filter(predicted_prob >= 0.995) %>%
  mutate(correct = ifelse(target == pred, 1, 0)) %>%
  group_by(time)%>%
  summarize(
    prop_correct = sum(correct)/n(),
    n = n(),
    predictor = "XGBoost"
  )

# rf and xgb vehicle
veh_results_thresh <- veh_results%>%
  filter(predicted_prob >= 0.995) %>%
  mutate(correct = ifelse(target == pred, 1, 0)) %>%
  group_by(time)%>%
  summarize(
    prop_correct = sum(correct)/n(),
    n = n(),
    predictor = "vehicle"
  )

# rf and xgb eye
eye_results_thresh <- eye_results%>%
  filter(predicted_prob >= 0.995) %>%
  mutate(correct = ifelse(target == pred, 1, 0)) %>%
  group_by(time)%>%
  summarize(
    prop_correct = sum(correct)/n(),
    n = n(),
    predictor = "eye"
  )

# rf and xgb combined
comb_results_thresh <- comb_results%>%
  filter(predicted_prob >= 0.995) %>%
  mutate(correct = ifelse(target == pred, 1, 0)) %>%
  group_by(time)%>%
  summarize(
    prop_correct = sum(correct)/n(),
    n = n(),
    predictor = "combined"
  )

# graph for rocket vehicle threshold > 0.995
ggplot(rocket_results_thresh, aes(x = time, y = prop_correct))+
  geom_col()+
  labs(y = "Proprtion of correct predictions")+
  scale_x_continuous(breaks = c(15,30,45,60,75,90))+
  geom_text(aes(label = round(prop_correct,3), vjust = -0.35))+
  labs(caption = "Proportion of correctly predicted targets for ROCKET vehicle predictors when predicted probability is > 0.995")

# rocket eye
ggplot(rocket_eye_thresh, aes(x = time, y = prop_correct))+
  geom_col()+
  labs(y = "Proprtion of correct predictions")+
  scale_x_continuous(breaks = c(15,30,45,60,75,90))+
  geom_text(aes(label = round(prop_correct,3), vjust = -0.35))+
  labs(caption = "Proportion of correctly predicted targets for ROCKET eye predictors when predicted probability is > 0.995")

# line graph fpr rocket combined
ggplot(rocket_comb_thresh, aes(x = time, y = prop_correct))+
  geom_point(color = "royalblue")+
  labs(y = "Proprtion of correct predictions")+
  scale_x_continuous(breaks = c(15,30,45,60,75,90))+
  geom_line(color = "royalblue")+
  geom_text(aes(label = round(prop_correct,3), vjust = -0.35))+
  labs(caption = "Proportion of correctly predicted targets for ROCKET combined predictors when predicted probability is > 0.99")

# rf and xgb vehicle 
ggplot(veh_results_thresh, aes(x = time, y = prop_correct))+
  geom_col()+
  labs(y = "Proprtion of correct predictions")+
  scale_x_continuous(breaks = c(15,30,45,60,75,90))+
  geom_text(aes(label = round(prop_correct,3), vjust = -0.35))+
  labs(caption = "Proportion of correctly predicted targets for vehicle predictors when predicted probability is > 0.99")

# rf and xgb eye
ggplot(eye_results_thresh, aes(x = time, y = prop_correct))+
  geom_col()+
  labs(y = "Proprtion of correct predictions")+
  scale_x_continuous(breaks = c(15,30,45,60,75,90))+
  geom_text(aes(label = round(prop_correct,3), vjust = -0.35))+
  labs(caption = "Proportion of correctly predicted targets for eye predictors when predicted probability is > 0.99")

# rf and xgb combined
ggplot(comb_results_thresh, aes(x = time, y = prop_correct))+
  geom_point(color = "royalblue")+
  labs(y = "Proprtion of correct predictions")+
  scale_x_continuous(breaks = c(15,30,45,60,75,90))+
  geom_line(color = "royalblue")+
  geom_text(aes(label = round(prop_correct,3), vjust = -0.35))+
  labs(caption = "Proportion of correctly predicted targets for combined predictors when predicted probability is > 0.99")

# combining results threshold dataframes for combined dfs
all_comb <- rbind(comb_results_thresh_rf, comb_results_thresh_xgb, rocket_comb_thresh)
all_comb

# graph for all three combined models for threshold >0.995
ggplot(all_comb, aes(x = time, y = prop_correct))+
  geom_point(aes(color = predictor))+
  labs(y = "Proportion of Correct Predictions", x = "Window Length", color = "Predictor")+
  scale_x_continuous(breaks = c(15,30,45,60,75,90))+
  geom_line(aes(color = predictor))+
  geom_text(aes(label = round(prop_correct,3), vjust = -0.35))+
  labs(color = "Model")#+
#labs(caption = "Proportion of correctly predicted targets for combined predictors when predicted probability is > 0.99")

# binding differe types of predictor threshes
all_results_thresh <- NULL
all_results_thresh <- rbind(eye_results_thresh, veh_results_thresh, comb_results_thresh)

ggplot(all_results_thresh, aes(x = time, y = prop_correct))+
  geom_point(aes(color = predictor))+
  labs(y = "Proprtion of correct predictions")+
  scale_x_continuous(breaks = c(15,30,45,60,75,90))+
  geom_line(aes(color = predictor))+
  #geom_text(aes(label = round(prop_correct,3), vjust = -0.35))+
  geom_abline(slope = 0, intercept = 0.997, lty = 2)+
  geom_text(x = 50, y = 0.999, label = "0.997", color = "black")+
  labs(caption = "Proportion of correctly predicted targets for all predictors when predicted probability is > 0.995")


# additional graphs for kss, readiness to drive, eye vs vehicle, etc
veh_results_60 <- veh_results%>%
  filter(time == 60)

eye_results_60 <- eye_results%>%
  filter(time == 60)


final_results_60 <- eye_results_60%>%
  full_join(veh_results_60, by = "sample_ID")


final_results_60 <- final_results_60 %>%
  mutate(correct = ifelse((target.x == pred.x & target.y== pred.y), 2, ifelse((target.x == pred.x & target.y!= pred.y) | (target.x!=pred.x & target.y==pred.y), 1, 0)))

final_results_60

ggplot(final_results_60, aes(predicted_prob.x,predicted_prob.y))+
  labs(y = "Vehicle only", x = "Eye only", caption = "0 for both predicted wrong, 1 for one model predicted correct, 2 for both models predicted correct")+
  geom_point(alpha = 0.2, aes(color = as.factor(correct)))+
  geom_abline(slope = 1)

veh_results <- NULL
for(i in 1:length(files)){
  list_files <- list.files(file.path(dir,files[i]))
  for(j in 1:10){
    data = read.csv(file.path(dir, files[i], list_files[j]))
    data <- data %>%
      mutate(time = 15*i,
             sample_ID = paste0(sample_ID,j))
    print(nrow(data))
    veh_results <- rbind(veh_results, data)
  }
}

unique_veh <- unique(veh_results$sample_ID)

eye_results <- NULL
for(i in 1:length(files2)){
  list_files2 <- list.files(file.path(dir2,files2[i]))
  for(j in 1:10){
    data = read.csv(file.path(dir2, files2[i], list_files2[j]))
    data <- data %>%
      mutate(time = 15*i,
             sample_ID = paste0(sample_ID,j))
    print(nrow(data))
    eye_results <- rbind(eye_results, data)
  }
}

unique_eye <- unique(eye_results$sample_ID)

```
```{r}
veh_results <- NULL
for(i in 1:1){
  list_files <- list.files(file.path(dir,files[i]))
  for(j in 1:10){
    data = read.csv(file.path(dir, files[i], list_files[j]))
    data <- data %>%
      mutate(time = 15*i)
    veh_results <- rbind(veh_results, data)
  }
}
```

```{r}
data <- read_excel("C:/Users/satavans/Downloads/kss_RTD.xlsx")
```

```{r}
subject_no <- numeric(length(veh_results))
scenario <- numeric(length(veh_results))
sample_Id = veh_results$sample_ID
for (i in 1:length(sample_Id)) {
  parts <- str_split(sample_Id[i], "_")[[1]]
  subject_no[i] <- parts[1]
  scenario[i] <- parts[3]
}

veh_results1 <- veh_results %>%
  mutate(subject_no = subject_no,
         scenario = scenario)

data <- data%>%
  mutate(scenario = substr(Daq, 13,14),
         subject_no = as.character(`Subject id`))

comb <- veh_results1 %>%
  left_join(data, by = c("subject_no", "scenario"))
```

```{r}
comb_results <- comb %>%
  filter(target == 1)%>%
  group_by(rtd)%>%
  summarize(
    prob = sum(pred)/n(),
    n = n()
  )


kss_results <- comb%>%
  group_by(csra_kss_score)%>%
  summarize(
    prob = sum(pred)/ n(),
    n()
  )

drive_no_results <- comb%>%
  group_by(`Drive No`)%>%
  summarize(
    prob = sum(pred)/ n(),
    n = n()
  )

crive_kss_results <- comb%>%
  group_by(`Drive No`)%>%
  summarize(
    avg_kss = mean(csra_kss_score, na.rm = TRUE)
  )
```


```{r}
ggplot(comb_results, aes(x = as.factor(rtd), y = prob ))+
  geom_col()+
  labs(x = "Ready to drive", y = "Proportion classified as impaired", caption = "1 for ready, 0 for not ready", title = "Classification of Impaired Drivers Based on their Readiness to Drive")
```


```{r}
ggplot(kss_results, aes(x = csra_kss_score, y = prob))+
  geom_col()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  labs(y = "Proportion classified as impaired", x = "KSS Score(Fatigue rating)", title ="Classification of all Drivers based on their KSS Scores ", caption = "1 for no fatigue and 9 for very tired")
```

```{r}
ggplot(drive_no_results, aes(x = `Drive No`, y = prob ))+
  geom_col()+
  labs(y = "Proportion classified as impaired", x = "Drive Number", title ="Classification of all Drivers based on their Drive Number ", caption = "1 for placebo drive and 2,3,4,5 for impaired drives")
```

```{r}
ggplot(crive_kss_results, aes(x = `Drive No`, y = avg_kss))+
  geom_col()+
  labs(y = "Average KSS Score", x = "Drive Number", title ="Average KSS Score for drivers during their different Drives", caption = "1 for placebo drive and 2,3,4,5 for impaired drives")
```

