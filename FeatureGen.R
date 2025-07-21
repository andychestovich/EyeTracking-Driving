
# also need to install rtools through CRAN
library(dplyr)
library(moments)
library("devtools")
# using saccades package to calculate certain variables
install_github("tmalsburg/saccades/saccades", dependencies=TRUE, force = TRUE)
library(saccades)
# File paths
root <- "C:/Users/chestovi/Downloads/"
start_directory <- "45SplitsCorrectedCSVs"
end_directory <- "45Splits_eyeReduced_saccades_Test"

# storing all the files 
files <- list.files(file.path(root, start_directory))
num_files = length(files)

#looping through files for generating features from data 
for(j in 1:num_files){
  #read specific file
  data = read.csv(file.path(root, start_directory, files[j]))
  data = as.data.frame(data)
  data <- data %>%
    # generate binary variable for whether the eyes are open 
    mutate(l_eye_close = ifelse(Output.FovioDMEResults.dme.core.eyelid.left.eyelid.closed == "true", 1 , 0),
           r_eye_close = ifelse(Output.FovioDMEResults.dme.core.eyelid.right.eyelid.closed == "true", 1 , 0), 
           id1 = paste0(id1, substr(DaqName, 13, 14)))
  
  #Saccade Section
  # get list of unique IDs
  id_list <- unique(data$id1)
  
  # make empty lists to serve as variables later
  saccades_results <- numeric(length(id_list)) 
  fixations <- numeric(length(id_list))  
  dur_fix <-numeric(length(id_list)) 
  horiz_disper <- numeric(length(id_list)) 
  vert_disper <- numeric(length(id_list))  
  pv_horiz <- numeric(length(id_list))  
  pv_vert <- numeric(length(id_list))  
  dur_fix_sd <- numeric(length(id_list))  
  horiz_disper_sd <- numeric(length(id_list))  
  vert_disper_sd <- numeric(length(id_list))  
  pv_horiz_sd <- numeric(length(id_list))  
  pv_vert_sd <- numeric(length(id_list))  
  
  # For each id 
  for(i in 1:length(id_list)){
    #filter the data for that ID 
    id_filtered_data <- data %>%
      filter(id1 == id_list[i])
    
    #format data as required by the package
    saccades <- id_filtered_data %>%
      select(id1, 
             Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.yaw.deg,
             Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.pitch.d, 
             Frames) %>%
      rename(trial = id1, 
             x = Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.yaw.deg, 
             y =  Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.pitch.d, 
             time = Frames)
    
    #detect fixations and calculate summary stats
    fix <- detect.fixations(saccades)
    saccades_summary <- calculate.summary(fix)
    
    #create variables
    fixations[i] <- saccades_summary$mean[3]
    dur_fix[i] <- saccades_summary$mean[4]
    horiz_disper[i] <- saccades_summary$mean[5]
    vert_disper[i] <- saccades_summary$mean[6] 
    pv_horiz[i] <- saccades_summary$mean[7]
    pv_vert[i] <- saccades_summary$mean[8]
    dur_fix_sd[i] <- saccades_summary$sd[4]
    horiz_disper_sd[i] <- saccades_summary$sd[5]
    vert_disper_sd[i] <- saccades_summary$sd[6] 
    pv_horiz_sd[i] <- saccades_summary$sd[7]
    pv_vert_sd[i] <- saccades_summary$sd[8]
  }
  #setting col names 
  saccades_results <- data.frame(id_list, fixations, dur_fix, horiz_disper, vert_disper, pv_horiz, pv_vert, dur_fix_sd,   horiz_disper_sd, vert_disper_sd, pv_horiz_sd, pv_vert_sd)
  
  colnames(saccades_results) = c("id1", "fixations", "dur_fix", "horiz_disper", "vert_disper", "pv_horiz", "pv_vert", "dur_fix_sd",   "horiz_disper_sd", "vert_disper_sd", "pv_horiz_sd", "pv_vert_sd")
  
  # other features
  data1 <- data %>%
    group_by(id1)%>%
    summarize(ID = unique(id1),
              Treat = unique(Condition),
              Status1 = unique(status1),
              Status2 = unique(status2),
              Status3 = unique(status3),
              Status4 = unique(status4),
              Status5 = unique(status5),
              Status6 = unique(status6),
              Status7 = unique(status7),
              Status8 = unique(status8),
              Status9 = unique(status9),
              Status10 = unique(status10),
              n = 5400, #window size as needed
              left_PERCLOS =sum(l_eye_close, na.rm = TRUE)/n,
              right_PERCLOS =  sum(r_eye_close, na.rm = TRUE)/n,
              Blinks = max(Output.FovioDMEResults.dme.core.eyelid.blink.counter, na.rm = TRUE)  -   min(Output.FovioDMEResults.dme.core.eyelid.blink.counter, na.rm = TRUE),
              # left pupil size 
              avg_left_pupil_size = mean(Output.FovioDMEResults.dme.core.pupil.left.pupil.diameter.mm, na.rm = TRUE), 
              left_pupil_size_q95 = quantile(Output.FovioDMEResults.dme.core.pupil.left.pupil.diameter.mm, probs = .95, na.rm = TRUE),
              left_pupil_size_q5 = quantile(Output.FovioDMEResults.dme.core.pupil.left.pupil.diameter.mm, probs = .05, na.rm = TRUE),
              sd_left_pupil_size = sd(Output.FovioDMEResults.dme.core.pupil.left.pupil.diameter.mm, na.rm = TRUE), 
              kurt_left_pupil_size = kurtosis(Output.FovioDMEResults.dme.core.pupil.left.pupil.diameter.mm, na.rm = TRUE), 
              skew_left_pupil_size = skewness(Output.FovioDMEResults.dme.core.pupil.left.pupil.diameter.mm, na.rm = TRUE),
              middle_90_left_pupil_size = left_pupil_size_q95 - left_pupil_size_q5,
              #right pupil size
              avg_right_pupil_size =  mean(Output.FovioDMEResults.dme.core.pupil.right.pupil.diameter.mm, na.rm = TRUE),
              right_pupil_size_q95 = quantile(Output.FovioDMEResults.dme.core.pupil.right.pupil.diameter.mm, probs = .95, na.rm = TRUE),
              right_pupil_size_q5 = quantile(Output.FovioDMEResults.dme.core.pupil.right.pupil.diameter.mm, probs = .05, na.rm = TRUE),
              sd_right_pupil_size = sd(Output.FovioDMEResults.dme.core.pupil.right.pupil.diameter.mm, na.rm = TRUE), 
              kurt_right_pupil_size = kurtosis(Output.FovioDMEResults.dme.core.pupil.right.pupil.diameter.mm, na.rm = TRUE), 
              skew_right_pupil_size = skewness(Output.FovioDMEResults.dme.core.pupil.right.pupil.diameter.mm, na.rm = TRUE),
              middle_90_right_pupil_size = right_pupil_size_q95 - right_pupil_size_q5,
              # head_pos_x
              avg_head_pos_x = mean(Output.FovioDMEResults.dme.core.head.head.pose.translation.x, na.rm = TRUE),
              head_pos_x_q95 = quantile(Output.FovioDMEResults.dme.core.head.head.pose.translation.x, probs = .95, na.rm = TRUE),
              head_pos_x_q5 = quantile(Output.FovioDMEResults.dme.core.head.head.pose.translation.x, probs = .05, na.rm = TRUE),
              sd_head_pos_x = sd(Output.FovioDMEResults.dme.core.head.head.pose.translation.x, na.rm = TRUE),
              kurt_head_pos_x = kurtosis(Output.FovioDMEResults.dme.core.head.head.pose.translation.x, na.rm = TRUE), 
              skew_head_pos_x = skewness(Output.FovioDMEResults.dme.core.head.head.pose.translation.x, na.rm = TRUE), 
              middle_90_head_pos_x = head_pos_x_q95 - head_pos_x_q5, 
              #head pos y
              avg_head_pos_y = mean(Output.FovioDMEResults.dme.core.head.head.pose.translation.y, na.rm = TRUE), 
              sd_head_pos_y = sd(Output.FovioDMEResults.dme.core.head.head.pose.translation.y, na.rm = TRUE),
              head_pos_y_q95 = quantile(Output.FovioDMEResults.dme.core.head.head.pose.translation.y, probs = .95, na.rm = TRUE),
              head_pos_y_q5 = quantile(Output.FovioDMEResults.dme.core.head.head.pose.translation.y, probs = .05, na.rm = TRUE),
              kurt_head_pos_y = kurtosis(Output.FovioDMEResults.dme.core.head.head.pose.translation.y, na.rm = TRUE), 
              skew_head_pos_y = skewness(Output.FovioDMEResults.dme.core.head.head.pose.translation.y, na.rm = TRUE), 
              middle_90_head_pos_y = head_pos_y_q95 - head_pos_y_q5, 
              #head pos z
              avg_head_pos_z = mean(Output.FovioDMEResults.dme.core.head.head.pose.translation.z, na.rm = TRUE), 
              sd_head_pos_z = sd(Output.FovioDMEResults.dme.core.head.head.pose.translation.z, na.rm = TRUE),
              head_pos_z_q95 = quantile(Output.FovioDMEResults.dme.core.head.head.pose.translation.z, probs = .95, na.rm = TRUE),
              head_pos_z_q5 = quantile(Output.FovioDMEResults.dme.core.head.head.pose.translation.z, probs = .05, na.rm = TRUE),
              kurt_head_pos_z = kurtosis(Output.FovioDMEResults.dme.core.head.head.pose.translation.z, na.rm = TRUE), 
              skew_head_pos_z = skewness(Output.FovioDMEResults.dme.core.head.head.pose.translation.z, na.rm = TRUE), 
              middle_90_head_pos_z = head_pos_z_q95 - head_pos_z_q5, 
              #eyes on road
              per_eyes_on_road = sum(Output.FovioDMEResults.das.das.state == "On Road(1)", na.rm = TRUE)/3600,
              # blink duration 
              avg_blink_dur = mean(Output.FovioDMEResults.dme.core.eyelid.average.blink.duration.u, na.rm = TRUE), 
              sd_blink_dur = sd(Output.FovioDMEResults.dme.core.eyelid.average.blink.duration.u, na.rm = TRUE), 
              skew_blink_dur = skewness(Output.FovioDMEResults.dme.core.eyelid.average.blink.duration.u, na.rm = TRUE), 
              kurt_blink_dur = kurtosis(Output.FovioDMEResults.dme.core.eyelid.average.blink.duration.u, na.rm = TRUE), 
              blink_dur_q95 = quantile(Output.FovioDMEResults.dme.core.eyelid.average.blink.duration.u, probs = .95, na.rm = TRUE), 
              blink_dur_q5 = quantile(Output.FovioDMEResults.dme.core.eyelid.average.blink.duration.u, probs = .05, na.rm = TRUE), 
              middle_90_blink_dur = blink_dur_q95 - blink_dur_q5)
  
  # gaze features
  data2 <- data %>%
    filter(Output.FovioDMEResults.dme.core.eyelid.right.eyelid.opening.val == "true")%>%
    filter(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.va == "true")%>%
    group_by(id1)%>%
    summarize(
      #gaze left 
      avg_xgaze_left = mean(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.x, na.rm = TRUE),
      avg_ygaze_left = mean(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.y, na.rm = TRUE),
      avg_zgaze_left = mean(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.z, na.rm = TRUE),
      avg_gaze_left = sqrt(avg_xgaze_left^2 + avg_ygaze_left^2 + avg_zgaze_left^2), 
      sd_xgaze_left = sd(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.x, na.rm = TRUE), 
      sd_ygaze_left = sd(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.y, na.rm = TRUE),
      sd_zgaze_left = sd(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.z, na.rm = TRUE),
      ygaze_left_q95 = quantile(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.y, probs = .95, na.rm = TRUE), 
      ygaze_left_q5 = quantile(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.y, probs = .05, na.rm = TRUE),
      xgaze_left_q95 = quantile(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.x, probs = .95, na.rm = TRUE),
      xgaze_left_q5 = quantile(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.x, probs = .05, na.rm = TRUE),
      zgaze_left_q95 = quantile(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.z, probs = .95, na.rm = TRUE),
      zgaze_left_q5 = quantile(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.z, probs = .05, na.rm = TRUE),
      ygaze_left_kurt = kurtosis(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.y, na.rm = TRUE), 
      xgaze_left_kurt = kurtosis(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.x, na.rm = TRUE),
      zgaze_left_kurt = kurtosis(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.z, na.rm = TRUE), 
      ygaze_left_skew = skewness(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.y, na.rm = TRUE), 
      xgaze_left_skew = kurtosis(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.x, na.rm = TRUE), 
      zgaze_left_skew = kurtosis(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.z, na.rm = TRUE), 
      middle_90_ygaze_left = ygaze_left_q95 - ygaze_left_q5, 
      middle_90_xgaze_left = xgaze_left_q95 - xgaze_left_q5,
      middle_90_zgaze_left = zgaze_left_q95 - zgaze_left_q5,
      #gaze right 
      avg_xgaze_right = mean(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.x, na.rm = TRUE),
      avg_ygaze_right = mean(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.y, na.rm = TRUE),
      avg_zgaze_right = mean(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.y, na.rm = TRUE),
      avg_gaze_right = sqrt(avg_xgaze_right^2 + avg_ygaze_right^2 + avg_zgaze_right^2), 
      sd_xgaze_right = sd(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.x, na.rm = TRUE), 
      sd_ygaze_right = sd(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.y, na.rm = TRUE),
      sd_zgaze_right = sd(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.z, na.rm = TRUE),
      ygaze_right_q95 = quantile(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.y, probs = .95, na.rm = TRUE), 
      ygaze_right_q5 = quantile(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.y, probs = .05, na.rm = TRUE),
      xgaze_right_q95 = quantile(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.x, probs = .95, na.rm = TRUE),
      xgaze_right_q5 = quantile(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.x, probs = .05, na.rm = TRUE),
      zgaze_right_q95 = quantile(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.z, probs = .95, na.rm = TRUE),
      zgaze_right_q5 = quantile(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.z, probs = .05, na.rm = TRUE),
      ygaze_right_kurt = kurtosis(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.y, na.rm = TRUE), 
      xgaze_right_kurt = kurtosis(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.x, na.rm = TRUE),
      zgaze_right_kurt = kurtosis(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.z, na.rm = TRUE),
      ygaze_right_skew = skewness(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.y, na.rm = TRUE),
      xgaze_right_skew = kurtosis(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.x, na.rm = TRUE), 
      zgaze_right_skew = kurtosis(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.z, na.rm = TRUE), 
      middle_90_ygaze_right = ygaze_right_q95 - ygaze_right_q5, 
      middle_90_xgaze_right = xgaze_right_q95 - xgaze_right_q5,
      middle_90_zgaze_right = zgaze_right_q95 - zgaze_right_q5)
  
  #pitch and yaw features
  data3 <- data %>%
    filter(Output.FovioDMEResults.dgaze.unified.gaze.direction.valid == "true")%>%
    group_by(id1)%>%
    summarize(
      #gaze direc pitch yaw 
      avg_pitch_unif_gaze = mean(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.pitch.d, na.rm = TRUE), 
      avg_yaw_unif_gaze = mean(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.yaw.deg, na.rm = TRUE), 
      sd_pitch_unif_gaze = sd(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.pitch.d, na.rm = TRUE), 
      sd_yaw_unif_gaze = sd(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.yaw.deg, na.rm = TRUE), 
      pitch_unif_q95 = quantile(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.pitch.d, .95, na.rm = TRUE), 
      yaw_unif_q95 = quantile(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.yaw.deg, .95, na.rm = TRUE),
      pitch_unif_q5 = quantile(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.pitch.d, .05, na.rm = TRUE), 
      yaw_unif_q5 = quantile(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.yaw.deg, .05, na.rm = TRUE),
      middle_90_pitch = pitch_unif_q95 - pitch_unif_q5,
      middle_90_yaw = yaw_unif_q95 - yaw_unif_q5, 
      kurt_pitch = kurtosis(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.pitch.d, na.rm = TRUE),
      kurt_yaw = kurtosis(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.yaw.deg, na.rm = TRUE), 
      skew_pitch = skewness(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.pitch.d, na.rm = TRUE), 
      skew_yaw = skewness(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.yaw.deg, na.rm = TRUE), 
      sd_comb = sd(sqrt(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.pitch.d^2 + Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.yaw.deg^2))
    )
  
  # joining all sub-data into a master dataset with all features
  data4 <- data1 %>%
    left_join(data2, by = "id1")%>%
    left_join(data3, by = "id1")%>%
    left_join(saccades_results, by = "id1")
  write.csv(data4, file.path(root, end_directory, files[j]))
  print(j)
}


