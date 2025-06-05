# File path
root <- "C:/Users/chestovi/Downloads/"

#Starting and ending directories
start_directory <- "Split_assignedCSV"
end_directory <- "60Second_overlap_reduced"

# storing all the files 
files <- list.files(file.path(root, start_directory))
num_files = length(files)

# looping through files for generating features from data 
for(i in 1:num_files){
  #read specific file and ensure that it is a data frame
  data = read.csv(file.path(root, start_directory, files[i]))
  data = as.data.frame(data)
  
  # generate binary variable for whether the eyes are open 
  data <- data %>%
    mutate(l_eye_close = ifelse(Output.FovioDMEResults.dme.core.eyelid.left.eyelid.closed == "true", 1 , 0),
           r_eye_close = ifelse(Output.FovioDMEResults.dme.core.eyelid.right.eyelid.closed == "true", 1 , 0))
  
  #grouping by ID, and generating relevant features (Will likely be updated as we try new features)
  data1 <- data %>%
    group_by(id1)%>%
    summarize(ID = unique(id1),
              Treat = unique(Treat), 
              Status = unique(status),
              n = 3600, 
              left_PERCLOS =sum(l_eye_close, na.rm = TRUE)/n,
              right_PERCLOS =  sum(r_eye_close, na.rm = TRUE)/n,
              Blinks = max(Output.FovioDMEResults.dme.core.eyelid.blink.counter, na.rm = TRUE)  -   min(Output.FovioDMEResults.dme.core.eyelid.blink.counter, na.rm = TRUE),
              avg_left_pupil_size = mean(Output.FovioDMEResults.dme.core.pupil.left.pupil.diameter.mm, na.rm = TRUE),
              avg_right_pupil_size =  mean(Output.FovioDMEResults.dme.core.pupil.right.pupil.diameter.mm, na.rm = TRUE), 
              sd_left_pupil_size = sd(Output.FovioDMEResults.dme.core.pupil.left.pupil.diameter.mm, na.rm = TRUE), 
              sd_right_pupil_size = sd(Output.FovioDMEResults.dme.core.pupil.right.pupil.diameter.mm, na.rm = TRUE),
              avg_head_pos_x = mean(Output.FovioDMEResults.dme.core.head.head.pose.translation.x, na.rm = TRUE),
              sd_head_pos_x = sd(Output.FovioDMEResults.dme.core.head.head.pose.translation.x, na.rm = TRUE),
              avg_head_pos_y = mean(Output.FovioDMEResults.dme.core.head.head.pose.translation.y, na.rm = TRUE), 
              sd_head_pos_y = sd(Output.FovioDMEResults.dme.core.head.head.pose.translation.y, na.rm = TRUE),
              avg_head_pos_z = mean(Output.FovioDMEResults.dme.core.head.head.pose.translation.z, na.rm = TRUE), 
              sd_head_pos_z = sd(Output.FovioDMEResults.dme.core.head.head.pose.translation.z, na.rm = TRUE))
  
  # seperate pipe as we need to filter for when the validation for the sensors working are true
  data2 <- data %>%
    filter(Output.FovioDMEResults.dme.core.eyelid.right.eyelid.opening.val == "true")%>%
    filter(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.va == "true")%>%
    group_by(id1)%>%
    summarize(sd_x_left = sd(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.x, na.rm = TRUE), 
              sd_y_left = sd(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.y, na.rm = TRUE),
              sd_z_left = sd(Output.FovioDMEResults.dme.core.gaze.left.eye.gaze.direction.y, na.rm = TRUE), 
              sd_x_right = sd(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.x, na.rm = TRUE), 
              sd_y_right = sd(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.y, na.rm = TRUE),
              sd_z_right = sd(Output.FovioDMEResults.dme.core.gaze.right.eye.gaze.direction.y, na.rm = TRUE)
    )
  # again, a seperate pipe to make the unified gaze direction is true 
  data3 <- data %>%
    filter(Output.FovioDMEResults.dgaze.unified.gaze.direction.valid == "true")%>%
    group_by(id1)%>%
    summarize(sd_pitch_unif = sd(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.pitch.d, na.rm = TRUE), 
              sd_yaw_unif = sd(Output.FovioDMEResults.dgaze.unified.gaze.direction.deg.yaw.deg, na.rm = TRUE))
  
  # joining all the data together 
  data4 <- data1 %>%
    left_join(data2, by = "id1")%>%
    left_join(data3, by = "id1")
  
  #writing file in end directory with original name
  write.csv(data4, file.path(root, end_directory, files[i]))
  
  # printing to view progress / see when loop potentially terminates due to a file with bad data
  print(i)
}