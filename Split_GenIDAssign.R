#loading libraries needed
library(dplyr)
library(tidyverse)

# function to add splits 
add_sample_idx = function(data, len = 3600){
  
  l = nrow(data)
  
  if(l > len){
    ## Same 60-sec samples as before
    sid1 = sort(rep(1:floor(l/len), times = len))
    new_data1 = cbind(data[1:length(sid1), ], Sample_ID = sid1)
    
    ## Need to offset to get overlapping ones
    new_data2 = new_data1[seq(-1,-len/2, by = -1), ]
    sid2 = sort(rep((max(sid1)+1):(max(sid1) + floor(nrow(new_data2)/len)), times = len )) ## Start ID at max+1 of non-overlap
    new_data2 = cbind(select(new_data1[1:length(sid2),], -Sample_ID), Sample_ID = sid2)
    
    return(rbind(new_data1, new_data2))
  } else { 
    # return null if not even 60 seconds of time on desired logstream
    return(NULL)
  }
}

# function to make distinct IDs for data
make_ids = function(data1){
  final <- data1 %>%
    # create ID with subject ID of driver, the split, and their impairment status
    mutate(id1 = paste0(data1$Subject,"_",data1$Sample_ID,"_",data1$Treat))
  
  return(final)
}

#root directory 
root <- "C:/Users/chestovi/Downloads"

# will get by looping through files
start_directory <- "NotSplit_assignedCSV"
end_directory <- "Split_assignedCSV"

#getting files
files <- list.files(file.path(root, start_directory))
num_files = length(files)

#percentage in training group
training_per = 0.667

# number of training and validation in a split
nt = round(num_files*training_per)
nv = num_files - nt

# generating list with number of training and validation needed 
train_list <- c(rep("t", nt), rep("v", nv))
#shuffle
train_list <- sample(train_list)

test <- NULL

# looping through files
for(i in 1:num_files){
  data = read.csv(file.path(root, start_directory, files[i]))
  
  data <- data %>%
    # get rid of duplicate column
    select(- X) %>%
    #assign treatment
    mutate(Treat = substring(data$DaqName, 13, 14))%>%
    #filter rural straight (double check)
    filter(SCC.LogStreams == 22)
  
  # if data isn't big enough, print the problematic file name 
  if(nrow(data) < 3600){
    print(train_list[i - 1])
  }
  else{
    # running all the functions in a pipe 
    test <- data %>%
      add_sample_idx()%>%
      make_ids()%>%
      mutate(status = train_list[i])
    
    write.csv(test, file.path(root, end_directory, files[i]))
  }
  # print after each loop to track progress
  print(i - 1)
}