# root directory (replace with your own)
root <- "C:/Users/chestovi/Downloads"

#starting and ending directory for reading and writing files
start_dir <- "60Second_overlap_reduced"
end_dir <- "60Sec_overlap_tv"

# creating training, validation variables that we can add data to 
training <- NULL
validation <- NULL 

#getting files
files <- list.files(file.path(root, start_dir))
num_files = length(files)

#looping through the files
for(i in 1:num_files)
{
  # reading in the reduced data
  reduce_data <- read.csv(file.path(root, start_dir, files[i]))
  
  #if a part of training set
  if(reduce_data$Status[1] == "t")
  {
    # binding the data on
    training <- rbind(training, reduce_data)
  }
  else{
    # binding the validation data
    validation <- rbind(validation, reduce_data)
  }
}

# writing csv for both training and validation 
write.csv(training, file.path(root, end_dir, "training.csv"))
write.csv(validation, file.path(root, end_dir, "validation.csv"))