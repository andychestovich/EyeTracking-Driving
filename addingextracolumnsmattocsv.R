
library(R.matlab, quietly = TRUE, warn.conflicts = FALSE) # Reads matlab files
library(stringr, quietly = TRUE, warn.conflicts = FALSE) # String manipulation
library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(readxl, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(progress, quietly = TRUE)

# converting .mat file to .csv 
mattocsv <- function(fileName){ 
  
  temp <- readMat.default(file.path(root, "matfiles", fileName))
  
  # store the mat file so that we dont accidentally overwrite the original since it takes awhile to load in 
  obj3 <- temp
  #get num of cols
  col_num <- length(obj3$elemDataI)
  
  # length of the frame column and thus ;ength of the data 
  no_rows <- length(temp$elemDataI[[38]][,1])
  # create appropriate sized df
  df <- data.frame(rows = seq(1:no_rows))
  
  # loop through columns
  for(i in 1:col_num){
    if(!is.list(temp$elemDataI[[i]][1,])){
      #copy df's column vals 
      df2 <- as.data.frame(obj3$elemDataI[[i]][,1])
      # add them to our ongoing df
      df <- data.frame(df, df2)
    }
    else
    {
      df2 <- as.data.frame(as.character(unlist(obj3$elemDataI[[i]][,1])))
      # add them to our ongoing df
      df <- data.frame(df, df2)
    }
  }
  # create list for col names 
  col_name <- list(length = col_num+1)
  col_name[1] <- "rows"
  
  for(i in 1:col_num){
    # gettting the name of columns by manipulating and using substring
    col_name[i+1] = strsplit(substring(deparse(obj3$elemDataI[,,1][i])[1], 6), split = " ")[[1]][1]
  }
  # assigning column names to the dataframe
  colnames(df) = col_name
  
  # making a Daqame using the name of the file and then joining the disposition file on that
  df <- df%>%
    mutate(DaqName = str_replace(fileName, ".mat", ".daq"))%>%
    left_join(disp_new, by = c("DaqName" = "DaqName"))
  
  # Create directory if if doesn't already exist
  if(!dir.exists(file.path(root, 'csvfiles'))) {
    dir.create(file.path(root, 'csvfiles'))
  }
  
  # writing all the files to the directory
  write.csv(df, file = file.path(root, "csvfiles", paste0(substr(fileName, 1, nchar(fileName)-4), ".csv")))
  
}

# loading disposition file  
disp_new <- read_csv("path/to/disposition")

# root directory 
root = file.path('/root')

# storing the name of files in a list to easily loop through
files <- list.files(file.path(root, "matfiles")) # list of name of .mat files

# progress bar to see if the code is writing files properly
# adapted and modified from Simon Hodson's code for converting files to csv 
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(files), show_after = 0)
pb$tick(0)
for (i in 1:length(files)) {
  if(file.exists(file.path(root, 'csvfiles', paste0(substr(files[i], 1, nchar(files[i])-4), ".csv")))) {
    #message(paste(fileNames[i], "elem data already converted to CSV."))
    print("yes")
  } else {
    mattocsv(files[i])
  }
  pb$tick()
}

# getting logstream and lanedev variables 
df1$LaneDev2 = NULL
df1$LaneDev2 = mat1$elemDataI[[26]][,2]

df1$LogStreams2 = NULL 
df1$LogStreams2 = mat1$elemDataI[[27]][,2]
mat1$elemdataI


# function to add the additional columns needed
addcols <- function(fileName){ 
  
  temp <- readMat.default(file.path(root, "matfiles", fileName))
  df1 <- read.csv(file.path(root, "csvfiles", fileName))
  
  # store the mat file so that we dont accidentally overwrite the original since it takes awhile to load in 
  obj3 <- temp
  
  
  df1$LaneDev2 = NULL
  df1$LaneDev2 = temp$elemDataI[[26]][,2]
  
  df1$LogStreams2 = NULL 
  df1$LogStreams2 = temp$elemDataI[[27]][,2]
  
  # Create directory if if doesn't already exist
  if(!dir.exists(file.path(root, 'newcsvfiles'))) {
    dir.create(file.path(root, 'newcsvfiles'))
  }
  
  # writing all the files to the directory
  write.csv(df1, file = file.path(root, "newcsvfiles", paste0(substr(fileName, 1, nchar(fileName)-4), ".csv")))
  
}


# root directory 
root = file.path("C:/Users/satavans/Downloads/matfiles")

# storing the name of files in a list to easily loop through
files <- list.files(file.path(root, "matfiles")) # list of name of .mat files

# progress bar to see if the code is writing files properly
# adapted and modified from Simon Hodson's code for converting files to csv 
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(files), show_after = 0)
pb$tick(0)
for (i in 1:length(files)) {
  if(file.exists(file.path(root, 'csvfiles', paste0(substr(files[i], 1, nchar(files[i])-4), ".csv")))) {
    #message(paste(fileNames[i], "elem data already converted to CSV."))
    print("yes")
  } else {
    addcols(files[i])
  }
  pb$tick()
}













