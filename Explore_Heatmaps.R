# Set Working Directory to MLINDIV master files
working_Dir <- "C:/Users/UCI - Robert Woodry/Desktop/Data Processing/R/MLINDIV"
setwd(working_Dir)

# Read in master files
bm <- read.csv("MLINDIV_behavioral_master.csv")
tm <- read.csv("MLINDIV_trial_master.csv")
pm <- read.csv("MLINDIV_participant_master.csv")

# For each subject's explore session, split behavioral data into time windows of window width window_width
#   and of step length window_by.

explore_windows <- function(data, window_width, window_by){
  
  # Create empty data frame where each row will contain a sliding time window of explore session data
  # TODO: define variables for ex_window data frame
  ex_windows <- data.frame()
  
  # Select only explore session data
  data <- data[which(data$Task == "Explore"), ]
  
  # Split behavioral master by subject
  split_by_subject <- split(data, data$Subject)
  
  for (i in 1:length(split_by_subject)){
    subject_data <- split_by_subject[[i]]
    subject_split_by_session <- split(subject_data, subject_data$Task_type)
    
    for (j in 1:length(subject_split_by_session)){
      # Split subject data into sessions, and for each one grab the choose onset times (wihtout NAs)
      subject_session_data <- subject_split_by_session[[j]]
      sub_times <- subject_session_data$Choose.OnsetTime
      sub_times <- sub_times[!is.na(sub_times)]
      
      # Subdivide session into equal time steps of window_width every window_by. Get intial time and increment.
      initial_time <- sub_times[1]
      end_time <- sub_times[length(sub_times)]
      
      win_starts <- seq(initial_time, end_time, window_by)
      win_ends <- win_starts + window_width
      win_ends[length(win_ends)] <- sub_times[length(sub_times)]
      
      # TODO: For each start-end window frame, grab all data rows which (use which function) Choose Onset times 
      #   fall within that window frame. Compile a dataframe row for ex_windows that contains pertinent info
      for (k in 1:length(win_starts)){
        
      }
    }
    
  }
}