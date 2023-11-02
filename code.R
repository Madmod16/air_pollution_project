install.packages("stringr")
install.packages("purrr")
library(purrr)
library(stringr)

# A function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 
# The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 
# 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument 
# and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id = 1:332) {
  id_str <- str_pad(id, 3, pad = "0") #add 0 on the front of a number
  id_str_csv <- paste0(id_str,".csv") #add .csv to the number 
  
  path <- paste(directory, '/', sep = "") #create a path to the id files 
  
  file_set <- paste0(path, id_str_csv)#create a vector with names of files 
  
  #Create a data frame from the vector with names of files
  df<-
    file_set %>% #>%> put file_set in map_df(~read.csv(.))
    map_df(~read.csv(.))
  pollutant <- df[pollutant] #choose which column will be used sulfate or nitrate
  pollutant_no_na <- na.omit(pollutant)#clean column from NA
  
  #change list to vector, make the vector numeric
  #and find a mean of values in the column
  mean_pollutant <- mean(as.numeric(as.character(unlist(pollutant_no_na))))
  mean_pollutant
}


# function that reads a directory full of files and reports 
# the number of completely observed cases in each data file. 
# The function should return a data frame where the first column 
# is the name of the file and the second column is the number of complete cases. 

complete <- function(directory, id = 1:332) {
  id_str <- str_pad(id, 3, pad = "0") #add 0 on the front of a number
  id_str_csv <- paste0(id_str,".csv") #add .csv to the number 
  
  path <- paste(directory, '/', sep = "") #create a path to the id files 
  
  file_set <- paste0(path, id_str_csv)#create a vector with names of files 
  
  #Create a data frame with the id column 
  df_complete <- data.frame(id)
  
  #Create of the empty vector that will be filled with the complete cases
  df_num_complete <- c()
  
  #The loop that reads the file, cleans it from NA and add 
  #the complete cases (the length) of column with sulfate
  #to the vector df_num_complete
  for(i in file_set) 
  {
    temp_df <- read.csv(i)
    temp_df_data <- na.omit(temp_df$sulfate)
    df_num_complete <- c(df_num_complete , length(temp_df_data))
  }
  
  #Append the complete cases(df_num_complete) to data frame
  df_complete$nobs <- df_num_complete
  return(df_complete)
}

# Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation 
# between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is 
# greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold 
# requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.

corr <- function(directory, threshold  = 0) {
  df<-data.frame(complete(directory, 1:332)) #Create data frame of complete values
  if(max(df$nobs) > threshold) #Check if threshold corresponds to complete cases
  {
    names(df) <- c("id", "nobs") #Add the labels to data frame
    
    #Filter files with the number of complete cases that is lower then threshold
    df_threshold <- df[df$nobs > threshold,] 
    #Extract names of files with the number of complete cases that is higher then threshold
    file_list_numeric <- c(df_threshold$id) 
    
    id_str <- str_pad(file_list_numeric, 3, pad = "0") #add 0 on the front of a number
    id_str_csv <- paste0(id_str,".csv") #add .csv to the number 
    
    path <- paste(directory, '/', sep = "") #create a path to the id files 
    file_set <- paste0(path, id_str_csv)#create a vector with names of files 
    
    #Create of the empty vector that will be filled with the correlations 
    df_num_complete <- c()
    
    #The loop that reads the file
    #checks if class of the data frame's tables are numeric
    #fills the vector with the correlations of sulfate and nitrate from the file 
    for(i in file_set) 
    {
      temp_df <- read.csv(i)
      if(class(temp_df$sulfate) == "numeric" & class(temp_df$nitrate) == "numeric")
      {
        df_num_complete <- c(df_num_complete, cor(temp_df$sulfate, temp_df$nitrate ,use = "complete.obs"))
      }
    }
    return(df_num_complete)
  }
  else
  {
    return(numeric())
  }
}