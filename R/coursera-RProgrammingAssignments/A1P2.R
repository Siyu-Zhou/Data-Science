# directory is fold path, target is column name which you want to calculate mean, id is order(1:3) of file not file name
complete <- function(directory, id) {
  setwd(directory)
  filenames <- list.files(directory, pattern="*.csv")
  #initianl a data table to store all the files needed
  store <- data.frame()
  for(i in id){
    #if I named read_csv(filenames[i]) as filenames
    #it will be error :`file` must be a string, raw vector or a connection.
    #so use different name when read file
    originaldata <- read_csv(filenames[i])
    #complete is a logic victor
    complete <- complete.cases(originaldata)
    nob <- sum(complete) 
    #combine i and nob in each file
    nobs <- data.frame(i,nob)
    #after loop, combine all the files into one data table
    store <- rbind(store,nobs)
  }
    store
  
}

#command:complete("F:/course/open course/R-JH/assignment/assignment1/specdata", 1:2)