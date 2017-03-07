# directory is fold path, target is column name which you want to calculate mean, id is order(1:3) of file not file name
pollutantmean <- function(directory, target, id) {
  setwd(directory);
  filenames <- list.files(directory, pattern="*.csv")
  #initianl a data table to store all the files needed
  store <- data.frame()
  for(i in id){
    
    originaldata <- read_csv(filenames[i])
 #after loop, combine all the files into one data table
    store <- rbind(store,originaldata)
  }
  #selectcol: select target column
  selectcol <- store[,target]
  # datawona: data without na
  datawona <- selectcol[!is.na(selectcol),]
  
  mean <- mean(dat[[target]])
  mean
  
}

#command:pollutantmean ("F:/course/open course/R-JH/assignment/assignment1/specdata", "nitrate", 1:3)