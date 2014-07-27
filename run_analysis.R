run_analysis<-function(){
  setwd("D:/Tom/Coursera/Data Science Specialization-Johns Hopkins/R_data/GCD/Project/repo")
  
  # Read in the data from files
  #subjects' ID run from 1 to 30
  subjectTest = read.table('../UCi HAR Dataset/test/subject_test.txt'); #imports subject_test.txt
  subjectTrain = read.table('../UCi HAR Dataset/train/subject_train.txt'); #imports subject_train.txt
  subject <- rbind(subjectTest, subjectTrain) #combine the two tables by rows
  colnames(subject) = "subjectID" #label the column

  activityTable = read.table('../UCi HAR Dataset/activity_labels.txt'); #imports activity_labels.txt -- ID Types 
  #Create a vector of the Activity Types
  aT <- vector("character",length=6) 
  for (i in 1:6){
    aT[i] = as.character(activityTable[i,2])
  }

  #The y_*.txt files have the IDs related to the names in the activity_labels.txt file
  yTest = read.table('../UCi HAR Dataset/test/y_test.txt'); #imports y_test.txt
  yTrain = read.table('../UCi HAR Dataset/train/y_train.txt'); #imports y_train.txt
  yData <- rbind(yTest, yTrain) #combine the two tables by rows
  #Create a numeric vector from the yData table
  yD <- vector("numeric",length=nrow(yData)) 
  for (i in 1:nrow(yData)){
    yD[i] = as.numeric(yData[i,1])
  }

  #transform the yData vector from a vector of activtivity IDs to a vector of activity types
  ydf <- vector("character",length=10299)
  for (i in 1:10299){
     yDI = as.numeric(yD[i])
     ydf[i] = aT[yDI]
   }
   y <- data.frame(ydf)
   colnames(y) = "activity" #label the column
  
  features = read.table('../UCi HAR Dataset/features.txt'); #imports features.txt
  # The column names for x_*.txt files is stored in the features.txt file
  xTest = read.table('../UCi HAR Dataset/test/x_test.txt'); #imports x_test.txt 
  xTrain = read.table('../UCi HAR Dataset/train/x_train.txt'); #imports x_train.txt
  x <- rbind(xTest, xTrain) #combine the two tables by rows
  colnames(x) = features[,2] #label the column -- first col of features is an ID, the second col is the names
  
  compTable<-cbind(subject,y,x) # The Complete Table
  
  # Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
  colNames  = colnames(compTable); 
  meanStdFreqLogical = (grepl("subject..",colNames) | grepl("activity",colNames) | grepl("-mean()..",colNames) | grepl("-std()..",colNames))
  
  # Subset compTable based on the meanStdLogical to keep only desired columns
  meanStdFreqTable = compTable[meanStdFreqLogical==TRUE];
  
  colNames  = colnames(meanStdFreqTable); 
  meanStdLogical = !grepl("-meanFreq()",colNames)
  
  # Subset meanStdFreqLogical based on the meanStdLogical to keep only desired columns
  meanStdTable = meanStdFreqTable[meanStdLogical==TRUE];

  #final tidy data set
  colNames  = colnames(meanStdTable); 
  tidyMeasures <- colNames[-c(1,2,82)]; #getting a list of measures to melt data
  tidyMelt <- melt(meanStdTable, id=c("subjectID", "activity"), measure.vars=c(tidyMeasures)); #melting tidy data set
  finalTable <- dcast(tidyMelt, activity + subjectID ~ variable, mean)
  write.table(finalTable,file="../tidyDataSet.txt", sep=",",row.names=FALSE)
  #finalTable
}