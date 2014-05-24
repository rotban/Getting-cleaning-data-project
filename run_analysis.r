

# Step1: set working directory #


# Step2: download and unzip the files in working directory. Skip this step if a folder "UCI HAR Dataset" already exists in your working directory and go to step 3.

fileurl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile="getdata-projectfiles-UCI HAR Dataset.zip")
unzip("getdata-projectfiles-UCI HAR Dataset.zip")


#Step 3: Select all the script below and hit Ctr+Ent"

# load required packages #
library(plyr)
library(reshape2)

# read label files #
setwd("./UCI HAR Dataset")
variable.list <- read.table("features.txt",sep="")
activity.lookup <- read.table("activity_labels.txt",sep="")

colnames(activity.lookup) <- c("activity_id","activity_label")

# read test data #
setwd("./test")
test.subject.id <- read.table("subject_test.txt",sep="")
test.raw.data <- read.table("X_test.txt", sep="")
test.activity.id <- read.table("y_test.txt", sep="")

colnames(test.subject.id) <- "subject_id"
colnames(test.activity.id) <- "activity_id"
colnames(test.raw.data) <- variable.list[,2]

test.activity.data <- join(test.activity.id, activity.lookup, by = "activity_id")
test.activity.label <- test.activity.data["activity_label"]

test.data <- cbind(test.subject.id,test.activity.label,test.raw.data)


# read train data #
setwd("../train")
train.subject.id <- read.table("subject_train.txt",sep="")
train.raw.data <- read.table("X_train.txt", sep="")
train.activity.id <- read.table("y_train.txt", sep="")

colnames(train.subject.id) <- "subject_id"
colnames(train.activity.id) <- "activity_id"
colnames(train.raw.data) <- variable.list[,2]

train.activity.data <- join(train.activity.id, activity.lookup, by = "activity_id")
train.activity.label <- train.activity.data["activity_label"]

train.data <- cbind(train.subject.id, train.activity.label, train.raw.data)

# merge test and train data #
data <- rbind(test.data, train.data)


# Extract mean and std dev columns #
splitcol <- data.frame(variable.list$V1, do.call('rbind', strsplit(as.character(variable.list$V2),'-',fixed=TRUE)))
mean.std <- subset(splitcol, xor(X2 == "std()", X2=="mean()"))
variable.number <- mean.std[1]
colnames(variable.number) <- "V1"
subset.variables <- merge(variable.list, variable.number, by="V1", all=F)
var.names <- do.call(paste0, subset.variables[c(2)])
data.subset <- data[var.names]
subject.activity <- data[c(1,2)]
final.subset <- cbind(subject.activity, data.subset)

# rename variables #
clean.subset <- rename(final.subset, c(
    "tBodyAcc-mean()-X"             = "timebodyaccmeanofx",
    "tBodyAcc-mean()-Y"             = "timebodyaccmeanofy",
    "tBodyAcc-mean()-Z"             = "timebodyaccmeanofz", 
    "tBodyAcc-std()-X"              = "timebodyaccstdofx",
    "tBodyAcc-std()-Y"              = "timebodyaccstdofy",
    "tBodyAcc-std()-Z"              = "timebodyaccstdofz",
    "tGravityAcc-mean()-X"          = "timegravityaccmeanofx",
    "tGravityAcc-mean()-Y"          = "timegravityaccmeanofy",
    "tGravityAcc-mean()-Z"          = "timegravityaccmeanofz",
    "tGravityAcc-std()-X"           = "timegravityaccstdofx", 
    "tGravityAcc-std()-Y"           = "timegravityaccstdofy",
    "tGravityAcc-std()-Z"           = "timegravityaccstdofz",
    "tBodyAccJerk-mean()-X"         = "timebodyaccjerkmeanofx",     
    "tBodyAccJerk-mean()-Y"         = "timebodyaccjerkmeanofy",       
    "tBodyAccJerk-mean()-Z"         = "timebodyaccjerkmeanofz",    
    "tBodyAccJerk-std()-X"          = "timebodyaccjerkstdofx",      
    "tBodyAccJerk-std()-Y"          = "timebodyaccjerkstdofy",   
    "tBodyAccJerk-std()-Z"          = "timebodyaccjerkstdofz", 
    "tBodyGyro-mean()-X"            = "timebodygyromeanofx",  
    "tBodyGyro-mean()-Y"            = "timebodygyromeanofy",  
    "tBodyGyro-mean()-Z"            = "timebodygyromeanofz", 
    "tBodyGyro-std()-X"             = "timebodygyrostdofx", 
    "tBodyGyro-std()-Y"             = "timebodygyrostdofy",
    "tBodyGyro-std()-Z"             = "timebodygyrostdofz",   
    "tBodyGyroJerk-mean()-X"        = "timebodygyrojerkmeanofx",
    "tBodyGyroJerk-mean()-Y"        = "timebodygyrojerkmeanofy",   
    "tBodyGyroJerk-mean()-Z"        = "timebodygyrojerkmeanofz", 
    "tBodyGyroJerk-std()-X"         = "timebodygyrojerkstdofx", 
    "tBodyGyroJerk-std()-Y"         = "timebodygyrojerkstdofy", 
    "tBodyGyroJerk-std()-Z"         = "timebodygyrojerkstdofz",  
    "tBodyAccMag-mean()"            = "timebodyaccmagmean",   
    "tBodyAccMag-std()"             = "timebodyaccmagstd",  
    "tGravityAccMag-mean()"         = "timegravityaccmagmean", 
    "tGravityAccMag-std()"          = "timegravityaccmagstd",
    "tBodyAccJerkMag-mean()"        = "timebodyaccjerkmagmean",
    "tBodyAccJerkMag-std()"         = "timebodyaccjerkmagstd",   
    "tBodyGyroMag-mean()"           = "timebodygyromagmean",
    "tBodyGyroMag-std()"            = "timebodygyromagstd",    
    "tBodyGyroJerkMag-mean()"       = "timebodygyrojerkmagmean", 
    "tBodyGyroJerkMag-std()"        = "timebodygyrojerkmagstd", 
    "fBodyAcc-mean()-X"             = "freqbodyaccmeanofx",
    "fBodyAcc-mean()-Y"             = "freqbodyaccmeanofy",       
    "fBodyAcc-mean()-Z"             = "freqbodyaccmeanofz",
    "fBodyAcc-std()-X"              = "freqbodyaccstdofx",        
    "fBodyAcc-std()-Y"              = "freqbodyaccstdofy", 
    "fBodyAcc-std()-Z"              = "freqbodyaccstdofz",        
    "fBodyAccJerk-mean()-X"         = "freqbodyaccjerkmeanofx",     
    "fBodyAccJerk-mean()-Y"         = "freqbodyaccjerkmeanofy",    
    "fBodyAccJerk-mean()-Z"         = "freqbodyaccjerkmeanofz",
    "fBodyAccJerk-std()-X"          = "freqbodyaccjerkstdofx",    
    "fBodyAccJerk-std()-Y"          = "freqbodyaccjerkstdofy",
    "fBodyAccJerk-std()-Z"          = "freqbodyaccjerkstdofz",    
    "fBodyGyro-mean()-X"            = "freqbodygyromeanofx",
    "fBodyGyro-mean()-Y"            = "freqbodygyromeanofy",    
    "fBodyGyro-mean()-Z"            = "freqbodygyromeanofz",
    "fBodyGyro-std()-X"             = "freqbodygyrostdofx",    
    "fBodyGyro-std()-Y"             = "freqbodygyrostdofy",   
    "fBodyGyro-std()-Z"             = "freqbodygyrostdofz",       
    "fBodyAccMag-mean()"            = "freqbodyaccmagmean",   
    "fBodyAccMag-std()"             = "freqbodyaccmagstd",    
    "fBodyBodyAccJerkMag-mean()"    = "freqbodyaccjerkmagmean",
    "fBodyBodyAccJerkMag-std()"     = "freqbodyaccjerkmagstd",
    "fBodyBodyGyroMag-mean()"       = "freqbodygyromagmean",
    "fBodyBodyGyroMag-std()"        = "freqbodygyromagstd",
    "fBodyBodyGyroJerkMag-mean()"   = "freqbodygyrojerkmagmean",
    "fBodyBodyGyroJerkMag-std()"    = "freqbodygyrojerkmagstd"
    ))


# Reshaping the data #

clean.melt <- melt(clean.subset, id=c("subject_id", "activity_label"), measure.vars = c(
    "timebodyaccmeanofx",
    "timebodyaccmeanofy",
    "timebodyaccmeanofz", 
    "timebodyaccstdofx",
    "timebodyaccstdofy",
    "timebodyaccstdofz",
    "timegravityaccmeanofx",
    "timegravityaccmeanofy",
    "timegravityaccmeanofz",
    "timegravityaccstdofx", 
    "timegravityaccstdofy",
    "timegravityaccstdofz",
    "timebodyaccjerkmeanofx",     
    "timebodyaccjerkmeanofy",       
    "timebodyaccjerkmeanofz",    
    "timebodyaccjerkstdofx",      
    "timebodyaccjerkstdofy",   
    "timebodyaccjerkstdofz", 
    "timebodygyromeanofx",  
    "timebodygyromeanofy",  
    "timebodygyromeanofz", 
    "timebodygyrostdofx", 
    "timebodygyrostdofy",
    "timebodygyrostdofz",   
    "timebodygyrojerkmeanofx",
    "timebodygyrojerkmeanofy",   
    "timebodygyrojerkmeanofz", 
    "timebodygyrojerkstdofx", 
    "timebodygyrojerkstdofy", 
    "timebodygyrojerkstdofz",  
    "timebodyaccmagmean",   
    "timebodyaccmagstd",  
    "timegravityaccmagmean", 
    "timegravityaccmagstd",
    "timebodyaccjerkmagmean",
    "timebodyaccjerkmagstd",   
    "timebodygyromagmean",
    "timebodygyromagstd",    
    "timebodygyrojerkmagmean", 
    "timebodygyrojerkmagstd", 
    "freqbodyaccmeanofx",
    "freqbodyaccmeanofy",       
    "freqbodyaccmeanofz",
    "freqbodyaccstdofx",        
    "freqbodyaccstdofy", 
    "freqbodyaccstdofz",        
    "freqbodyaccjerkmeanofx",     
    "freqbodyaccjerkmeanofy",    
    "freqbodyaccjerkmeanofz",
    "freqbodyaccjerkstdofx",    
    "freqbodyaccjerkstdofy",
    "freqbodyaccjerkstdofz",    
    "freqbodygyromeanofx",
    "freqbodygyromeanofy",    
    "freqbodygyromeanofz",
    "freqbodygyrostdofx",    
    "freqbodygyrostdofy",   
    "freqbodygyrostdofz",       
    "freqbodyaccmagmean",   
    "freqbodyaccmagstd",    
    "freqbodyaccjerkmagmean",
    "freqbodyaccjerkmagstd",
    "freqbodygyromagmean",
    "freqbodygyromagstd",
    "freqbodygyrojerkmagmean",
    "freqbodygyrojerkmagstd"))

tidyData <- dcast(clean.melt, subject_id + activity_label ~ variable, mean)

# Write txt file in the working directory #
setwd("../")
setwd("../")
write.table(tidyData, file="tidyData.txt", sep="\t")
