# Getting and Cleaning Data
# Coursera Project Course Assignment

# library
library(plyr)

# download file if it isn't already there
if (!file.exists("UCI HAR Dataset.zip")) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        fileName <- "UCI HAR Dataset.zip"
        download.file(fileUrl, destfile=fileName)
        unzip(fileName)
        dateDownloaded <- date()
}

# read in variable names
filePath <- "./UCI HAR Dataset/"
features.txt <- read.table(paste(filePath,"features.txt",sep=""))
variable.names <- as.character(features.txt[,2])

# create logical vector to extract a subset of columns
contains.mean <- grepl("mean()",variable.names,fixed=TRUE)
contains.std <- grepl("std()",variable.names,fixed=TRUE)        
selected.cols <- contains.mean | contains.std               

# clean column variable names
names <- gsub("()","",variable.names,fixed=TRUE)
names <- gsub("-","",names,fixed=TRUE)
names <- gsub("mean",".Mean",names,fixed=TRUE)
names <- gsub("std",".Std",names,fixed=TRUE)

# read training measurements
measure.train <- read.table(paste(filePath,"train/X_train.txt",sep=""), 
                        col.names=as.character(names))  
# read test measurements
measure.test <- read.table(paste(filePath,"test/X_test.txt",sep=""),
                         col.names=as.character(names))   

# extract columns using logical vector
data.train <- measure.train[,selected.cols == TRUE]
datasource <- rep(c("train"), nrow(data.train))
data.train <- cbind(datasource, data.train )

data.test <- measure.test[,selected.cols == TRUE]
datasource <- rep(c("test"), nrow(data.test))
data.test <- cbind(datasource, data.test )

# combine training and test
data = rbind(data.train, data.test)

# define subject ID
subject.id.train <- read.table(paste(filePath,"train/subject_train.txt",sep=""),
                         col.names=c("subject.id"))  
subject.id.test <- read.table(paste(filePath,"test/subject_test.txt",sep=""),
                         col.names=c("subject.id"))  
subject.id = rbind(subject.id.train, subject.id.test)           


# define activity ID
activity.id.train <- read.table(paste(filePath,"train/y_train.txt",sep=""),   
                              col.names=c("id"))
activity.id.test <- read.table(paste(filePath,"test/y_test.txt",sep=""),   
                          col.names=c("id"))
activity.id = rbind(activity.id.train, activity.id.test)        

# read description labels activities
activity.labels <- read.table(paste(filePath,"activity_labels.txt",sep=""),
                              col.names=c("id","activity")) 

# join tables to get activity description instead of activity number
activity = join(activity.id, activity.labels)

# combine dataset with Subject ID and Activity columns
x <- cbind(activity$activity, activity$id, data)
names(x)[1] <- "activity"
names(x)[2] <- "activity.id"
xx <- cbind(subject.id, x)

# sort dataset by (subject.id, activity)
results <- xx[order(xx$subject.id, xx$activity),]
row.names(results) <- NULL

# create new tidy dataset
tidydata <-aggregate(results, by=list(results$datasource, 
                                      results$subject.id,
                                      results$activity), FUN=mean)

tidydata <- subset(tidydata,,-c(subject.id, activity, datasource))
names(tidydata)[1] <- "datasource" 
names(tidydata)[2] <- "subject.id"                  
names(tidydata)[3] <- "activity"  

# clean names
tidynames <- tolower(names(tidydata))
tidynames <- gsub("acc","accelerometer",tidynames,fixed=TRUE)
tidynames <- gsub("gyro","gyroscope",tidynames,fixed=TRUE)
tidynames <- gsub("mag","magnitude",tidynames,fixed=TRUE)
tidynames <- gsub("std","standarddeviation",tidynames,fixed=TRUE)
tidynames <- gsub("freq","frequency",tidynames,fixed=TRUE)
tidynames <- gsub("tbody","timebody",tidynames,fixed=TRUE)
tidynames <- gsub("tgravity","timegravity",tidynames,fixed=TRUE)
tidynames <- gsub("fbody","fastfouriertransformbody",tidynames,fixed=TRUE)
tidynames <- gsub(".","",tidynames,fixed=TRUE)

names(tidydata) <- tidynames

# write output
write.csv(tidydata, "UCI-HAR-tidydata.txt", row.names=FALSE)

