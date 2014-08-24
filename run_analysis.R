############################################################
### Course Project Getting and Cleaning Data MOOC Script ###
###
### August 24, 2014
### Data source https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
### Data downloaded "Sun Aug 24 14:50:48 2014"
###
### Scripts will work as long as the UCI HAR DATASET directory 
### is placed in the working directory.
###
### Based on the data in UCI HAR DATASET, the code constructs a
### dataset with observations for 30 subjects on 6 activites 
### (that is, 30x6=180 subject-activity observations).
###
### Specifically, the data are averages of the mean and std data from
### from the variables listed in the "features" file.
### The instructions are a bit unclear if all variables with "mean"
### should be included (for instance, is "meanfreq" included?) or just  
### those that include mean(). In the output data, I have included all
### possibly relevant variables. however, I have included an additional  
### code that, if uncommented, will de-select the irrelvant variables. 
###
### Hi from Norway!



###########################
### Reading in the data ###

# Files from top level of UCI HAR Dataset directory
#   Activity gives labels for each of the 6 types of activities for which data was collected
#   Features gives labels for each of 561 variables on which data was collected
activity_labels <- read.table("UCI HAR DATASET/activity_labels.txt") 
features <- read.table("UCI HAR DATASET/features.txt") 

# Files from the test subdirectory of UCI HAR dataset
#   X_test is data om the 561 variables ("features")
#   y_test associates each observation with an activity
#   subject_test associates each observation with a subject
X_test <- read.table("UCI HAR DATASET/test/X_test.txt") 
y_test <- read.table("UCI HAR DATASET/test/y_test.txt") 
subject_test <- read.table("UCI HAR DATASET/test/subject_test.txt") 

# Files from the train subdirectory of UCI HAR dataset
#   Same structure as the files in the test directory
X_train <- read.table("UCI HAR DATASET/train/X_train.txt") 
y_train <- read.table("UCI HAR DATASET/train/y_train.txt") 
subject_train <- read.table("UCI HAR DATASET/train/subject_train.txt") 



###################################################
### Combine the data and add names to variables ###

# Attach subject and activity labels to test and train data using cbind
test_labels <- cbind(y_test, subject_test)
test_data <- cbind(test_labels, X_test)
train_labels <- cbind(y_train, subject_train)
train_data <- cbind(train_labels, X_train)

# Combine test and train data using rbind 
data_raw <- rbind(test_data, train_data)

# Add descriptive labels to the variables
# Here I use the variable names provided in the features file
labels <- tolower(c("activity", "subject", as.character(features[,2])))
names(data_raw) <- labels



######################
### Data selection ###

# Extract the measurements on the mean and standard deviation for each measurement.

feature_select <- grep(c("activity|subject|mean|std"), names(data_raw))

data_selection <- data_raw[, labels[feature_select]]
# Note: This includes all variables including the text "mean"
# To exclude the variables "meanfreq, gravitymean, tbodyaccmean"
# uncomment the two commands listed below 
#   feature_deselect <- grep(c("meanfreq|tbodyaccmean|gravitymean"), names(data_selection))
#   data_selection <- subset(data_selection, select = -feature_deselect )



################################################################
### Computes average of every columm and create tidy dataset ###

# Split, apply, combine structure: 
#   Split creates a set of 180 dataframes (30 subjects x 6 activities).
#   lapply with colmeans computes the mean of every observation
#   do.call is used to recombine the data frames 
data_split <- split(data_selection, list(data_selection$subject, data_selection$activity))
data_avg_subj_act <- lapply(data_split, colMeans)
data_means<-do.call(rbind, data_avg_subj_act)

# Add descriptive activity names to name the activities in the data set
names(activity_labels) <- (c("activity", "activity name"))
data <- merge(activity_labels, data_means, by="activity", all=FALSE)

# Prints the data to a text file 
write.table(data, "output.txt", col.names=names(data), row.names=FALSE)
output <- read.table("output.txt", header=TRUE)
