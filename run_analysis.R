## Data Cleaning - Final Project Assignment
## =========================================
##    You should create one R script called run_analysis.R that does the following.

## 1- Set Working Directory 

setwd("C:/Users/Ahmed/Google Drive/Pers/Data Science/Data Cleaning/Final Project/UCI HAR Dataset")

## 2- Load into R
subject_test <- read.table("test/subject_test.txt" ,sep = "")
X_test       <- read.table("test/X_test.txt" ,sep = "")
Y_test       <- read.table("test/Y_test.txt" ,sep = "")

subject_train <- read.table("train/subject_train.txt" ,sep = "")
X_train       <- read.table("train/X_train.txt" ,sep = "")
Y_train       <- read.table("train/Y_train.txt" ,sep = "")

features_names <- read.table("features.txt" ,sep = "")
activity_labels <- read.table("activity_labels.txt" ,sep = "")
 
##    1.Merges the training and the test sets to create one data set.

OneSet <- rbind(cbind(subject_test,Y_test,X_test),cbind(subject_train,Y_train,X_train))

##    2.Extracts only the measurements on the mean and standard deviation for each measurement. 

MeanAndStdBooleanVector <- c(c(TRUE,TRUE),grepl('mean|std',features_names$V2))
MeanAndStdSet <- OneSet[,MeanAndStdBooleanVector]

##    3.Uses descriptive activity names to name the activities in the data set

MeanAndStdSet$V1.1 <- sapply(MeanAndStdSet$V1.1,function(x) activity_labels$V2[x])

##    4.Appropriately labels the data set with descriptive variable names. 

## Clean Names by removing special characters

features_names[,2] = gsub("-","",features_names[,2])
features_names[,2] = gsub(",","",features_names[,2])
features_names[,2] = gsub("[()]","",features_names[,2])

## Assign Column Names

colnames(MeanAndStdSet)[3:81] <- features_names[MeanAndStdBooleanVector[3:563],2]
colnames(MeanAndStdSet)[1] <- 'SubjectID'
colnames(MeanAndStdSet)[2] <- 'Activity'

##    5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## Finally produce the average of each variable grouped by each subject and activity.

data_summary <- MeanAndStdSet %>%
group_by(SubjectID,Activity) %>%
summarise_each(funs(mean))

## Output the Clean Data Set to a file tidy_data.txt
write.table(data_summary,"tidy_data.txt")
 