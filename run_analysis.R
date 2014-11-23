#*************************************************************************************
#*                                                                                   *
#*                             DOWNLOADING AND UNZIPPING                             *
#*                                                                                   *
#*************************************************************************************

#************ Downloading HAR data from the UCI Machine Learning web site ************

getDataUCIHAR <- function(...) {
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url=fileURL, destfile="dataUCIHAR.zip")
  
}

#******************* Unziping the downloaded zipped folder and files *****************

unZipHARDataUCI <- function(...) {
  unzip("dataUCIHAR.zip", files = NULL, list = FALSE, 
        overwrite = TRUE, junkpaths = FALSE, 
        exdir = "C:/Users/Christine/Documents/data/DataHARFromUCI", 
        unzip = "internal", setTimes = TRUE)
}  

#*************************************************************************************
#*                                                                                   *
#*               MERGING DATA SETS FILES AND SUBSETTING OF MEASURES                  *
#*                                                                                   *
#*  1. Merges the training and the test sets to create one data set.                 *
#*  2. Extracts only the measurements on the mean and the standard deviation for     *
#*     each measurement                                                              *
#*  3. Uses descriptive activity names to name the activities in the data set        *                                                 *
#*                                                                                   *
#*************************************************************************************                                                                  *

moveAndMergeFiles <- function(...) {
  targetdir <- c("~/data/GetAndCleanDataCourseProject")
  
  files <- c("subject_test.txt", "y_test.txt", "X_test.txt", "subject_train.txt",
             "y_train.txt", "X_train.txt", "features.txt", "activity_labels.txt") 
  
  file.create(files[1], files[2], files[3], files[4], files[5], files[6], files[7], 
              files[8]) 
  
  origintestdir <- c("~/data/DataHARFromUCI/UCI HAR Dataset/test")
  testfilestocopy <- c("subject_test.txt", "y_test.txt", "X_test.txt")
  setwd("~/data/DataHARFromUCI/UCI HAR Dataset/test")
  file.copy(from=testfilestocopy, to=targetdir, overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
  
  origintraindir <- c("~/data/DataHARFromUCI/UCI HAR Dataset/train")
  trainfilestocopy <- c("subject_train.txt", "y_train.txt", "X_train.txt")
  setwd("~/data/DataHARFromUCI/UCI HAR Dataset/train")
  file.copy(from=trainfilestocopy, to=targetdir, overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
  
  origincommondir <- c("~/data/DataHARFromUCI/UCI HAR Dataset")
  commonfilestocopy <- c("features.txt", "activity_labels.txt")
  setwd("~/data/DataHARFromUCI/UCI HAR Dataset")
  file.copy(from=commonfilestocopy, to=targetdir, overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
  
  setwd("~/data/GetAndCleanDataCourseProject")
  
  testVolunteers <- read.table("subject_test.txt")
  testActivities <- read.table("y_test.txt")
  testFeatures <- read.table("X_test.txt") 
  rawPerVolunteerAndActivity <- cbind(testVolunteers, testActivities)
  rawTest <- cbind(rawPerVolunteerAndActivity,testFeatures)
  
  trainVolunteers <- read.table("subject_train.txt")
  trainActivities <- read.table("y_train.txt")
  trainFeatures <- read.table("X_train.txt") 
  rawPerVolunteerAndActivity2 <- cbind(trainVolunteers, trainActivities)
  rawTrain <- cbind(rawPerVolunteerAndActivity2,trainFeatures)
  
  rawDataSetComplete <- rbind(rawTest, rawTrain)
  colnames(rawDataSetComplete) <- c("VolunteerID", "Activity", 1:561)
  
  rawDataSetComplete$Activity[rawDataSetComplete$Activity == "1"] <- "WALKING"
  rawDataSetComplete$Activity[rawDataSetComplete$Activity == "2"] <- "WALKING_UPSTAIRS"
  rawDataSetComplete$Activity[rawDataSetComplete$Activity == "3"] <- "WALKING_DOWNSTAIRS"
  rawDataSetComplete$Activity[rawDataSetComplete$Activity == "4"] <- "SITTING"
  rawDataSetComplete$Activity[rawDataSetComplete$Activity == "5"] <- "STANDING"
  rawDataSetComplete$Activity[rawDataSetComplete$Activity == "6"] <- "LAYING"
  
  features <- read.table("features.txt")
  myFeat <- c(features)
  meanColNber <-  grep("mean\\(\\)", myFeat$V2)
  stdColNber <-  grep("std\\(\\)", myFeat$V2) 
  colNber <- c(meanColNber, stdColNber)
  
  ordertoapply <- c(1:561)
  orderedColNber <- colNber[order(match(colNber,ordertoapply))]
  newOrdercolNber <- orderedColNber + 2
  
  library(dplyr)
  mytbldf <- tbl_df(rawDataSetComplete)
  mySubTabl <- select(mytbldf, VolunteerID, Activity,newOrdercolNber)
  
  write.table(mySubTabl, file="mergedHARDataSet.txt", append = FALSE, 
              quote = TRUE, sep = " ", eol = "\n", na = "NA", dec = ".", 
              row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")
  
  file.remove(testfilestocopy)
  file.remove(trainfilestocopy)
  file.remove(commonfilestocopy)  
}

#*************************************************************************************
#*                                                                                   *
#*                LABELLING DATA SET WITH DESCRIPTIVE VARIABLE NAMES                 *
#*                                                                                   *
#*  4.  Appropriately labels the data set with descriptive variable names.           *                                                                       *
#*                                                                                   *
#*      The result is the final complete data set (saved as "dataSetStep4.txt") with * 
#*      the HAR UCI measures corresponding to the mean() and std() for:              *
#*              -
#*
#************************************************************************************* 

dataFeaturesLabels <- function(...) {
  
  library(dplyr)
  mergeDataFile <- read.table("mergedHARDataSet.txt", header=TRUE) 
  myTblDf <- tbl_df(mergeDataFile) 
  
  mySubTabl <- select(myTblDf, VolunteerID, Activity, Body_Acc_Mean_X = X1, 
                      Body_Acc_Mean_Y = X2, Body_Acc_Mean_Z = X3, 
                      Body_Acc_Sd_X = X4, Body_Acc_Sd_Y = X5, 
                      Body_Acc_Sd_Z = X6, Gravity_Acc_Mean_X = X41, 
                      Gravity_Acc_Mean_Y = X42, Gravity_Acc_Mean_Z = X43, 
                      Gravity_Acc_Sd_X = X44, Gravity_Acc_Sd_Y = X45, 
                      Gravity_Acc_Sd_Z = X46, Body_AccJerk_Mean_X = X81, 
                      Body_AccJerk_Mean_Y = X82, Body_AccJerk_Mean_Z = X83, 
                      Body_AccJerk_Sd_X = X84, Body_AccJerk_Sd_Y = X85, 
                      Body_AccJerk_Sd_Z = X86, Body_Gyro_Mean_X = X121, 
                      Body_Gyro_Mean_Y = X122, Body_Gyro_Mean_Z = X123, 
                      Body_Gyro_Sd_X = X124, Body_Gyro_Sd_Y = X125, 
                      Body_Gyro_Sd_Z = X126, Body_GyroJerk_Mean_X = X161, 
                      Body_GyroJerk_Mean_Y = X162, Body_GyroJerk_Mean_Z = X163, 
                      Body_GyroJerk_Sd_X = X164, Body_GyroJerk_Sd_Y = X165, 
                      Body_GyroJerk_Sd_Z = X166, Body_Acc_Mag_Mean = X201, Body_Acc_Mag_Sd = X202, 
                      Gravity_Acc_Mag_Mean = X214, Gravity_Acc_Mag_Sd = X215, 
                      Body_AccJerk_Mag_Mean = X227,Body_AccJerk_Mag_Sd = X228, 
                      Body_Gyro_Mag_Mean = X240, Body_Gyro_Mag_Sd = X241, 
                      Body_GyroJerk_Mag_Mean = X253, Body_GyroJerk_Mag_Sd = X254, 
                      FFTBody_Acc_Mean_X = X266, FFTBody_Acc_Mean_Y = X267, 
                      FFTBody_Acc_Mean_Z = X268, FFTBody_Acc_Sd_X = X269, FFTBody_Acc_Sd_Y = X270, 
                      FFTBody_Acc_Sd_Z = X271, FFTBody_AccJerk_Mean_X = X345, 
                      FFTBody_AccJerk_Mean_Y = X346, FFTBody_AccJerk_Mean_Z = X347, 
                      FFTBody_AccJerk_Sd_X = X348, FFTBody_AccJerk_Sd_Y = X349, 
                      FFTBody_AccJerk_Sd_Z = X350, FFTBody_Gyro_Mean_X = X424, 
                      FFTBody_Gyro_Mean_Y = X425, FFTBody_Gyro_Mean_Z = X426, 
                      FFTBody_Gyro_Sd_X = X427, FFTBody_Gyro_Sd_Y = X428, 
                      FFTBody_Gyro_Sd_Z = X429, FFTBody_Acc_Mag_Mean = X503, 
                      FFTBody_Acc_Mag_Sd = X504, FFTBodyBody_AccJerk_Mag_Mean = X516, 
                      FFTBodyBody_AccJerk_Mag_Sd = X517, 
                      FFTBodyBody_Gyro_Mag_Mean = X529, 
                      FFTBodyBody_Gyro_Mag_Sd = X530, 
                      FFTBodyBody_GyroJerk_Mag_Mean = X542, 
                      FFTBodyBody_GyroJerk_Mag_Sd = X543)
  
  write.table(mySubTabl, file="dataSetStep4.txt", append = FALSE, 
              quote = TRUE, sep = " ", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "") 
} 

#*************************************************************************************
#*                                                                                   *
#*                  TIDY DATA SET WITH THE AVERAGE OF EACH VARIABLE                  *
#*                      PER / GROUPED BY ACTIVITY AND SUBJECT                        *
#*                                                                                   *
#*  5.  From the data set in step 4, creates a second, independent tidy data set     * 
#       with the average of each variable for each activity and each subject.        *
#*                                                                                   *
#************************************************************************************* 

getTidyData <- function(...) {
  library(dplyr)
  subDataFile <- read.table("dataSetStep4.txt", header=TRUE) 
  myData <- tbl_df(subDataFile) 

# First of all, we have to group by
#    => group_by(Activity)
#    => group_by(VolunteerID)

  by_ActivityAndVolunteer <- myData %>%
                                group_by(Activity) %>%
                                group_by(VolunteerID) 


# Summarize the data set group VolunteerID for each activity, in order to obtain a tidy data set 
# independent from the previous one (refer to the "dataSeStep4.txt") with the creation of the new 
# required variables, i.e the averages of the initial measures mean() and std() 

  subData <- subset(by_ActivityAndVolunteer, Activity == "STANDING", select=
                      VolunteerID:FFTBodyBody_GyroJerk_Mag_Sd)

  ActOneVol_sum <- summarize(subData, 
                          unique = n_distinct(VolunteerID),
                          avgBody_Acc_Mean_X = mean(Body_Acc_Mean_X), 
                          avgBody_Acc_Mean_Y = mean(Body_Acc_Mean_Y), 
                          avgBody_Acc_Mean_Z = mean(Body_Acc_Mean_Z), 
                          avgBody_Acc_Sd_X = mean(Body_Acc_Sd_X), 
                          avgBody_Acc_Sd_Y = mean(Body_Acc_Sd_Y),
                          avgBody_Acc_Sd_Z = mean(Body_Acc_Sd_Z),
                          avgGravity_Acc_Mean_X = mean(Gravity_Acc_Mean_X),
                          avgGravity_Acc_Mean_Y = mean(Gravity_Acc_Mean_Y), 
                          avgGravity_Acc_Mean_Z = mean(Gravity_Acc_Mean_Z), 
                          avgGravity_Acc_Sd_X = mean(Gravity_Acc_Sd_X), 
                          avgGravity_Acc_Sd_Y = mean(Gravity_Acc_Sd_Y),
                          avgGravity_Acc_Sd_Z = mean(Gravity_Acc_Sd_Z), 
                          avgBody_AccJerk_Mean_X = mean(Body_AccJerk_Mean_X), 
                          avgBody_AccJerk_Mean_Y = mean(Body_AccJerk_Mean_Y), 
                          avgBody_AccJerk_Mean_Z = mean(Body_AccJerk_Mean_Z), 
                          avgBody_AccJerk_Sd_X =  mean(Body_AccJerk_Sd_X),
                          avgBody_AccJerk_Sd_Y = mean(Body_AccJerk_Sd_Y),
                          avgBody_AccJerk_Sd_Z = mean(Body_AccJerk_Sd_Z),
                          avgBody_Gyro_Mean_X = mean(Body_Gyro_Mean_X),
                          avgBody_Gyro_Mean_Y = mean(Body_Gyro_Mean_Y), 
                          avgBody_Gyro_Mean_Z = mean(Body_Gyro_Mean_Z), 
                          avgBody_Gyro_Sd_X = mean(Body_Gyro_Sd_X), 
                          avgBody_Gyro_Sd_Y = mean(Body_Gyro_Sd_Y), 
                          avgBody_Gyro_Sd_Z = mean(Body_Gyro_Sd_Z),
                          avgBody_GyroJerk_Mean_X = mean(Body_GyroJerk_Mean_X), 
                          avgBody_GyroJerk_Mean_Y = mean(Body_GyroJerk_Mean_Y), 
                          avgBody_GyroJerk_Mean_Z = mean(Body_GyroJerk_Mean_Z),
                          avgBody_GyroJerk_Sd_X = mean(Body_GyroJerk_Sd_X), 
                          avgBody_GyroJerk_Sd_Y = mean(Body_GyroJerk_Sd_Y),
                          avgBody_GyroJerk_Sd_Z = mean(Body_GyroJerk_Sd_Z),
                          avgBody_Acc_Mag_Mean = mean(Body_Acc_Mag_Mean), 
                          avgBody_Acc_Mag_Sd =  mean(Body_Acc_Mag_Sd), 
                          avgGravity_Acc_Mag_Mean = mean(Gravity_Acc_Mag_Mean), 
                          avgGravity_Acc_Mag_Sd = mean(Gravity_Acc_Mag_Sd),
                          avgBody_AccJerk_Mag_Mean =  mean(Body_AccJerk_Mag_Mean),
                          avgBody_AccJerk_Mag_Sd = mean(Body_AccJerk_Mag_Sd),
                          avgBody_Gyro_Mag_Mean = mean(Body_Gyro_Mag_Mean),
                          avgBody_Gyro_Mag_Sd = mean(Body_Gyro_Mag_Sd), 
                          avgBody_GyroJerk_Mag_Mean = mean(Body_GyroJerk_Mag_Mean),
                          avgBody_GyroJerk_Mag_Sd = mean(Body_GyroJerk_Mag_Sd),
                          avgFFTBody_Acc_Mean_X = mean(FFTBody_Acc_Mean_X),
                          avgFFTBody_Acc_Mean_Y = mean(FFTBody_Acc_Mean_Y),
                          avgFFTBody_Acc_Mean_Z = mean(FFTBody_Acc_Mean_Z),
                          avgFFTBody_Acc_Sd_X = mean(FFTBody_Acc_Sd_X), 
                          avgFFTBody_Acc_Sd_Y = mean(FFTBody_Acc_Sd_Y), 
                          avgFFTBody_Acc_Sd_Z = mean(FFTBody_Acc_Sd_Z),
                          avgFFTBody_AccJerk_Mean_X = mean(FFTBody_AccJerk_Mean_X),
                          avgFFTBody_AccJerk_Mean_Y = mean(FFTBody_AccJerk_Mean_Y),
                          avgFFTBody_AccJerk_Mean_Z = mean(FFTBody_AccJerk_Mean_Z),
                          avgFFTBody_AccJerk_Sd_X = mean(FFTBody_AccJerk_Sd_X),
                          avgFFTBody_AccJerk_Sd_Y = mean(FFTBody_AccJerk_Sd_Y),
                          avgFFTBody_AccJerk_Sd_Z = mean(FFTBody_AccJerk_Sd_Z),
                          avgFFTBody_Gyro_Mean_X = mean(FFTBody_Gyro_Mean_X), 
                          avgFFTBody_Gyro_Mean_Y = mean(FFTBody_Gyro_Mean_Y), 
                          avgFFTBody_Gyro_Mean_Z = mean(FFTBody_Gyro_Mean_Z),
                          avgFFTBody_Gyro_Sd_X = mean(FFTBody_Gyro_Sd_X),
                          avgFFTBody_Gyro_Sd_Y = mean(FFTBody_Gyro_Sd_Y),
                          avgFFTBody_Gyro_Sd_Z = mean(FFTBody_Gyro_Sd_Z),
                          avgFFTBody_Acc_Mag_Mean = mean(FFTBody_Acc_Mag_Mean),
                          avgFFTBody_Acc_Mag_Sd = mean(FFTBody_Acc_Mag_Sd), 
                          avgFFTBodyBody_AccJerk_Mag_Mean = mean(FFTBodyBody_AccJerk_Mag_Mean),
                          avgFFTBodyBody_AccJerk_Mag_Sd =  mean(FFTBodyBody_AccJerk_Mag_Sd),
                          avgFFTBodyBody_Gyro_Mag_Mean = mean(FFTBodyBody_Gyro_Mag_Mean),
                          avgFFTBodyBody_Gyro_Mag_Sd = mean(FFTBodyBody_Gyro_Mag_Sd),
                          avgFFTBodyBody_GyroJerk_Mag_Mean = mean(FFTBodyBody_GyroJerk_Mag_Mean),
                          avgFFTBodyBody_GyroJerk_Mag_Sd = mean(FFTBodyBody_GyroJerk_Mag_Sd)) 

  subData <- subset(by_ActivityAndVolunteer, Activity == "WALKING", select=
                    VolunteerID:FFTBodyBody_GyroJerk_Mag_Sd)

  ActTwoVol_sum <- summarize(subData, 
                           unique = n_distinct(VolunteerID),
                           avgBody_Acc_Mean_X = mean(Body_Acc_Mean_X), 
                           avgBody_Acc_Mean_Y = mean(Body_Acc_Mean_Y), 
                           avgBody_Acc_Mean_Z = mean(Body_Acc_Mean_Z), 
                           avgBody_Acc_Sd_X = mean(Body_Acc_Sd_X), 
                           avgBody_Acc_Sd_Y = mean(Body_Acc_Sd_Y),
                           avgBody_Acc_Sd_Z = mean(Body_Acc_Sd_Z),
                           avgGravity_Acc_Mean_X = mean(Gravity_Acc_Mean_X),
                           avgGravity_Acc_Mean_Y = mean(Gravity_Acc_Mean_Y), 
                           avgGravity_Acc_Mean_Z = mean(Gravity_Acc_Mean_Z), 
                           avgGravity_Acc_Sd_X = mean(Gravity_Acc_Sd_X), 
                           avgGravity_Acc_Sd_Y = mean(Gravity_Acc_Sd_Y),
                           avgGravity_Acc_Sd_Z = mean(Gravity_Acc_Sd_Z), 
                           avgBody_AccJerk_Mean_X = mean(Body_AccJerk_Mean_X), 
                           avgBody_AccJerk_Mean_Y = mean(Body_AccJerk_Mean_Y), 
                           avgBody_AccJerk_Mean_Z = mean(Body_AccJerk_Mean_Z), 
                           avgBody_AccJerk_Sd_X =  mean(Body_AccJerk_Sd_X),
                           avgBody_AccJerk_Sd_Y = mean(Body_AccJerk_Sd_Y),
                           avgBody_AccJerk_Sd_Z = mean(Body_AccJerk_Sd_Z),
                           avgBody_Gyro_Mean_X = mean(Body_Gyro_Mean_X),
                           avgBody_Gyro_Mean_Y = mean(Body_Gyro_Mean_Y), 
                           avgBody_Gyro_Mean_Z = mean(Body_Gyro_Mean_Z), 
                           avgBody_Gyro_Sd_X = mean(Body_Gyro_Sd_X), 
                           avgBody_Gyro_Sd_Y = mean(Body_Gyro_Sd_Y), 
                           avgBody_Gyro_Sd_Z = mean(Body_Gyro_Sd_Z),
                           avgBody_GyroJerk_Mean_X = mean(Body_GyroJerk_Mean_X), 
                           avgBody_GyroJerk_Mean_Y = mean(Body_GyroJerk_Mean_Y), 
                           avgBody_GyroJerk_Mean_Z = mean(Body_GyroJerk_Mean_Z),
                           avgBody_GyroJerk_Sd_X = mean(Body_GyroJerk_Sd_X), 
                           avgBody_GyroJerk_Sd_Y = mean(Body_GyroJerk_Sd_Y),
                           avgBody_GyroJerk_Sd_Z = mean(Body_GyroJerk_Sd_Z),
                           avgBody_Acc_Mag_Mean = mean(Body_Acc_Mag_Mean), 
                           avgBody_Acc_Mag_Sd =  mean(Body_Acc_Mag_Sd), 
                           avgGravity_Acc_Mag_Mean = mean(Gravity_Acc_Mag_Mean), 
                           avgGravity_Acc_Mag_Sd = mean(Gravity_Acc_Mag_Sd),
                           avgBody_AccJerk_Mag_Mean =  mean(Body_AccJerk_Mag_Mean),
                           avgBody_AccJerk_Mag_Sd = mean(Body_AccJerk_Mag_Sd),
                           avgBody_Gyro_Mag_Mean = mean(Body_Gyro_Mag_Mean),
                           avgBody_Gyro_Mag_Sd = mean(Body_Gyro_Mag_Sd), 
                           avgBody_GyroJerk_Mag_Mean = mean(Body_GyroJerk_Mag_Mean),
                           avgBody_GyroJerk_Mag_Sd = mean(Body_GyroJerk_Mag_Sd),
                           avgFFTBody_Acc_Mean_X = mean(FFTBody_Acc_Mean_X),
                           avgFFTBody_Acc_Mean_Y = mean(FFTBody_Acc_Mean_Y),
                           avgFFTBody_Acc_Mean_Z = mean(FFTBody_Acc_Mean_Z),
                           avgFFTBody_Acc_Sd_X = mean(FFTBody_Acc_Sd_X), 
                           avgFFTBody_Acc_Sd_Y = mean(FFTBody_Acc_Sd_Y), 
                           avgFFTBody_Acc_Sd_Z = mean(FFTBody_Acc_Sd_Z),
                           avgFFTBody_AccJerk_Mean_X = mean(FFTBody_AccJerk_Mean_X),
                           avgFFTBody_AccJerk_Mean_Y = mean(FFTBody_AccJerk_Mean_Y),
                           avgFFTBody_AccJerk_Mean_Z = mean(FFTBody_AccJerk_Mean_Z),
                           avgFFTBody_AccJerk_Sd_X = mean(FFTBody_AccJerk_Sd_X),
                           avgFFTBody_AccJerk_Sd_Y = mean(FFTBody_AccJerk_Sd_Y),
                           avgFFTBody_AccJerk_Sd_Z = mean(FFTBody_AccJerk_Sd_Z),
                           avgFFTBody_Gyro_Mean_X = mean(FFTBody_Gyro_Mean_X), 
                           avgFFTBody_Gyro_Mean_Y = mean(FFTBody_Gyro_Mean_Y), 
                           avgFFTBody_Gyro_Mean_Z = mean(FFTBody_Gyro_Mean_Z),
                           avgFFTBody_Gyro_Sd_X = mean(FFTBody_Gyro_Sd_X),
                           avgFFTBody_Gyro_Sd_Y = mean(FFTBody_Gyro_Sd_Y),
                           avgFFTBody_Gyro_Sd_Z = mean(FFTBody_Gyro_Sd_Z),
                           avgFFTBody_Acc_Mag_Mean = mean(FFTBody_Acc_Mag_Mean),
                           avgFFTBody_Acc_Mag_Sd = mean(FFTBody_Acc_Mag_Sd), 
                           avgFFTBodyBody_AccJerk_Mag_Mean = mean(FFTBodyBody_AccJerk_Mag_Mean),
                           avgFFTBodyBody_AccJerk_Mag_Sd =  mean(FFTBodyBody_AccJerk_Mag_Sd),
                           avgFFTBodyBody_Gyro_Mag_Mean = mean(FFTBodyBody_Gyro_Mag_Mean),
                           avgFFTBodyBody_Gyro_Mag_Sd = mean(FFTBodyBody_Gyro_Mag_Sd),
                           avgFFTBodyBody_GyroJerk_Mag_Mean = mean(FFTBodyBody_GyroJerk_Mag_Mean),
                           avgFFTBodyBody_GyroJerk_Mag_Sd = mean(FFTBodyBody_GyroJerk_Mag_Sd)) 

subData <- subset(by_ActivityAndVolunteer, Activity == "WALKING_UPSTAIRS", select=
                    VolunteerID:FFTBodyBody_GyroJerk_Mag_Sd)

ActThreeVol_sum <- summarize(subData, 
                           unique = n_distinct(VolunteerID),
                           avgBody_Acc_Mean_X = mean(Body_Acc_Mean_X), 
                           avgBody_Acc_Mean_Y = mean(Body_Acc_Mean_Y), 
                           avgBody_Acc_Mean_Z = mean(Body_Acc_Mean_Z), 
                           avgBody_Acc_Sd_X = mean(Body_Acc_Sd_X), 
                           avgBody_Acc_Sd_Y = mean(Body_Acc_Sd_Y),
                           avgBody_Acc_Sd_Z = mean(Body_Acc_Sd_Z),
                           avgGravity_Acc_Mean_X = mean(Gravity_Acc_Mean_X),
                           avgGravity_Acc_Mean_Y = mean(Gravity_Acc_Mean_Y), 
                           avgGravity_Acc_Mean_Z = mean(Gravity_Acc_Mean_Z), 
                           avgGravity_Acc_Sd_X = mean(Gravity_Acc_Sd_X), 
                           avgGravity_Acc_Sd_Y = mean(Gravity_Acc_Sd_Y),
                           avgGravity_Acc_Sd_Z = mean(Gravity_Acc_Sd_Z), 
                           avgBody_AccJerk_Mean_X = mean(Body_AccJerk_Mean_X), 
                           avgBody_AccJerk_Mean_Y = mean(Body_AccJerk_Mean_Y), 
                           avgBody_AccJerk_Mean_Z = mean(Body_AccJerk_Mean_Z), 
                           avgBody_AccJerk_Sd_X =  mean(Body_AccJerk_Sd_X),
                           avgBody_AccJerk_Sd_Y = mean(Body_AccJerk_Sd_Y),
                           avgBody_AccJerk_Sd_Z = mean(Body_AccJerk_Sd_Z),
                           avgBody_Gyro_Mean_X = mean(Body_Gyro_Mean_X),
                           avgBody_Gyro_Mean_Y = mean(Body_Gyro_Mean_Y), 
                           avgBody_Gyro_Mean_Z = mean(Body_Gyro_Mean_Z), 
                           avgBody_Gyro_Sd_X = mean(Body_Gyro_Sd_X), 
                           avgBody_Gyro_Sd_Y = mean(Body_Gyro_Sd_Y), 
                           avgBody_Gyro_Sd_Z = mean(Body_Gyro_Sd_Z),
                           avgBody_GyroJerk_Mean_X = mean(Body_GyroJerk_Mean_X), 
                           avgBody_GyroJerk_Mean_Y = mean(Body_GyroJerk_Mean_Y), 
                           avgBody_GyroJerk_Mean_Z = mean(Body_GyroJerk_Mean_Z),
                           avgBody_GyroJerk_Sd_X = mean(Body_GyroJerk_Sd_X), 
                           avgBody_GyroJerk_Sd_Y = mean(Body_GyroJerk_Sd_Y),
                           avgBody_GyroJerk_Sd_Z = mean(Body_GyroJerk_Sd_Z),
                           avgBody_Acc_Mag_Mean = mean(Body_Acc_Mag_Mean), 
                           avgBody_Acc_Mag_Sd =  mean(Body_Acc_Mag_Sd), 
                           avgGravity_Acc_Mag_Mean = mean(Gravity_Acc_Mag_Mean), 
                           avgGravity_Acc_Mag_Sd = mean(Gravity_Acc_Mag_Sd),
                           avgBody_AccJerk_Mag_Mean =  mean(Body_AccJerk_Mag_Mean),
                           avgBody_AccJerk_Mag_Sd = mean(Body_AccJerk_Mag_Sd),
                           avgBody_Gyro_Mag_Mean = mean(Body_Gyro_Mag_Mean),
                           avgBody_Gyro_Mag_Sd = mean(Body_Gyro_Mag_Sd), 
                           avgBody_GyroJerk_Mag_Mean = mean(Body_GyroJerk_Mag_Mean),
                           avgBody_GyroJerk_Mag_Sd = mean(Body_GyroJerk_Mag_Sd),
                           avgFFTBody_Acc_Mean_X = mean(FFTBody_Acc_Mean_X),
                           avgFFTBody_Acc_Mean_Y = mean(FFTBody_Acc_Mean_Y),
                           avgFFTBody_Acc_Mean_Z = mean(FFTBody_Acc_Mean_Z),
                           avgFFTBody_Acc_Sd_X = mean(FFTBody_Acc_Sd_X), 
                           avgFFTBody_Acc_Sd_Y = mean(FFTBody_Acc_Sd_Y), 
                           avgFFTBody_Acc_Sd_Z = mean(FFTBody_Acc_Sd_Z),
                           avgFFTBody_AccJerk_Mean_X = mean(FFTBody_AccJerk_Mean_X),
                           avgFFTBody_AccJerk_Mean_Y = mean(FFTBody_AccJerk_Mean_Y),
                           avgFFTBody_AccJerk_Mean_Z = mean(FFTBody_AccJerk_Mean_Z),
                           avgFFTBody_AccJerk_Sd_X = mean(FFTBody_AccJerk_Sd_X),
                           avgFFTBody_AccJerk_Sd_Y = mean(FFTBody_AccJerk_Sd_Y),
                           avgFFTBody_AccJerk_Sd_Z = mean(FFTBody_AccJerk_Sd_Z),
                           avgFFTBody_Gyro_Mean_X = mean(FFTBody_Gyro_Mean_X), 
                           avgFFTBody_Gyro_Mean_Y = mean(FFTBody_Gyro_Mean_Y), 
                           avgFFTBody_Gyro_Mean_Z = mean(FFTBody_Gyro_Mean_Z),
                           avgFFTBody_Gyro_Sd_X = mean(FFTBody_Gyro_Sd_X),
                           avgFFTBody_Gyro_Sd_Y = mean(FFTBody_Gyro_Sd_Y),
                           avgFFTBody_Gyro_Sd_Z = mean(FFTBody_Gyro_Sd_Z),
                           avgFFTBody_Acc_Mag_Mean = mean(FFTBody_Acc_Mag_Mean),
                           avgFFTBody_Acc_Mag_Sd = mean(FFTBody_Acc_Mag_Sd), 
                           avgFFTBodyBody_AccJerk_Mag_Mean = mean(FFTBodyBody_AccJerk_Mag_Mean),
                           avgFFTBodyBody_AccJerk_Mag_Sd =  mean(FFTBodyBody_AccJerk_Mag_Sd),
                           avgFFTBodyBody_Gyro_Mag_Mean = mean(FFTBodyBody_Gyro_Mag_Mean),
                           avgFFTBodyBody_Gyro_Mag_Sd = mean(FFTBodyBody_Gyro_Mag_Sd),
                           avgFFTBodyBody_GyroJerk_Mag_Mean = mean(FFTBodyBody_GyroJerk_Mag_Mean),
                           avgFFTBodyBody_GyroJerk_Mag_Sd = mean(FFTBodyBody_GyroJerk_Mag_Sd)) 

subData <- subset(by_ActivityAndVolunteer, Activity == "WALKING_DOWNSTAIRS", select=
                    VolunteerID:FFTBodyBody_GyroJerk_Mag_Sd)

ActFourVol_sum <- summarize(subData, 
                             unique = n_distinct(VolunteerID),
                             avgBody_Acc_Mean_X = mean(Body_Acc_Mean_X), 
                             avgBody_Acc_Mean_Y = mean(Body_Acc_Mean_Y), 
                             avgBody_Acc_Mean_Z = mean(Body_Acc_Mean_Z), 
                             avgBody_Acc_Sd_X = mean(Body_Acc_Sd_X), 
                             avgBody_Acc_Sd_Y = mean(Body_Acc_Sd_Y),
                             avgBody_Acc_Sd_Z = mean(Body_Acc_Sd_Z),
                             avgGravity_Acc_Mean_X = mean(Gravity_Acc_Mean_X),
                             avgGravity_Acc_Mean_Y = mean(Gravity_Acc_Mean_Y), 
                             avgGravity_Acc_Mean_Z = mean(Gravity_Acc_Mean_Z), 
                             avgGravity_Acc_Sd_X = mean(Gravity_Acc_Sd_X), 
                             avgGravity_Acc_Sd_Y = mean(Gravity_Acc_Sd_Y),
                             avgGravity_Acc_Sd_Z = mean(Gravity_Acc_Sd_Z), 
                             avgBody_AccJerk_Mean_X = mean(Body_AccJerk_Mean_X), 
                             avgBody_AccJerk_Mean_Y = mean(Body_AccJerk_Mean_Y), 
                             avgBody_AccJerk_Mean_Z = mean(Body_AccJerk_Mean_Z), 
                             avgBody_AccJerk_Sd_X =  mean(Body_AccJerk_Sd_X),
                             avgBody_AccJerk_Sd_Y = mean(Body_AccJerk_Sd_Y),
                             avgBody_AccJerk_Sd_Z = mean(Body_AccJerk_Sd_Z),
                             avgBody_Gyro_Mean_X = mean(Body_Gyro_Mean_X),
                             avgBody_Gyro_Mean_Y = mean(Body_Gyro_Mean_Y), 
                             avgBody_Gyro_Mean_Z = mean(Body_Gyro_Mean_Z), 
                             avgBody_Gyro_Sd_X = mean(Body_Gyro_Sd_X), 
                             avgBody_Gyro_Sd_Y = mean(Body_Gyro_Sd_Y), 
                             avgBody_Gyro_Sd_Z = mean(Body_Gyro_Sd_Z),
                             avgBody_GyroJerk_Mean_X = mean(Body_GyroJerk_Mean_X), 
                             avgBody_GyroJerk_Mean_Y = mean(Body_GyroJerk_Mean_Y), 
                             avgBody_GyroJerk_Mean_Z = mean(Body_GyroJerk_Mean_Z),
                             avgBody_GyroJerk_Sd_X = mean(Body_GyroJerk_Sd_X), 
                             avgBody_GyroJerk_Sd_Y = mean(Body_GyroJerk_Sd_Y),
                             avgBody_GyroJerk_Sd_Z = mean(Body_GyroJerk_Sd_Z),
                             avgBody_Acc_Mag_Mean = mean(Body_Acc_Mag_Mean), 
                             avgBody_Acc_Mag_Sd =  mean(Body_Acc_Mag_Sd), 
                             avgGravity_Acc_Mag_Mean = mean(Gravity_Acc_Mag_Mean), 
                             avgGravity_Acc_Mag_Sd = mean(Gravity_Acc_Mag_Sd),
                             avgBody_AccJerk_Mag_Mean =  mean(Body_AccJerk_Mag_Mean),
                             avgBody_AccJerk_Mag_Sd = mean(Body_AccJerk_Mag_Sd),
                             avgBody_Gyro_Mag_Mean = mean(Body_Gyro_Mag_Mean),
                             avgBody_Gyro_Mag_Sd = mean(Body_Gyro_Mag_Sd), 
                             avgBody_GyroJerk_Mag_Mean = mean(Body_GyroJerk_Mag_Mean),
                             avgBody_GyroJerk_Mag_Sd = mean(Body_GyroJerk_Mag_Sd),
                             avgFFTBody_Acc_Mean_X = mean(FFTBody_Acc_Mean_X),
                             avgFFTBody_Acc_Mean_Y = mean(FFTBody_Acc_Mean_Y),
                             avgFFTBody_Acc_Mean_Z = mean(FFTBody_Acc_Mean_Z),
                             avgFFTBody_Acc_Sd_X = mean(FFTBody_Acc_Sd_X), 
                             avgFFTBody_Acc_Sd_Y = mean(FFTBody_Acc_Sd_Y), 
                             avgFFTBody_Acc_Sd_Z = mean(FFTBody_Acc_Sd_Z),
                             avgFFTBody_AccJerk_Mean_X = mean(FFTBody_AccJerk_Mean_X),
                             avgFFTBody_AccJerk_Mean_Y = mean(FFTBody_AccJerk_Mean_Y),
                             avgFFTBody_AccJerk_Mean_Z = mean(FFTBody_AccJerk_Mean_Z),
                             avgFFTBody_AccJerk_Sd_X = mean(FFTBody_AccJerk_Sd_X),
                             avgFFTBody_AccJerk_Sd_Y = mean(FFTBody_AccJerk_Sd_Y),
                             avgFFTBody_AccJerk_Sd_Z = mean(FFTBody_AccJerk_Sd_Z),
                             avgFFTBody_Gyro_Mean_X = mean(FFTBody_Gyro_Mean_X), 
                             avgFFTBody_Gyro_Mean_Y = mean(FFTBody_Gyro_Mean_Y), 
                             avgFFTBody_Gyro_Mean_Z = mean(FFTBody_Gyro_Mean_Z),
                             avgFFTBody_Gyro_Sd_X = mean(FFTBody_Gyro_Sd_X),
                             avgFFTBody_Gyro_Sd_Y = mean(FFTBody_Gyro_Sd_Y),
                             avgFFTBody_Gyro_Sd_Z = mean(FFTBody_Gyro_Sd_Z),
                             avgFFTBody_Acc_Mag_Mean = mean(FFTBody_Acc_Mag_Mean),
                             avgFFTBody_Acc_Mag_Sd = mean(FFTBody_Acc_Mag_Sd), 
                             avgFFTBodyBody_AccJerk_Mag_Mean = mean(FFTBodyBody_AccJerk_Mag_Mean),
                             avgFFTBodyBody_AccJerk_Mag_Sd =  mean(FFTBodyBody_AccJerk_Mag_Sd),
                             avgFFTBodyBody_Gyro_Mag_Mean = mean(FFTBodyBody_Gyro_Mag_Mean),
                             avgFFTBodyBody_Gyro_Mag_Sd = mean(FFTBodyBody_Gyro_Mag_Sd),
                             avgFFTBodyBody_GyroJerk_Mag_Mean = mean(FFTBodyBody_GyroJerk_Mag_Mean),
                             avgFFTBodyBody_GyroJerk_Mag_Sd = mean(FFTBodyBody_GyroJerk_Mag_Sd)) 

subData <- subset(by_ActivityAndVolunteer, Activity == "LAYING", select=
                    VolunteerID:FFTBodyBody_GyroJerk_Mag_Sd)

ActFiveVol_sum <- summarize(subData, 
                            unique = n_distinct(VolunteerID),
                            avgBody_Acc_Mean_X = mean(Body_Acc_Mean_X), 
                            avgBody_Acc_Mean_Y = mean(Body_Acc_Mean_Y), 
                            avgBody_Acc_Mean_Z = mean(Body_Acc_Mean_Z), 
                            avgBody_Acc_Sd_X = mean(Body_Acc_Sd_X), 
                            avgBody_Acc_Sd_Y = mean(Body_Acc_Sd_Y),
                            avgBody_Acc_Sd_Z = mean(Body_Acc_Sd_Z),
                            avgGravity_Acc_Mean_X = mean(Gravity_Acc_Mean_X),
                            avgGravity_Acc_Mean_Y = mean(Gravity_Acc_Mean_Y), 
                            avgGravity_Acc_Mean_Z = mean(Gravity_Acc_Mean_Z), 
                            avgGravity_Acc_Sd_X = mean(Gravity_Acc_Sd_X), 
                            avgGravity_Acc_Sd_Y = mean(Gravity_Acc_Sd_Y),
                            avgGravity_Acc_Sd_Z = mean(Gravity_Acc_Sd_Z), 
                            avgBody_AccJerk_Mean_X = mean(Body_AccJerk_Mean_X), 
                            avgBody_AccJerk_Mean_Y = mean(Body_AccJerk_Mean_Y), 
                            avgBody_AccJerk_Mean_Z = mean(Body_AccJerk_Mean_Z), 
                            avgBody_AccJerk_Sd_X =  mean(Body_AccJerk_Sd_X),
                            avgBody_AccJerk_Sd_Y = mean(Body_AccJerk_Sd_Y),
                            avgBody_AccJerk_Sd_Z = mean(Body_AccJerk_Sd_Z),
                            avgBody_Gyro_Mean_X = mean(Body_Gyro_Mean_X),
                            avgBody_Gyro_Mean_Y = mean(Body_Gyro_Mean_Y), 
                            avgBody_Gyro_Mean_Z = mean(Body_Gyro_Mean_Z), 
                            avgBody_Gyro_Sd_X = mean(Body_Gyro_Sd_X), 
                            avgBody_Gyro_Sd_Y = mean(Body_Gyro_Sd_Y), 
                            avgBody_Gyro_Sd_Z = mean(Body_Gyro_Sd_Z),
                            avgBody_GyroJerk_Mean_X = mean(Body_GyroJerk_Mean_X), 
                            avgBody_GyroJerk_Mean_Y = mean(Body_GyroJerk_Mean_Y), 
                            avgBody_GyroJerk_Mean_Z = mean(Body_GyroJerk_Mean_Z),
                            avgBody_GyroJerk_Sd_X = mean(Body_GyroJerk_Sd_X), 
                            avgBody_GyroJerk_Sd_Y = mean(Body_GyroJerk_Sd_Y),
                            avgBody_GyroJerk_Sd_Z = mean(Body_GyroJerk_Sd_Z),
                            avgBody_Acc_Mag_Mean = mean(Body_Acc_Mag_Mean), 
                            avgBody_Acc_Mag_Sd =  mean(Body_Acc_Mag_Sd), 
                            avgGravity_Acc_Mag_Mean = mean(Gravity_Acc_Mag_Mean), 
                            avgGravity_Acc_Mag_Sd = mean(Gravity_Acc_Mag_Sd),
                            avgBody_AccJerk_Mag_Mean =  mean(Body_AccJerk_Mag_Mean),
                            avgBody_AccJerk_Mag_Sd = mean(Body_AccJerk_Mag_Sd),
                            avgBody_Gyro_Mag_Mean = mean(Body_Gyro_Mag_Mean),
                            avgBody_Gyro_Mag_Sd = mean(Body_Gyro_Mag_Sd), 
                            avgBody_GyroJerk_Mag_Mean = mean(Body_GyroJerk_Mag_Mean),
                            avgBody_GyroJerk_Mag_Sd = mean(Body_GyroJerk_Mag_Sd),
                            avgFFTBody_Acc_Mean_X = mean(FFTBody_Acc_Mean_X),
                            avgFFTBody_Acc_Mean_Y = mean(FFTBody_Acc_Mean_Y),
                            avgFFTBody_Acc_Mean_Z = mean(FFTBody_Acc_Mean_Z),
                            avgFFTBody_Acc_Sd_X = mean(FFTBody_Acc_Sd_X), 
                            avgFFTBody_Acc_Sd_Y = mean(FFTBody_Acc_Sd_Y), 
                            avgFFTBody_Acc_Sd_Z = mean(FFTBody_Acc_Sd_Z),
                            avgFFTBody_AccJerk_Mean_X = mean(FFTBody_AccJerk_Mean_X),
                            avgFFTBody_AccJerk_Mean_Y = mean(FFTBody_AccJerk_Mean_Y),
                            avgFFTBody_AccJerk_Mean_Z = mean(FFTBody_AccJerk_Mean_Z),
                            avgFFTBody_AccJerk_Sd_X = mean(FFTBody_AccJerk_Sd_X),
                            avgFFTBody_AccJerk_Sd_Y = mean(FFTBody_AccJerk_Sd_Y),
                            avgFFTBody_AccJerk_Sd_Z = mean(FFTBody_AccJerk_Sd_Z),
                            avgFFTBody_Gyro_Mean_X = mean(FFTBody_Gyro_Mean_X), 
                            avgFFTBody_Gyro_Mean_Y = mean(FFTBody_Gyro_Mean_Y), 
                            avgFFTBody_Gyro_Mean_Z = mean(FFTBody_Gyro_Mean_Z),
                            avgFFTBody_Gyro_Sd_X = mean(FFTBody_Gyro_Sd_X),
                            avgFFTBody_Gyro_Sd_Y = mean(FFTBody_Gyro_Sd_Y),
                            avgFFTBody_Gyro_Sd_Z = mean(FFTBody_Gyro_Sd_Z),
                            avgFFTBody_Acc_Mag_Mean = mean(FFTBody_Acc_Mag_Mean),
                            avgFFTBody_Acc_Mag_Sd = mean(FFTBody_Acc_Mag_Sd), 
                            avgFFTBodyBody_AccJerk_Mag_Mean = mean(FFTBodyBody_AccJerk_Mag_Mean),
                            avgFFTBodyBody_AccJerk_Mag_Sd =  mean(FFTBodyBody_AccJerk_Mag_Sd),
                            avgFFTBodyBody_Gyro_Mag_Mean = mean(FFTBodyBody_Gyro_Mag_Mean),
                            avgFFTBodyBody_Gyro_Mag_Sd = mean(FFTBodyBody_Gyro_Mag_Sd),
                            avgFFTBodyBody_GyroJerk_Mag_Mean = mean(FFTBodyBody_GyroJerk_Mag_Mean),
                            avgFFTBodyBody_GyroJerk_Mag_Sd = mean(FFTBodyBody_GyroJerk_Mag_Sd)) 

subData <- subset(by_ActivityAndVolunteer, Activity == "SITTING", select=
                    VolunteerID:FFTBodyBody_GyroJerk_Mag_Sd)

ActSixVol_sum <- summarize(subData, 
                            unique = n_distinct(VolunteerID),
                            avgBody_Acc_Mean_X = mean(Body_Acc_Mean_X), 
                            avgBody_Acc_Mean_Y = mean(Body_Acc_Mean_Y), 
                            avgBody_Acc_Mean_Z = mean(Body_Acc_Mean_Z), 
                            avgBody_Acc_Sd_X = mean(Body_Acc_Sd_X), 
                            avgBody_Acc_Sd_Y = mean(Body_Acc_Sd_Y),
                            avgBody_Acc_Sd_Z = mean(Body_Acc_Sd_Z),
                            avgGravity_Acc_Mean_X = mean(Gravity_Acc_Mean_X),
                            avgGravity_Acc_Mean_Y = mean(Gravity_Acc_Mean_Y), 
                            avgGravity_Acc_Mean_Z = mean(Gravity_Acc_Mean_Z), 
                            avgGravity_Acc_Sd_X = mean(Gravity_Acc_Sd_X), 
                            avgGravity_Acc_Sd_Y = mean(Gravity_Acc_Sd_Y),
                            avgGravity_Acc_Sd_Z = mean(Gravity_Acc_Sd_Z), 
                            avgBody_AccJerk_Mean_X = mean(Body_AccJerk_Mean_X), 
                            avgBody_AccJerk_Mean_Y = mean(Body_AccJerk_Mean_Y), 
                            avgBody_AccJerk_Mean_Z = mean(Body_AccJerk_Mean_Z), 
                            avgBody_AccJerk_Sd_X =  mean(Body_AccJerk_Sd_X),
                            avgBody_AccJerk_Sd_Y = mean(Body_AccJerk_Sd_Y),
                            avgBody_AccJerk_Sd_Z = mean(Body_AccJerk_Sd_Z),
                            avgBody_Gyro_Mean_X = mean(Body_Gyro_Mean_X),
                            avgBody_Gyro_Mean_Y = mean(Body_Gyro_Mean_Y), 
                            avgBody_Gyro_Mean_Z = mean(Body_Gyro_Mean_Z), 
                            avgBody_Gyro_Sd_X = mean(Body_Gyro_Sd_X), 
                            avgBody_Gyro_Sd_Y = mean(Body_Gyro_Sd_Y), 
                            avgBody_Gyro_Sd_Z = mean(Body_Gyro_Sd_Z),
                            avgBody_GyroJerk_Mean_X = mean(Body_GyroJerk_Mean_X), 
                            avgBody_GyroJerk_Mean_Y = mean(Body_GyroJerk_Mean_Y), 
                            avgBody_GyroJerk_Mean_Z = mean(Body_GyroJerk_Mean_Z),
                            avgBody_GyroJerk_Sd_X = mean(Body_GyroJerk_Sd_X), 
                            avgBody_GyroJerk_Sd_Y = mean(Body_GyroJerk_Sd_Y),
                            avgBody_GyroJerk_Sd_Z = mean(Body_GyroJerk_Sd_Z),
                            avgBody_Acc_Mag_Mean = mean(Body_Acc_Mag_Mean), 
                            avgBody_Acc_Mag_Sd =  mean(Body_Acc_Mag_Sd), 
                            avgGravity_Acc_Mag_Mean = mean(Gravity_Acc_Mag_Mean), 
                            avgGravity_Acc_Mag_Sd = mean(Gravity_Acc_Mag_Sd),
                            avgBody_AccJerk_Mag_Mean =  mean(Body_AccJerk_Mag_Mean),
                            avgBody_AccJerk_Mag_Sd = mean(Body_AccJerk_Mag_Sd),
                            avgBody_Gyro_Mag_Mean = mean(Body_Gyro_Mag_Mean),
                            avgBody_Gyro_Mag_Sd = mean(Body_Gyro_Mag_Sd), 
                            avgBody_GyroJerk_Mag_Mean = mean(Body_GyroJerk_Mag_Mean),
                            avgBody_GyroJerk_Mag_Sd = mean(Body_GyroJerk_Mag_Sd),
                            avgFFTBody_Acc_Mean_X = mean(FFTBody_Acc_Mean_X),
                            avgFFTBody_Acc_Mean_Y = mean(FFTBody_Acc_Mean_Y),
                            avgFFTBody_Acc_Mean_Z = mean(FFTBody_Acc_Mean_Z),
                            avgFFTBody_Acc_Sd_X = mean(FFTBody_Acc_Sd_X), 
                            avgFFTBody_Acc_Sd_Y = mean(FFTBody_Acc_Sd_Y), 
                            avgFFTBody_Acc_Sd_Z = mean(FFTBody_Acc_Sd_Z),
                            avgFFTBody_AccJerk_Mean_X = mean(FFTBody_AccJerk_Mean_X),
                            avgFFTBody_AccJerk_Mean_Y = mean(FFTBody_AccJerk_Mean_Y),
                            avgFFTBody_AccJerk_Mean_Z = mean(FFTBody_AccJerk_Mean_Z),
                            avgFFTBody_AccJerk_Sd_X = mean(FFTBody_AccJerk_Sd_X),
                            avgFFTBody_AccJerk_Sd_Y = mean(FFTBody_AccJerk_Sd_Y),
                            avgFFTBody_AccJerk_Sd_Z = mean(FFTBody_AccJerk_Sd_Z),
                            avgFFTBody_Gyro_Mean_X = mean(FFTBody_Gyro_Mean_X), 
                            avgFFTBody_Gyro_Mean_Y = mean(FFTBody_Gyro_Mean_Y), 
                            avgFFTBody_Gyro_Mean_Z = mean(FFTBody_Gyro_Mean_Z),
                            avgFFTBody_Gyro_Sd_X = mean(FFTBody_Gyro_Sd_X),
                            avgFFTBody_Gyro_Sd_Y = mean(FFTBody_Gyro_Sd_Y),
                            avgFFTBody_Gyro_Sd_Z = mean(FFTBody_Gyro_Sd_Z),
                            avgFFTBody_Acc_Mag_Mean = mean(FFTBody_Acc_Mag_Mean),
                            avgFFTBody_Acc_Mag_Sd = mean(FFTBody_Acc_Mag_Sd), 
                            avgFFTBodyBody_AccJerk_Mag_Mean = mean(FFTBodyBody_AccJerk_Mag_Mean),
                            avgFFTBodyBody_AccJerk_Mag_Sd =  mean(FFTBodyBody_AccJerk_Mag_Sd),
                            avgFFTBodyBody_Gyro_Mag_Mean = mean(FFTBodyBody_Gyro_Mag_Mean),
                            avgFFTBodyBody_Gyro_Mag_Sd = mean(FFTBodyBody_Gyro_Mag_Sd),
                            avgFFTBodyBody_GyroJerk_Mag_Mean = mean(FFTBodyBody_GyroJerk_Mag_Mean),
                            avgFFTBodyBody_GyroJerk_Mag_Sd = mean(FFTBodyBody_GyroJerk_Mag_Sd)) 

    combActOneVol_sum <- mutate(ActOneVol_sum, Activity = "STANDING")
    combActTwoVol_sum <- mutate(ActTwoVol_sum, Activity = "WALKING")
    combActThreeVol_sum <- mutate(ActThreeVol_sum, Activity = "WALKING_UPSTAIRS")
    combActFourVol_sum <- mutate(ActFourVol_sum, Activity = "WALKING_DOWNSTAIRS")
    combActFiveVol_sum <- mutate(ActFiveVol_sum, Activity = "LAYING")
    combActSixVol_sum <- mutate(ActSixVol_sum, Activity = "SITTING")

    combAllVol_sum <- rbind(combActOneVol_sum, combActTwoVol_sum, combActThreeVol_sum, combActFourVol_sum,
                            combActFiveVol_sum, combActSixVol_sum)

    finalData <- select(combAllVol_sum, Activity, VolunteerID, avgBody_Acc_Mean_X:avgFFTBodyBody_GyroJerk_Mag_Sd)

    write.table(finalData, file="tidyDataSetStep5.txt", append = FALSE, 
            quote = TRUE, sep = " ", eol = "\n", na = "NA", dec = ".", 
            row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

} 

run_analysis <- function(...) {
#  getDataUCIHAR()
#  unZipHARDataUCI()
  moveAndMergeFiles()
  dataFeaturesLabels()
  getTidyData()
}