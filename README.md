# coursera-assignment
# The code is for my file called run_analysis.R which sits in my local R Studio.  
# The code book for the variables is at the bottom.
library(dplyr)
if(!file.exists("/data")){dir.create("./data")}
dataset_url <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataset_url, "getdata-projectfiles-UCI HAR Dataset.zip")
unzip("getdata-projectfiles-UCI HAR Dataset.zip", exdir = "data")
setwd(paste(getwd(),"/data",sep=""))
setwd(paste(getwd(),"/UCI HAR Dataset",sep=""))
setwd(paste(getwd(),"/test",sep=""))
test <- read.table("X_test.txt", header = FALSE)
subject_test <- read.table("subject_test.txt", header = FALSE)
subject_test <-rename(subject_test, S1 = V1)
test <- cbind(subject_test, test)
y_test <- read.table("y_test.txt", header = FALSE)
y_test <- rename(y_test, A1 = V1)
test <- cbind(y_test, test)
select_test <- select(test, A1, S1, V1:V6, 
                      V41:V46, V81:V86, V121:V126, 
                      V161:V166, V201:V202, V214:V215, 
                      V227:V228, V240:V241, V253:V254, 
                      V266:V271, V345:V350, V424:V429, 
                      V503:V504, V516:V517, V529:V530, 
                      V542:V543)
setwd("../")
setwd(paste(getwd(),"/train",sep=""))
train <- read.table("X_train.txt", header = FALSE)
subject_train <- read.table("subject_train.txt", header = FALSE)
subject_train <-rename(subject_train, S1 = V1)
train <- cbind(subject_train, train)
y_train <- read.table("y_train.txt", header = FALSE)
y_train <- rename(y_train, A1 = V1)
train <- cbind(y_train, train)
select_train <- select(train, A1, S1, V1:V6, 
                       V41:V46, V81:V86, V121:V126, 
                       V161:V166, V201:V202, V214:V215, 
                       V227:V228, V240:V241, V253:V254, 
                       V266:V271, V345:V350, V424:V429, 
                       V503:V504, V516:V517, V529:V530, 
                       V542:V543)
bind_train_test<-rbind(select_test, select_train)
bind_train_test$A1 <- replace(bind_train_test$A1, bind_train_test$A1==1, 'walking')
bind_train_test$A1 <- replace(bind_train_test$A1, bind_train_test$A1==2, 'walking_upstairs')
bind_train_test$A1 <- replace(bind_train_test$A1, bind_train_test$A1==3, 'walking_downstairs')
bind_train_test$A1 <- replace(bind_train_test$A1, bind_train_test$A1==4, 'sitting')
bind_train_test$A1 <- replace(bind_train_test$A1, bind_train_test$A1==5, 'standing')
bind_train_test$A1 <- replace(bind_train_test$A1, bind_train_test$A1==6, 'laying')

bind_train_test <- rename(bind_train_test, activity = A1, subject = S1) 
bind_train_test <- rename(bind_train_test, 
      BodyAccMeanX = V1, 
      BodyAccMeanY = V2,
      BodyAccMeanZ = V3,
      BodyAccStdX =  V4,
      BodyAccStdY =  V5,
      BodyAccStdZ =  V6,
      GravityAccMeanX =  V41,
      GravityAccMeanY =  V42,
      GravityAccMeanZ =  V43,
      GravityAccStdX =  V44,
      GravityAccStdY =  V45,
      GravityAccStdZ =  V46,
      BodyAccJerkMeanX =  V81,
      BodyAccJerkMeanY =  V82,
      BodyAccJerkMeanZ = V83,
      BodyAccJerkStdX = V84,
      BodyAccJerkStdY = V85,
      BodyAccJerkStdZ = V86,
      BodyGyroMeanX = V121,
      BodyGyroMeanY =  V122,
      BodyGyroMeanZ  =  V123,
      BodyGyroStdX =  V124,
      BodyGyroStdY =  V125,
      BodyGyroStdZ = V126,
      BodyGyroJerkMeanX = V161,
      BodyGyroJerkMeanY =  V162,
      BodyGyroJerkMeanZ =  V163,
      BodyGyroJerkStdX =  V164,
      BodyGyroJerkStdY =  V165,
      BodyGyroJerkStdZ =  V166,
      BodyAccMagMean =  V201,
      BodyAccMagStd =  V202,
      GravityAccMagMean =  V214,
      GravityAccMagStd =  V215,
      BodyAccJerkMagMean =  V227,
      BodyAccJerkMagStd =  V228,
      BodyGyroMagMean =  V240,
      BodyGyroMagStd =  V241,
      BodyGyroJerkMagMean =  V253,
      BodyGyroJerkMagStd =  V254,
      BodyAccMeanX2 =  V266,
      BodyAccMean2Y = V267,
      BodyAccMeanZ2 = V268,
      BodyAccStdX2 = V269,
      BodyAccStdY2 = V270,
      BodyAccStdZ2 = V271,
      BodyAccJerkMeanX2 = V345,
      BodyAccJerkMeanY2 = V346,
      BodyAccJerkMeanZ2 = V347,
      BodyAccJerkStdX2 = V348,
      BodyAccJerkStdY2 =  V349,
      BodyAccJerkStdZ2 = V350,
      BodyGyroMeanX2 = V424,
      BodyGyroMeanY2 = V425,
      BodyGyroMeanZ2 = V426,
      BodyGyroStdX2 = V427,
      BodyGyroStdY2 = V428,
      BodyGyroStdZ2 = V429,
      BodyAccMagMean2 = V503, 
      BodyAccMagStd2 =  V504,
      BodyBodyAccJerkMagMean = V516, 
      BodyBodyAccJerkMagStd = V517,
      BodyBodyGyroMagMean =  V529,
      BodyBodyGyroMagStd =  V530,
      BodyBodyGyroJerkMagMean = V542,
      BodyBodyGyroJerkMagStd = V543)

SecondDataSet <- aggregate(bind_train_test[, 3:68], 
                            list(bind_train_test$activity, 
                                 bind_train_test$subject), 
                            mean)
                            
                            
SecondDataSet <- rename(SecondDataSet, Activity = Group.1, Subject = Group.2)
write.table ( SecondDataSet, "seconddataset.txt", row.names = FALSE)

#Codebook
  #activity = physical activity 
  #subject = number assigned to the person in the study
  #BodyAccMeanX = Mean of BodyAccX measure
  #BodyAccMeanY = Mean of BodyAccY measure
      #BodyAccMeanZ = Mean of BodyAccZ measure
      #BodyAccStdX =  Std of BodyAccX measure
      #BodyAccStdY =  Std of BodyAccY measure
      #BodyAccStdZ =  Std of BodyAccZ measure
      #GravityAccMeanX =  Mean of GravityAcc X measure
      #GravityAccMeanY =  Mean of GravityAcc Y measure
      #GravityAccMeanZ =  Mean of GravityAcc Z measure
      #GravityAccStdX =  Std of GravityAcc X measure
      #GravityAccStdY =  Std of GravityAcc Y measure
      #GravityAccStdZ =  Std of GravityAcc Z measure
      #BodyAccJerkMeanX =  Mean of BodyAccJerk X measure
      #BodyAccJerkMeanY =  Mean of BodyAccJerk Y measure
      #BodyAccJerkMeanZ = Mean of BodyAccJerk Zmeasure
      #BodyAccJerkStdX = Std of BodyAccJerk X measure
      #BodyAccJerkStdY = Std of BodyAccJerk Y measure
      #BodyAccJerkStdZ = Std of BodyAccJerk Z measure
      #BodyGyroMeanX = Mean of BodyGyro X measure
      #BodyGyroMeanY =  Mean of BodyGyro Y measure
      #BodyGyroMeanZ  =  Mean of BodyGyro Z measure
      #BodyGyroStdX =  Std of BodyGyro X measure
      #BodyGyroStdY =  Std of BodyGyro Y measure
      #BodyGyroStdZ = Std of BodyGyro Z measure
      #BodyGyroJerkMeanX = Mean of BodyGyroJerk X measure
      #BodyGyroJerkMeanY =  Mean of BodyGyroJerk Y measure
      #BodyGyroJerkMeanZ =  Mean of BodyGyroJerk Z measure
      #BodyGyroJerkStdX =  Std of BodyGyroJerk X measure
      #BodyGyroJerkStdY =  Std of BodyGyroJerk Y measure
      #BodyGyroJerkStdZ =  Std of BodyGyroJerk Z measure,
      #BodyAccMagMean =  Mean of BodyAccMag measure
      #BodyAccMagStd =  Std of BodyAccMag measure
      #GravityAccMagMean = Mean of GravityAccMag measure
      #GravityAccMagStd =  Std of GravityAccMag measure
      #BodyAccJerkMagMean =  Mean of BodyAccJerkMag measure
     # BodyAccJerkMagStd =  Std of BodyAccJerkMag measure
     # BodyGyroMagMean =  Mean of BodyGyroMag measure
     # BodyGyroMagStd =  Std of BodyGyroMag measure
      #BodyGyroJerkMagMean =  Mean of BodyGyroJerkMag measure
     # BodyGyroJerkMagStd =  Std of BodyGyroJerkMag measure
     # BodyAccMeanX2 =  Mean of BodyAccMeanX2 measure
    #  BodyAccMean2Y = Mean of BodyAccMean2Y measure
     # BBodyAccMean2Y = Mean of BodyAccMean2Y measure
      #BodyAccStdX2 = Std of BodyAccStdX2 measure
    #  BodyAccStdY2 = Std of BodyAccStdY2 measure
    #  BodyAccStdZ2 = Std of BodyAccStdZ2measure
     # BodyAccJerkMeanX2 = Mean of BodyAccJerkMeanX2 measure
    #  BodyAccJerkMeanY2 = Mean of BodyAccJerkMeanY2 measure
    #  BodyAccJerkMeanZ2 = Mean of BodyAccJerkMeanZ2 measure
     # BodyAccJerkStdX2 = Std of BodyAccJerkStdX2 measure
    #  BodyAccJerkStdY2 =  Std of BodyAccJerkStdY2 measure
     # BodyAccJerkStdZ2 = Std of BodyAccJerkStdZ2 measure
     # BodyGyroMeanX2 = Mean of BodyGyroMeanX2 measure
     # BodyGyroMeanY2 = Mean of BodyGyroMeanY2 measure
     # BodyGyroMeanZ2 = Mean of  BodyGyroMeanZ2 measure
     # BodyGyroStdX2 = Std of BodyGyroStdX2 measure
     # BodyGyroStdY2 = Std of BodyGyroStdY2 measure
     # BodyGyroStdZ2 = Std of BodyGyroStdZ2 measure
     # BodyAccMagMean2 = Mean of BodyAccMagMean2 measure
     # BodyAccMagStd2 =  Std of BodyAccMagStd2 measure
     # BodyBodyAccJerkMagMean = Std of BodyBodyAccJerkMagMean measure 
     # BodyBodyAccJerkMagStd = Std of BodyBodyAccJerkMagStd measure
     # BodyBodyGyroMagMean =  Mean of BodyBodyGyroMagMean measure
     # BodyBodyGyroMagStd =  Std of BodyBodyGyroMagStd measure
     # BodyBodyGyroJerkMagMean = Mean of BodyBodyGyroJerkMagMean measure
     # BodyBodyGyroJerkMagStd = Std of BodyBodyGyroJerkMagStd measure
