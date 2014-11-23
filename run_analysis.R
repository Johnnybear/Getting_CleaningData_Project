library(plyr)
library(reshape2)
#-------------------------------------------------------------------------
# Merges the training and the test sets to create one data set.
#-------------------------------------------------------------------------
setwd("C:/Users/Johnny/R/Getting_CleaningData_Project/UCI HAR Dataset")
test<-read.table("./test/X_test.txt")
train<-read.table("./train/X_train.txt")

# add the subjects now
test.subjects<-read.table("./test/subject_test.txt")
test<-cbind(test,test.subjects)

train.subjects<-read.table("./train/subject_train.txt")
train<-cbind(train,train.subjects)

# add the activity codes
test.activitycodes<-read.table("./test/y_test.txt")
test<-cbind(test,test.activitycodes)

train.activitycodes<-read.table("./train/y_train.txt")
train<-cbind(train,train.activitycodes)

#combine test and train rows
combined<-rbind(test, train)

# add descriptive column names from features.txt 
combined.colnames<-read.table("./features.txt")
colnames(combined)<-c(as.character(combined.colnames$V2),"subjects","activitycodes")

#-------------------------------------------------------------------------
# Extracts only the measurements on the mean and standard deviation 
# for each measurement. 
#-------------------------------------------------------------------------
MeansAndStd<-grep("mean[^F]|-std|subjects|activitycodes",names(combined),value=TRUE)  

combined<-combined[MeansAndStd]

#-------------------------------------------------------------------------
# Use descriptive activity names to name the activities in the data set
#-------------------------------------------------------------------------
acrvlabels<-read.table("./activity_labels.txt")
combined<-cbind(combined,activity=acrvlabels[combined$activitycodes,][2])
# remove the activity code column
combined <- subset( combined, select = -activitycodes )
colnames(combined)[68] <- "activity"

#-------------------------------------------------------------------------
# Appropriately labels the data set with descriptive variable names. 
#-------------------------------------------------------------------------
colList<-names(combined)

# use camelcase to make these more readible
colList<-gsub("mean","Mean",colList)
colList<-gsub("std","StdDev",colList)
colList<-gsub("BodyBody","Body",colList)
colList<-gsub("\\(","",colList)
colList<-gsub("\\)","",colList)
colList<-gsub("\\-","",colList)
colList<-gsub("^t","Time",colList)
colList<-gsub("^f","Freq",colList)

#add ave* prefix to the fields for the final dataset
newCols = c()
for(mycol in colList){
        if (!(mycol %in% c("subjects","activity"))){
                newCols = append(newCols,paste("ave", mycol, sep=""))
        } else {
                newCols = append(newCols,mycol)
        }  
} 
colnames(combined)<-newCols

#remove the underscore and make all activity data values lowercase
combined$activity<-gsub("WALKING_UPSTAIRS","walkingUpstairs", combined$activity)
combined$activity<-gsub("WALKING_DOWNSTAIRS","walkingDownstairs", combined$activity)
combined$activity<-gsub("SITTING","sitting", combined$activity)
combined$activity<-gsub("STANDING","standing", combined$activity)
combined$activity<-gsub("WALKING","walking", combined$activity)
combined$activity<-gsub("LAYING","laying", combined$activity)

#-------------------------------------------------------------------------
# Creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject. 
#-------------------------------------------------------------------------

#sort by subjects, activities
combined<-arrange(combined,subjects,activity)

#melt down into numeric variables by subject,activity
melted<-melt(combined,id=c("subjects","activity"))

#group by subject,activity
tidy<-dcast(melted,subjects+ activity ~ variable,mean)

#write out to a csv
write.csv(tidy, file="./tidyData.csv",row.names=FALSE)