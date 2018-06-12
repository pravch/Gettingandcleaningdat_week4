library(dplyr)

# read train data, test data, variable names and activity names
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
Subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
Subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
variable_names <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

#Merge the training data set and the test data set to create one single data set
X_total <- rbind(X_train, X_test)
Y_total <- rbind(Y_train, Y_test)
Subject_total <- rbind(Subject_train, Subject_test)

#Search for variables names which contail mean and std to extract only values of those variables
selected_var <- variable_names[grep("mean\\(\\)|std\\(\\)",variable_names[,2]),]
X_total <- X_total[,selected_var[,1]]

#Assign Descriptive activity names to name the activities in the data set
colnames(Y_total) <- "activity"
Y_total$activitylabel <- factor(Y_total$activity, labels = as.character(activity_labels[,2]))
activitylabel <- Y_total[,-1]

# Lable the data set with descriptive variable names.
colnames(X_total) <- variable_names[selected_var[,1],2]

# Create an inndependent tidy data set with the average of each variable for each activity and each subject.
colnames(Subject_total) <- "subject"
total <- cbind(X_total, activitylabel, Subject_total)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_all(funs(mean))
write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)
