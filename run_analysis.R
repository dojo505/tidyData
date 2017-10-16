## script will read in data from 
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard 
##      deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent 
##      tidy data set with the average of each variable for each 
##      activity and each subject.
library(dplyr)

if (!(dir.exists("./train") && dir.exists("./test"))) {
    stop("one or more directories not found")
}

if (!dir.exists("./tidy_output")) {
    dir.create("./tidy_output")
}

if (file.exists("./tidy_output/merged_data.txt")) {
    stop("output file 'merged_data' exists, will not overwrite")
}

## PART 1
# read testdata
test_data <- read.table(file = "./test/X_test.txt")
test_activity <- read.table(file = "./test/y_test.txt")
test_subj_ids <- read.table(file = "./test/subject_test.txt")

# read training data
train_data <- read.table(file = "./train/X_train.txt")
train_activity <- read.table(file = "./train/y_train.txt")
train_subj_ids <- read.table(file = "./train/subject_train.txt")

#merge data
full_test <- cbind(test_subj_ids, test_activity, test_data)
full_train <- cbind(train_subj_ids, train_activity, train_data)

merged_data <- rbind(full_test, full_train)

## PART 4
features <- read.table(file = "features.txt", stringsAsFactors = FALSE)
lbls <- features[,2]
colnames(merged_data) <- append(c("subject","activity"), lbls)

## PART 2
## keep only columns describing mean and std_dev
## any label with "mean()" or "std()" in name is kept
## note - drop columns describing meanFreq()
mean_index <- grep("mean\\(", lbls)
std_index <- grep("std\\(", lbls)
mean_index <- mean_index + 2 
std_index <- std_index + 2
i <- c(1, 2) #keep subject and activity id
i <- append(i, mean_index)
i <- sort(append(i, std_index))
merged_data <- merged_data[,i] 

## PART 3
## rename activity data
act_labels <- read.table("./activity_labels.txt", stringsAsFactors = FALSE)
act_labels <- act_labels[,2]
merged_data[,2] <- act_labels[merged_data[,2]]

## PART 4 partially done above
## fix names by removing "()" characters
colnames(merged_data) <- gsub("\\(\\)", "",colnames(merged_data))
## now that full table is ready it is saved
write.table(merged_data, "./tidy_output/merged_data.txt")

## PART 5
## average for each subject and each activity
## 30 subjects by 6 activities = 180 rows
tidy_data <- merged_data %>% group_by(subject, activity) %>% 
    summarise_all(funs(mean))
write.table(tidy_data, "./tidy_output/tidy_data.txt")
## done?

