# Code Book for Getting and Cleaning Data Course Project
This code book describes the variables, the data, and any transformations or work that was performed to clean up the data
* Step 1: "Merges the training and the test sets to create one data set."
* Step 2: "Extracts only the measurements on the mean and standard deviation for each measurement."
* Step 3: "Uses descriptive activity names to name the activities in the data set."
* Step 4: "Appropriately labels the data set with descriptive activity names." 
* Step 5: "Creates a second, independent tidy data set with the average of each variable for each activity and each subject."

_____________________________________________________________

The dataset is based on data from [1], downloaded from [here](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

Training and the test sets were merged to create one data set
_____________________________________________________________

* test\subject_test.txt and train\subject_train.txt were row merged into a subject data table with the column name subjectID
* test\y_test.txt and train\y_train.txt were row merged into a single data table then a mapping was made between the ID in the y-data-frame and the activityTable created from activity_labels.txt to get a vector of the appropriate activity type called y
* test\x_test.txt, train\x_train.txt were row merged into x data table and the columns were given names from features.txt
* subjectID, activity and the x data were column merged into a complete table

Using a logical vector based on the column names of the complete table that only include subjectID, activity, and all columns that have mean() or std() created a new table called meanStdTable was formed . 

Column names:
_____________

1. Subject: 
.. range of values [1:30]
.. id of a subject performing their activity
2. Activity:
.. Name of activity performed

Rest of the columns correspond to averaged variables describing mean/std measurements. 
E.g. tGravityAcc-mean()-X
tGravityAcc-mean()-Y
tGravityAcc-mean()-Z
tGravityAcc-std()-X
tGravityAcc-std()-Y
tGravityAcc-std()-Z

Names of these variables are the same as in features.txt.

A second, data set with the average of each variable for each activity and each subject was created. The rows were reordered so that activities performed by each subject are in adjacent rows. 
Names of these variables are the same as in features.txt but the values are averages of each variable for each activity and each subject.

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

