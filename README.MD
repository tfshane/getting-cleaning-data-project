# Getting and Cleaning Data Course Project
This is the repo for the final project for Johns Hopkins Getting and Cleaning Data course.

This repo contains the following files and folders. 
* **README.md** The README file for this repo.
* **CodeBook.md** A code book that describes the variables, the data, and any transformations or work that you performed to clean up the data
* **run_analysis.R** The r script processes the data and generated the required tidy dataset for submission. 

### Overview
This project serves to demonstrate the collection and cleaning of a tidy data set that can be used for subsequent
analysis. A full description of the data used in this project can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

[The source data for this project can be found here.](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Making Modifications to This Script
Once you have obtained and unzipped the source files, you will need to make one modification to the R file before you can process the data.
Note that on line 1 of run_analysis.R, you will set the path of the working directory to select the location of the source files in your own directory.

### Project Summary
The following is a summary description of the project instructions

You should create one R script called run_analysis.R that does the following. 
**1.** Merges the training and the test sets to create one data set.
**2.** Extracts only the measurements on the mean and standard deviation for each measurement. 
**3.** Uses descriptive activity names to name the activities in the data set
**4.** Appropriately labels the data set with descriptive activity names. 
**5.** Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
