Code Book
============================


# Experiment details

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 


# Data information

See the link: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones for more info on the data.


# Dataset

See the data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip and in particular the README.txt file and the features_info.txt file for more info on the data.

There are 561 variables in test and training datasets. Test and training datasets differ in data related to number of subjects.

# Following changes were made to the dataset

* Subset of dataset was created to only include mean and standard deviation variables. So out of 561, 66 variables were included
* This subsetting was done by only including variables with "mean" and "std" strings in variable names. 
* Six activies listed above were matched with related observation
* Variable names were cleaned to remove the "-" or "()" from original names
* A tidy data set was created that calculated the average of each variable
