ReadMe Getting and Cleaning Data MOOC Course Project
====================================================
August 24, 2014
Greetings for 

Content
-------

**ReadMe** for *run_analysis.R* script and *codebook.txt*

The script *run_analysis.R* constructs a dataset of averaged mean and standard deviation
accelerometer and gyroscope data for 30 subjects and 6 activities (WALKING, WALKING_UPSTAIRS, 
WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING). Data were collected using a smartphone (Samsung 
Galaxy S II) on the waist. Raw data from experiment conducted by 

> Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. 
> Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. 
> International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

The *codebook.txt* contains a list of the variables in the dataset produced by *run_analysis.R*. 
Each entry includes a short description of the data and the data type. The characters "a...b" are used
to indicate that a variable with a data type that takes continuous values on the interval [0,1]. 

run_analysis.R
--------------

This script assembles a dataset based on the contents of the HCI HAR DATASET directory.
If this directory is in the working directory, the script will function properly.
Specifically, this script acts on the following eight files. 

- Files from top level of UCI HAR Dataset directory
1. *UCI HAR DATASET/activity_labels.txt* 
2. *UCI HAR DATASET/features.txt*
- Files from the test subdirectory of UCI HAR dataset
3. *UCI HAR DATASET/test/X_test.txt*
4. *UCI HAR DATASET/test/y_test.txt* 
5. *UCI HAR DATASET/test/subject_test.txt
-Files from the train subdirectory of UCI HAR dataset
6. *UCI HAR DATASET/train/X_train.txt* 
7. *UCI HAR DATASET/train/y_train.txt*
8. *UCI HAR DATASET/train/subject_train.txt*

The output of the script is a ``tidy'' dataset *ouput.txt*. Each row of this dataset is a single
observation, each column pertains to a single variable, and the variables are all given 
descriptive names. This is consistent with Hadley Wickhams [principles of tidy data, section 2](http://vita.had.co.nz/papers/tidy-data.pdf).
This dataset contains *averaged* data for each *subject and activity* for a subset of the original 
Human Activity data. The subset of data this script acts on is all the *mean* and *standard deviation* 
data for the various accelerometer and gyroscope data in the Human Activity dataset.(There is some 
ambiguity about exactly which mean variable to include. I have chosen to be inclusiveand keep all 
variables with contain the string `mean.'However, the script includes syntax to select only those 
variables with *mean()*. To run this code, simply uncomment the relevant lines of code). 

Specifically, *run_analysis.R* does the following:

1. Merges the test data with the test labels on subject on activity. Then it does the same for the train data.
2. Combines the test and train data to form one large dataset.
3. Variables names are added to all variables. 
I chose to use the data labels provided in the ``feature_info.txt'' file as the names. 
These are *descriptive* names because they say whether the data is time or frequency domain, what the data 
measures, and, when relevant, whether the measurement is in the x,y, or z direction. 
4. Extracts the measurements on the mean and standard deviation variables only.
5. Use a split, apply, combine procedure to compute the mean of every variable for each subject, activity 
and then recombine this into a new dataframe.
6. The activity labels specified by the original experiment (WALKING, etc.) were then attached.
7. write.table used to create *output.txt* file. Notice that the data has headers. This file contains
the average of each variable for each activity and each subject. 


