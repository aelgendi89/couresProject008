run_analysis<-function(){
  
  ## part 1
  
  ## read X_train.txt
  file<-"./UCI HAR Dataset/train/X_train.txt"
  train<-read.table(file,header=FALSE)
  
  
  ## read X_test.txt
  file<-"./UCI HAR Dataset/test/X_test.txt"
  test<-read.table(file,header=FALSE)
  
  ## merging train and test
  data<-rbind(test,train)
  
  #########################################################
  
  ## part 2
  
  ## read features 
  file<-"./UCI HAR Dataset/features.txt"
  features<-read.table(file,header=FALSE)
  
  ## name features to dataset
  names(data)<-features[[2]]
  
  ## Extracts only the measurements 
  ## on the mean and standard deviation for each measurement
  x_mean<-grep("mean()",features[,2],fixed=TRUE)
  x_std<-grep("std()",features[,2],fixed=TRUE)
  
  features1<-features[x_std,]
  features1<-rbind(features[x_mean,],features1)
  
  data1<-data[,features1[,1]]
  
  #####################################################
  
  ## part 3
  
  ## read test labels Y_test.txt
  file<-"./UCI HAR Dataset/test/Y_test.txt"
  test_label<-read.table(file,header=FALSE)
  
  ## read train labels Y_train.txt
  file<-"./UCI HAR Dataset/train/Y_train.txt"
  train_label<-read.table(file,header=FALSE)  
    
  ## merge test and train labels
  data_label<-rbind(test_label,train_label)
  
  
  ## read activity_label.txt
  file<-"./UCI HAR Dataset/activity_labels.txt"
  activity_label<-read.table(file,header=FALSE)
  
  ## merge label with activity labels
  labels<-merge(data_label,activity_label,by.x="V1",by.y="V1")
  data2<-cbind(data1,labels[,2])
  
  ####################################################
  
  ## part 4
  
  ## name activity variable
  names(data2)[67]<-"Activity"
  
  ## read subject_test.txt
  file<-"./UCI HAR Dataset/test/subject_test.txt"
  subject_test<-read.table(file,header=FALSE)
  
  ## read subject_train.txt
  file<-"./UCI HAR Dataset/train/subject_train.txt"
  subject_train<-read.table(file,header=FALSE)
  
  ## merge subject test and subject train
  data_subject<-rbind(subject_test,subject_train)
  
  ## merge subject data to data set
  data3<-cbind(data2,data_subject)
  names(data3)[68]<-"Subject"
  
  ####################################################
  
  ## part 5
  
  
  ## the average for each variable after grouping
  library(dplyr)
  by_cyl <- group_by(data3,Activity,Subject)
  summury<-summarise_each(by_cyl,funs(mean))
  
  ## out summury at file called tidydata.txt
  write.table(summury,"tidydata.txt",row.name=FALSE)
}
