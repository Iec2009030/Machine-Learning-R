# Materials Informatics
# Computer Project 2
# Problem 2

library(MASS)
library(ggplot2)
library(class)

#Reading the Test and Traininf files
setwd("~/Desktop/MTLS/PROJECT-2/")
SFE_training = read.table("SFE_Train_Data.txt ",header=T, sep = '\t')
SFE_test = read.table("SFE_Test_Data.txt ",header=T, sep = '\t')

#Selecting from Exhaustive Approach
cols <- c(1,2,3,4,5,6,7)
rows <- nrow(SFE_training)
#Storing all the Combinations possible in a matrix
all_comb <- combn(cols,5)
size <- ncol(all_comb)

#Forming Test and Traning set
err_lda_exhaust_cum <- vector("numeric",length(size))
err_3nn_exhaust_cum <- vector("numeric",length(size))

for (i in c(1:size)) {
  chosen_data = all_comb[,i]
  data_frame_training = SFE_training[,c(chosen_data)]
  data_frame_training$SFE = SFE_training[,8]
  
  #Calculating the Apparent error for LDA
  training_set.lda <- lda(SFE ~ ., data = data_frame_training)
  testing_set.lda.p <- predict(training_set.lda, newdata = data_frame_training[,c(1,2,3,4,5)])$class
  table_1_predictors=table(testing_set.lda.p,data_frame_training$SFE)
  err_lda_exhaust = (table_1_predictors[[2]][1] + table_1_predictors[[3]][1])/rows
  err_lda_exhaust_cum[i] = err_lda_exhaust
  
  #Calculating the Apparent error for 3NN
  prc_test_pred = knn(data_frame_training[,-6],data_frame_training[,-6],data_frame_training[,6],3)
  table_2_predictors=table(prc_test_pred,data_frame_training$SFE)
  err_3nn_exhaust = (table_2_predictors[[2]][1] + table_2_predictors[[3]][1])/rows
  err_3nn_exhaust_cum[i] <- err_3nn_exhaust
  
}
  #Finding the Best 5 predictors using Exhaustive Search using LDA and 3NN
   err_3nn_exhaust_cum = matrix(err_3nn_exhaust_cum)
   err_lda_exhaust_cum = matrix(err_lda_exhaust_cum)
   lda_min = apply(err_lda_exhaust_cum,2,min)
   knn_min = apply(err_3nn_exhaust_cum,2,min)
   
   err_3nn_exhaust_cum = as.vector(err_3nn_exhaust_cum)
   err_lda_exhaust_cum = as.vector(err_lda_exhaust_cum)
   len = length(err_3nn_exhaust_cum)
   
   for (i in c(1:len)) {
     if (lda_min == err_lda_exhaust_cum[i] ){
       best_lda = all_comb[,i]
       break;
     }
   }
   for (i in c(1:len)) {
     if (lda_min == err_3nn_exhaust_cum[i] ){
       best_3nn = all_comb[,i]
       break;
     }
   }
   
  #Selecting from Sequential Forward Search
   
  columns = 7
  err_sfs_train = vector("numeric",length(7))
  name = names(SFE_training)
  
  #Selecting the 1st Feature
  for (i in c(1:7)) {
    
    training_sfs = as.data.frame(SFE_training[,i])
    training_sfs$SFE = SFE_training[,8]
    colnames(training_sfs)[1] = name[i]
    
    #Calculating Error
    training_set.lda <- lda(SFE ~ ., data = training_sfs)
    b = data.frame(training_sfs[,1])
    colnames(b)[1] = name[i]
    testing_set.lda.p <- predict(training_set.lda, newdata = b,se.fit=TRUE)$class
    table_1_predictors=table(testing_set.lda.p,training_sfs$SFE)
    err_sfs = (table_1_predictors[[2]][1] + table_1_predictors[[3]][1])/rows
    err_sfs_train[i] = err_sfs
    
  }
  err_sfs_train = matrix(err_sfs_train)
  first_feature = apply(err_sfs_train,2,min)
  f_f = which(err_sfs_train %in% first_feature)
  
  #Selecting 2nd Feature
  vec = 1:7
  for (i in vec[-f_f]) {
    
    training_sfs = SFE_training[,c(i,f_f)]
    training_sfs$SFE = SFE_training[,8]
    
    #Calculating Error
    training_set.lda <- lda(SFE ~ ., data = training_sfs)
    testing_set.lda.p <- predict(training_set.lda, newdata = training_sfs[,c(1,2)],se.fit=TRUE)$class
    table_1_predictors=table(testing_set.lda.p,training_sfs$SFE)
    err_sfs = (table_1_predictors[[2]][1] + table_1_predictors[[3]][1])/rows
    err_sfs_train[i] = err_sfs
    
  }
  err_sfs_train[c(f_f),] = 1
  second_feature = apply(err_sfs_train,2,min)
  s_f = which(err_sfs_train %in% second_feature)
  s_f = as.numeric(s_f[1])
  
  #Selecting 3rd Feature
  vec = 1:7
  for (i in vec[-c(s_f,f_f)]) {
    
    training_sfs = SFE_training[,c(i,f_f,s_f)]
    training_sfs$SFE = SFE_training[,8]
    
    #Calculating Error
    training_set.lda <- lda(SFE ~ ., data = training_sfs)
    testing_set.lda.p <- predict(training_set.lda, newdata = training_sfs[,c(1,2,3)],se.fit=TRUE)$class
    table_1_predictors=table(testing_set.lda.p,training_sfs$SFE)
    err_sfs = (table_1_predictors[[2]][1] + table_1_predictors[[3]][1])/rows
    err_sfs_train[i] = err_sfs
    
  }
  err_sfs_train[c(s_f,f_f),] = 1
  third_feature = apply(err_sfs_train,2,min)
  t_f = which(err_sfs_train %in% third_feature)
  t_f = as.numeric(t_f[1])
  
  #Selecting 4th Feature
  vec = 1:7
  for (i in vec[-c(s_f,f_f,t_f)]) {
    
    training_sfs = SFE_training[,c(i,f_f,s_f,t_f)]
    training_sfs$SFE = SFE_training[,8]
    
    #Calculating Error
    training_set.lda <- lda(SFE ~ ., data = training_sfs)
    testing_set.lda.p <- predict(training_set.lda, newdata = training_sfs[,c(1,2,3,4)],se.fit=TRUE)$class
    table_1_predictors=table(testing_set.lda.p,training_sfs$SFE)
    err_sfs = (table_1_predictors[[2]][1] + table_1_predictors[[3]][1])/rows
    err_sfs_train[i] = err_sfs
    
  }
  err_sfs_train[c(s_f,f_f,t_f),] = 1
  fourth_feature = apply(err_sfs_train,2,min)
  fo_f = which(err_sfs_train %in% fourth_feature)
  fo_f = as.numeric(fo_f[1])
  
  #Selecting 5th Feature
  vec = 1:7
  for (i in vec[-c(s_f,f_f,t_f,fo_f)]) {
    
    training_sfs = SFE_training[,c(i,f_f,s_f,t_f,fo_f)]
    training_sfs$SFE = SFE_training[,8]
    
    #Calculating Error
    training_set.lda <- lda(SFE ~ ., data = training_sfs)
    testing_set.lda.p <- predict(training_set.lda, newdata = training_sfs[,c(1,2,3,4,5)],se.fit=TRUE)$class
    table_1_predictors=table(testing_set.lda.p,training_sfs$SFE)
    err_sfs = (table_1_predictors[[2]][1] + table_1_predictors[[3]][1])/rows
    err_sfs_train[i] = err_sfs
    
  }
  err_sfs_train[c(s_f,f_f,t_f,fo_f),] = 1
  fifth_feature = apply(err_sfs_train,2,min)
  fi_f = which(err_sfs_train %in% fifth_feature)
  fi_f = as.numeric(fi_f[1])
