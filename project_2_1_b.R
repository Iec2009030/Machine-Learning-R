library(MASS)
library(ggplot2)
library(class)
library(gmodels)

#Parameters for Simulation
corr = 0.2    #Correlation value
prior_p = 0.5 #Prior Probablity
sig = 1      #Value of the Sigma

#Generating 500 Synthetic data of sample size 20,30,40,50 and 60
# with Sigma value 1
average_error_lda_apparent<- vector("numeric",length(5))
average_error_3nn_apparent<- vector("numeric",length(5))
average_err_lda_cv <- vector("numeric",length(5))
average_err_3nn_cv <- vector("numeric",length(5))
average_err_lda_cv_5 <- vector("numeric",length(5))
average_err_3nn_cv_5 <- vector("numeric",length(5))

for(j in c(20,30,40,50,60)){
  
  exact_err_lda_apparent <- vector("numeric",length(300)) # vector to store exact errors
  exact_err_3nn_apparent <- vector("numeric",length(300)) # vector to store exact errors
  exact_err_lda_cv    <- vector("numeric",length(300))
  exact_err_3nn_cv   <- vector("numeric",length(300))
  exact_err_lda_cv_5   <- vector("numeric",length(300))
  exact_err_3nn_cv_5 <- vector("numeric",length(300))
  for (i in c(1:300)) {
    n1= floor(0.6*j)
    p = floor(0.4 *j)
    n0 = sample(p:n1,1)
    n1 = j - n0 
    
    #Generating the samples
    X0 <- mvrnorm(n0,mu=c(0,0),Sigma=matrix(sig*c(1,corr,corr,1),2,2))
    X1 <- mvrnorm(n1,mu=c(1,1),Sigma=matrix(sig*c(1,corr,corr,1),2,2))
    
    #Adding a new Collumn for Labels
    X2 <- rbind(X0,X1)
    
    data_frame <- data.frame(X2)
    data_frame["Class"] <- NA
    frame_temp = NULL
    size = nrow(data_frame)
    
    for(k in c(1:size)){
      if(k<=n0){
        frame_temp[k]=0
      }
      if(k>n0){
        frame_temp[k]=1
      }
    }
    frame_temp = factor(frame_temp)
    data_frame$Class = frame_temp
    
    ####Staring Leave one Out Cross Validation Error Estimator#####
    loop1 = nrow(data_frame)
    exact_err_lda_cv_loop <- vector("numeric",length(loop1))
    exact_err_3nn_cv_loop <- vector("numeric",length(loop1))
    exact_err_lda_cv_5_loop <- vector("numeric",length(loop1))
    exact_err_3nn_cv_5_loop <- vector("numeric",length(loop1))
    
    for (k in c(1:loop1)) {
      data_frame_training = data_frame[-k,]
      data_frame_test = data_frame[k,]
      
      #Calculating the error for LDA
      training_set.lda <- lda(Class ~ X1 + X2, data = data_frame_training,prior=c(0.5,0.5))
      testing_set.lda.p <- predict(training_set.lda, newdata = data_frame_test[,c(1,2)])$class
      table_1_predictors=table(testing_set.lda.p,data_frame_test$Class)
      err_lda_CV_1 = (table_1_predictors[[2]][1] + table_1_predictors[[3]][1])
      exact_err_lda_cv_loop[k] = err_lda_CV_1
      
      #Calculating error for 3NN
      prc_train_label = data_frame_training[,3]
      prc_test_label = data_frame_test[,3]
      prc_test_pred = knn(data_frame_training,data_frame_test,data_frame_training[,3],3)
      x2=table(prc_test_pred,data_frame_test$Class)
      err_3nn = (x2[[2]][1] + x2[[3]][1])
      exact_err_3nn_cv_loop[k] <- err_3nn
    }
    exact_err_lda_cv_loop = matrix(exact_err_lda_cv_loop)
    exact_err_3nn_cv_loop = matrix(exact_err_3nn_cv_loop)
    exact_err_lda_cv[i] = apply(exact_err_lda_cv_loop,2,mean)
    exact_err_3nn_cv[i] = apply(exact_err_3nn_cv_loop,2,mean)
    
    
    #####Starting 5 cross Validation ##################
    loop2 = nrow(data_frame)
    k = 5
    b = loop2/5
    k1 = 1 
    count = 1
    while (k > 0){
      k2 = k1 + b - 1
      data_frame_training = data_frame[-c(k1:k2),]
      data_frame_test = data_frame[c(k1:k2),]
      k1 = k1 + b
      
      #Calculating the error for LDA
      training_set.lda <- lda(Class ~ X1 + X2, data = data_frame_training,prior=c(0.5,0.5))
      testing_set.lda.p <- predict(training_set.lda, newdata = data_frame_test[,c(1,2)])$class
      table_1_predictors=table(testing_set.lda.p,data_frame_test$Class)
      err_lda_CV_1 = (table_1_predictors[[2]][1] + table_1_predictors[[3]][1])/b
      exact_err_lda_cv_5_loop[count] = err_lda_CV_1
      
      #Calculating error for 3NN
      prc_train_label = data_frame_training[,3]
      prc_test_label = data_frame_test[,3]
      prc_test_pred = knn(data_frame_training,data_frame_test,data_frame_training[,3],3)
      x2=table(prc_test_pred,data_frame_test$Class)
      err_3nn = (x2[[2]][1] + x2[[3]][1])/b
      exact_err_3nn_cv_5_loop[count] <- err_3nn
      
      count = count + 1
      k = k - 1
    }
    exact_err_lda_cv_5_loop = matrix(exact_err_lda_cv_5_loop)
    exact_err_3nn_cv_5_loop = matrix(exact_err_3nn_cv_5_loop)
    exact_err_lda_cv_5[i] = apply(exact_err_lda_cv_5_loop,2,mean)
    exact_err_3nn_cv_5[i] = apply(exact_err_3nn_cv_5_loop,2,mean)
    
    #Calculating Mean
    m0 = apply(X0,2,mean)
    m1 = apply(X1,2,mean)
    
    #Calculating Intercept and Slope
    S <- ((n0-1)*var(X0)+(n1-1)*var(X1))/(j-2) # pooled covariance
    Si <- solve(S);
    a <- as.vector(Si%*%(m1-m0))
    b <- as.numeric(0.5%*%t(m0-m1)%*%Si%*%(m0+m1))
    
    # Find exact classification error
    #dt0 <- sqrt(a[1]^2+2*corr*a[1]*a[2]+a[2]^2)
    #err <- 0.5*(pnorm((-a[1]-a[2]-b)/dt0)+pnorm(b/dt0))
    err <- (sum(X0%*%a + b>0) + sum(X1%*%a + b <= 0))/j
    exact_err_lda_apparent[i] <- err
    
    #Finding Apparent error for 3NN
    prc_test_pred = knn(data_frame,data_frame,data_frame[,3],3)
    x1=table(prc_test_pred,data_frame$Class)
    err_3nn = (x1[[2]][1] + x1[[3]][1])/j
    exact_err_3nn_apparent[i] <- err_3nn
  }
  exact_err_lda_apparent = matrix(exact_err_lda_apparent)
  exact_err_3nn_apparent = matrix(exact_err_3nn_apparent)
  average_error_lda_apparent[j] <- apply(exact_err_lda_apparent,2,mean)
  average_error_3nn_apparent[j] <- apply(exact_err_3nn_apparent,2,mean)
  
  exact_err_lda_cv = matrix(exact_err_lda_cv)
  exact_err_3nn_cv = matrix(exact_err_3nn_cv)
  average_err_lda_cv[j] <- apply(exact_err_lda_cv,2,mean)
  average_err_3nn_cv[j] <- apply(exact_err_3nn_cv,2,mean)
  
  exact_err_lda_cv_5 = matrix(exact_err_lda_cv_5)
  exact_err_3nn_cv_5 = matrix(exact_err_3nn_cv_5)
  average_err_lda_cv_5[j] <- apply(exact_err_lda_cv_5,2,mean)
  average_err_3nn_cv_5[j] <- apply(exact_err_3nn_cv_5,2,mean)
}

average_error_lda_apparent[1] = NA
average_error_3nn_apparent[1] = NA
average_err_lda_cv[1] = NA
average_err_3nn_cv[1] = NA
average_err_lda_cv_5[1] = NA
average_err_3nn_cv_5[1] = NA

#Sample_size <- c(20,30,40,50,60)
#Plotting the Result Obtained
#plot(average_error_3nn,xlim=c(0,80),ylim=c(0,0.5),col="red",pch="O",lwd=2,xlab="Sample Size",ylab="Error ")
#points(average_error_lda,col="red",pch="X")
#legend("topright",legend=c("3NN","LDA"),pch=c('O','X'),col='red',bg="lightgray")
#grid()
