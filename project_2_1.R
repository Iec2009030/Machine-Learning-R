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
average_error_lda<- vector("numeric",length(5))
average_error_3nn<- vector("numeric",length(5))

  for(j in c(20,30,40,50,60)){
      
      exact_err_lda <- vector("numeric",length(500)) # vector to store exact errors
      exact_err_3nn <- vector("numeric",length(500)) # vector to store exact errors
      
      for (i in c(1:500)) {
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
      
        #Calculating Mean
        m0 = apply(X0,2,mean)
        m1 = apply(X1,2,mean)
        
        #Calculating Intercept and Slope
        S <- ((n0-1)*var(X0)+(n1-1)*var(X1))/(j-2) # pooled covariance
        Si <- solve(S);
        a <- as.vector(Si%*%(m1-m0))
        b <- as.numeric(0.5%*%t(m0-m1)%*%Si%*%(m0+m1))
        slope <- -a[1]/a[2]
        intercept <- -b/a[2]
        
         # Simulate testing data(400)
        n1= floor(0.6*400)
        p = floor(0.4 *400)
        nt0 = sample(p:n1,1)
        nt1 = 400 - nt0 
        Xt0 <- mvrnorm(nt0,mu=c(0,0),Sigma=matrix(sig*c(1,corr,corr,1),2,2))
        Xt1 <- mvrnorm(nt1,mu=c(1,1),Sigma=matrix(sig*c(1,corr,corr,1),2,2))
        
        #Adding CLass to the data frame
        #Adding a new Collumn for Labels
        Xt2 <- rbind(Xt0,Xt1)
        
        data_frame_test <- data.frame(Xt2)
        data_frame_test["Class"] <- NA
        frame_temp = NULL
        size = nrow(data_frame_test)
        
        for(k in c(1:size)){
          if(k<=nt0){
            frame_temp[k]=0
          }
          if(k>nt0){
            frame_temp[k]=1
          }
        }
        frame_temp = factor(frame_temp)
        data_frame_test$Class = frame_temp
        
        # Find test-sample (Monte-Carlo) classification error
        err <- (sum(Xt0%*%a + b>0) + sum(Xt1%*%a + b <= 0))/400
        exact_err_lda[i] <- err
        
        # Find 3-NN based error
        prc_train_label = data_frame[,3]
        prc_test_label = data_frame_test[,3]
        prc_test_pred = knn(data_frame,data_frame_test,data_frame[,3],3)
        x=table(prc_test_pred,data_frame_test$Class)
        err_3nn = (x[[2]][1] + x[[3]][1])/400
        exact_err_3nn[i] <- err_3nn
      }
      exact_err_lda = matrix(exact_err_lda)
      average_error_lda[j] <- apply(exact_err_lda,2,mean)
      exact_err_3nn = matrix(exact_err_3nn)
      average_error_3nn[j] <- apply(exact_err_3nn,2,mean)
  }

average_error_3nn[1] = NA
average_error_lda[1] = NA
Sample_size <- c(20,30,40,50,60)

#Plotting the Result Obtained
plot(average_error_3nn,xlim=c(0,80),ylim=c(0,0.5),col="red",pch="O",lwd=2,xlab="Sample Size",ylab="Error ")
points(average_error_lda,col="red",pch="X")
legend("topright",legend=c("3NN","LDA"),pch=c('O','X'),col='red',bg="lightgray")
grid()
