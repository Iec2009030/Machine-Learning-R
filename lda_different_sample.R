library(ggplot2)
library(mvtnorm)
library(MASS)
library(Matrix)
#generate 500 Sample data, Building the Testset
#Generate a covariance matrix with sigma = 1 and row = 0.2
n1= floor(0.6*500)
p = floor(0.4 *500)
n = sample(p:n1,1)
Sigma <- matrix(c(1,0.2,0.2,1),2,2)
norm1 <- rmvnorm(n,rep(0,2),Sigma)
norm2 <- rmvnorm(500-n,rep(1,2),Sigma)
norm3 <- rbind(norm1,norm2)

data_frame_test <- data.frame(norm3)
data_frame_test["Class"] <- NA
frame_temp = NULL
size = nrow(data_frame_test)
for(i in c(1:size)){
  if(i<=n){
    frame_temp[i]=1
  }
  if(i>n){
    frame_temp[i]=0
  }
}

frame_temp = factor(frame_temp)
data_frame_test$Class = frame_temp

#Generate data for 40,60,80 and 100 samples
n1= floor(0.6*40)
p = floor(0.4 *40)
q1 = sample(p:n1,1)
Sigma <- matrix(c(1,0.2,0.2,1),2,2)
norm1 <- rmvnorm(q1,rep(0,2),Sigma)
norm2 <- rmvnorm(40-q1,rep(1,2),Sigma)
norm3 <- rbind(norm1,norm2)

data_frame_training1 <- data.frame(norm3)
data_frame_training1["Class"] <- NA
frame_temp = NULL
size = nrow(data_frame_training1)
for(i in c(1:size)){
  if(i<=q1){
    frame_temp[i]=1
  }
  if(i>q1){
    frame_temp[i]=0
  }
}

frame_temp = factor(frame_temp)
data_frame_training1$Class = frame_temp

n1= floor(0.6*60)
p = floor(0.4 *60)
q2 = sample(p:n1,1)
Sigma <- matrix(c(1,0.2,0.2,1),2,2)
norm1 <- rmvnorm(q2,rep(0,2),Sigma)
norm2 <- rmvnorm(60-q2,rep(1,2),Sigma)
norm3 <- rbind(norm1,norm2)

data_frame_training2 <- data.frame(norm3)
data_frame_training2["Class"] <- NA
frame_temp = NULL
size = nrow(data_frame_training2)
for(i in c(1:size)){
  if(i<=q2){
    frame_temp[i]=1
  }
  if(i>q2){
    frame_temp[i]=0
  }
}

frame_temp = factor(frame_temp)
data_frame_training2$Class = frame_temp

n1= floor(0.6*80)
p = floor(0.4 *80)
q3 = sample(p:n1,1)
Sigma <- matrix(c(1,0.2,0.2,1),2,2)
norm1 <- rmvnorm(q3,rep(0,2),Sigma)
norm2 <- rmvnorm(80-q3,rep(1,2),Sigma)
norm3 <- rbind(norm1,norm2)

data_frame_training3 <- data.frame(norm3)
data_frame_training3["Class"] <- NA
frame_temp = NULL
size = nrow(data_frame_training3)
for(i in c(1:size)){
  if(i<=q3){
    frame_temp[i]=1
  }
  if(i>q3){
    frame_temp[i]=0
  }
}

frame_temp = factor(frame_temp)
data_frame_training3$Class = frame_temp

n1= floor(0.6*100)
p = floor(0.4 *100)
q4 = sample(p:n1,1)
Sigma <- matrix(c(1,0.2,0.2,1),2,2)
norm1 <- rmvnorm(q4,rep(0,2),Sigma)
norm2 <- rmvnorm(100-q4,rep(1,2),Sigma)
norm3 <- rbind(norm1,norm2)

data_frame_training4 <- data.frame(norm3)
data_frame_training4["Class"] <- NA
frame_temp = NULL
size = nrow(data_frame_training4)
for(i in c(1:size)){
  if(i<=q4){
    frame_temp[i]=1
  }
  if(i>q4){
    frame_temp[i]=0
  }
}

frame_temp = factor(frame_temp)
data_frame_training4$Class = frame_temp

#Calculating the error for n =40
training_set.lda <- lda(Class ~ X1 + X2, data = data_frame_training1,prior=c(0.5,0.5))
testing_set.lda.p <- predict(training_set.lda, newdata = data_frame_test[,c(1,2)])$class
table_1_predictors=table(testing_set.lda.p,data_frame_test$Class)

#Calculating the error for n =60
training_set.lda <- lda(Class ~ X1 + X2, data = data_frame_training2,prior=c(0.5,0.5))
testing_set.lda.p <- predict(training_set.lda, newdata = data_frame_test[,c(1,2)])$class
table_2_predictors=table(testing_set.lda.p,data_frame_test$Class)

#Calculating the error for n =80
training_set.lda <- lda(Class ~ X1 + X2, data = data_frame_training3,prior=c(0.5,0.5))
testing_set.lda.p <- predict(training_set.lda, newdata = data_frame_test[,c(1,2)])$class
table_3_predictors=table(testing_set.lda.p,data_frame_test$Class)

#Calculating the error for n =100
training_set.lda <- lda(Class ~ X1 + X2, data = data_frame_training4,prior=c(0.5,0.5))
testing_set.lda.p <- predict(training_set.lda, newdata = data_frame_test[,c(1,2)])$class
table_4_predictors=table(testing_set.lda.p,data_frame_test$Class)

#Error using the Error formula provided in class
error = NULL
#Calculating the coefficient an and bn
size = nrow(data_frame_training1)
mean2 = sapply(data_frame_training1[c(1,q1),c(1,2)], mean)
mean1 = sapply(data_frame_training1[c(q1+1,size),c(1,2)], mean)

cov1 = cov(data_frame_training1$X1,data_frame_training1$X2)
cov_inv = ginv(cov1)

mean_add1 = mean1 + mean2
mean_sub1 = mean2 - mean1
mean_t1 = t(mean_sub1)

an1= cov_inv %*% mean_sub1
an1_t1 = t(an1)
bn1 = -1/2 * mean_t1
cov_inv = c(cov_inv)
bn2 = bn1 * cov_inv
bn = bn2 %*% mean_add1

inter_med = ((an1 %*% mean1) + bn) / sqrt(t(an1_t1 * cov1)  %*% an1_t1)
inter_med1 = -((an1 %*% mean2) + bn) / sqrt(t(an1_t1 * cov1)  %*% an1_t1)
error[1] = 1/2 *(pnorm(inter_med) + pnorm(inter_med1))

size = nrow(data_frame_training2)
mean2 = sapply(data_frame_training2[c(1,q2),c(1,2)], mean)
mean1 = sapply(data_frame_training2[c(q2+1,size),c(1,2)], mean)
cov1 = cov(data_frame_training2$X1,data_frame_training2$X2)
cov_inv = ginv(cov1)

mean_add1 = mean1 + mean2
mean_sub1 = mean2 - mean1
mean_t1 = t(mean_sub1)

an1 = cov_inv %*% mean_sub1
an1_t1 = t(an1)

bn1 = -1/2 * mean_t1
cov_inv = c(cov_inv)
bn2 = bn1 * cov_inv
bn = bn2 %*% mean_add1

inter_med = ((an1 %*% mean1) + bn) / sqrt(t(an1_t1 * cov1)  %*% an1_t1)
inter_med1 = -((an1 %*% mean2) + bn) / sqrt(t(an1_t1 * cov1)  %*% an1_t1)
error[2] = 1/2 *(pnorm(inter_med) + pnorm(inter_med1))

size = nrow(data_frame_training3)
mean2 = sapply(data_frame_training3[c(1,q3),c(1,2)], mean)
mean1 = sapply(data_frame_training3[c(q3+1,size),c(1,2)], mean)
cov1 = cov(data_frame_training3$X1,data_frame_training3$X2)
cov_inv = ginv(cov1)

mean_add1 = mean1 + mean2
mean_sub1 = mean2 - mean1
mean_t1 = t(mean_sub1)

an1 = cov_inv %*% mean_sub1
an1_t1 = t(an1)

bn1 = -1/2 * mean_t1
cov_inv = c(cov_inv)
bn2 = bn1 * cov_inv
bn = bn2 %*% mean_add1

inter_med = ((an1 %*% mean1) + bn) / sqrt(t(an1_t1 * cov1)  %*% an1_t1)
inter_med1 = -((an1 %*% mean2) + bn) / sqrt(t(an1_t1 * cov1)  %*% an1_t1)
error[3] = 1/2 *(pnorm(inter_med) + pnorm(inter_med1))

size = nrow(data_frame_training4)
mean2 = sapply(data_frame_training4[c(1,q4),c(1,2)], mean)
mean1 = sapply(data_frame_training4[c(q4+1,size),c(1,2)], mean)
cov1 = cov(data_frame_training4$X1,data_frame_training4$X2)
cov_inv = ginv(cov1)

mean_add1 = mean1 + mean2
mean_sub1 = mean2 - mean1
mean_t1 = t(mean_sub1)

an1 = cov_inv %*% mean_sub1
an1_t1 = t(an1)
bn1 = -1/2 * mean_t1
cov_inv = c(cov_inv)
bn2 = bn1 * cov_inv
bn = bn2 %*% mean_add1

inter_med = ((an1 %*% mean1) + bn) / sqrt(t(an1_t1 * cov1)  %*% an1_t1)
inter_med1 = -((an1 %*% mean2) + bn) / sqrt(t(an1_t1 * cov1)  %*% an1_t1)

error[4] = 1/2 *(pnorm(inter_med) + pnorm(inter_med1))
error_lda = c(0.304,0.252,0.276,0.268)
sample1 = c(40,60,80,100)
plot(sample1,error_lda,type= 'l')
plot(sample1,error,type= 'l')



