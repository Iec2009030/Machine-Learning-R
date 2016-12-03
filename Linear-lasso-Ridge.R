library(MASS)
library(ggplot2)
library(glmnet)
library(class)

SFE_dataset <- read.csv(file="/home/nbansal/Desktop/MTLS/PROJECT-4/SFE_Dataset.csv",head=TRUE,sep=",")
SFE_dataset_original=SFE_dataset
#removing the non-informative columns

## This for removing all the Collumns with 40% or More Zeros
i <- 0
length_datasheet <- length(SFE_dataset)
num <-c(1:length_datasheet)
#data <- temp 
temp <- SFE_dataset
for (col in num) {
  a <- SFE_dataset[,col]
  p = length(a)
  m <- a == 0
  d<- a[m]
  if (length(d) >= 0.4*p) {
    temp <- temp[,-(col-i)]  
    i <- i+1
  }
}

## This is for removing all the rows with Zeros in them
i<-0
num_row <- length(temp[,1])
num_row <- c(1:num_row)
SFE_dataset <- temp
for (row in num_row) {
  a <- temp[row,]
  p = length(a)
  m <- a == 0
  d <- a[m]
  if (length(d) > 0) {
    SFE_dataset <- SFE_dataset[-(row-i),]
    i <- i+1
  }
}
SFE_dataset_pred = SFE_dataset[,c(1,2,3,4,5,6,7)]
SFE_dataset_resp = as.vector(SFE_dataset[,8])

##Question 1 ####
SFE_dataset_pred_C = as.matrix(SFE_dataset_pred[,1])
SFE_dataset_pred_N = as.matrix(SFE_dataset_pred[,2])
SFE_dataset_pred_Ni = as.matrix(SFE_dataset_pred[,3])
SFE_dataset_pred_Fe = as.matrix(SFE_dataset_pred[,4])
SFE_dataset_pred_Mn = as.matrix(SFE_dataset_pred[,5])
SFE_dataset_pred_Si = as.matrix(SFE_dataset_pred[,6])
SFE_dataset_pred_Cr = as.matrix(SFE_dataset_pred[,7])

###Fitting the Model####
list1 = list()
mod1 = lm(SFE ~ C, data = SFE_dataset)
mod2 = lm(SFE ~ N, data = SFE_dataset)
mod3 = lm(SFE ~ Ni, data = SFE_dataset)
mod4 = lm(SFE ~ Fe, data = SFE_dataset)
mod5 = lm(SFE ~ Mn, data = SFE_dataset)
mod6 = lm(SFE ~ Si, data = SFE_dataset)
mod7 = lm(SFE ~ Cr, data = SFE_dataset)
##coefficients(mod1)
###summary(mod1)
##Storing the output as List as the Output of summary(mod1) is of type list#####
list1[[1]] = summary(mod1)
list1[[2]] = summary(mod2)
list1[[3]] = summary(mod3)
list1[[4]] = summary(mod4)
list1[[5]] = summary(mod5)
list1[[6]] = summary(mod6)
list1[[7]] = summary(mod7)

len = length(list1)
maxi = 0
q2 = 0
##Compare the R2 Statitics########################
##q2 contains the element with highest R2 value###
for (i in c(1:len)) {
  q1 = as.numeric(list1[[i]][8])
  if (q1 > maxi){
    maxi = q1
    q2 = i
  }
}

#### b part ###### 
pt<-plot(SFE ~ C, data = SFE_dataset,
       xlab = " Individual Elements(C)",
       ylab = "Response SFE",
       main = "Linear Regression", col = "green")
a = as.data.frame(coefficients(mod1))
intercept = a[1,1]
coeff = a[2,1]
pt <- pt+ abline(intercept,coeff,col = "red")


pt<-plot(SFE ~ N, data = SFE_dataset,
         xlab = " Individual Elements(N)",
         ylab = "Response SFE",
         main = "Linear Regression", col = "green")
a = as.data.frame(coefficients(mod2))
intercept = a[1,1]
coeff = a[2,1]
pt <- pt+ abline(intercept,coeff,col = "red")


pt<-plot(SFE ~ Ni, data = SFE_dataset,
         xlab = " Individual Elements(Ni)",
         ylab = "Response SFE",
         main = "Linear Regression", col = "green")
a = as.data.frame(coefficients(mod3))
intercept = a[1,1]
coeff = a[2,1]
pt <- pt+ abline(intercept,coeff,col = "red")

pt<-plot(SFE ~ Fe, data = SFE_dataset,
         xlab = " Individual Elements(Fe)",
         ylab = "Response SFE",
         main = "Linear Regression", col = "green")
a = as.data.frame(coefficients(mod4))
intercept_Fe = a[1,1]
coeff_Fe = a[2,1]
pt <- pt+ abline(intercept_Fe,coeff_Fe,col = "red")

pt<-plot(SFE ~ Mn, data = SFE_dataset,
         xlab = " Individual Elements(Mn)",
         ylab = "Response SFE",
         main = "Linear Regression", col = "green")
a = as.data.frame(coefficients(mod5))
intercept = a[1,1]
coeff = a[2,1]
pt <- pt+ abline(intercept,coeff,col = "red")


pt<-plot(SFE ~ Si, data = SFE_dataset,
         xlab = " Individual Elements(Si)",
         ylab = "Response SFE",
         main = "Linear Regression", col = "green")
a = as.data.frame(coefficients(mod6))
intercept = a[1,1]
coeff = a[2,1]
pt <- pt+ abline(intercept,coeff,col = "red")


pt<-plot(SFE ~ Cr, data = SFE_dataset,
         xlab = " Individual Elements(Cr)",
         ylab = "Response SFE",
         main = "Linear Regression", col = "green")
a = as.data.frame(coefficients(mod7))
intercept = a[1,1]
coeff = a[2,1]
pt <- pt+ abline(intercept,coeff,col = "red")


######Quesion-2################
######Exhaustive Approach #####
print ("Exhaustive Approach")
data_names = names(SFE_dataset)
print (data_names[3])
col_name <- c(1,2,3,4,5,6,7)

for(i in c(2:5)){
  sel_matrix = combn(col_name, i)
  sel_col = ncol(sel_matrix)
  max12 = 0
  q22 = 0
  square = 0
  for (j in c(1:sel_col)) {
    col = as.vector(sel_matrix[,j])
    l = length(col)
    vec <- vector("numeric",length(l))
    for (k in c(1:l)) {
      vec[k] = data_names[col[k]]
    }
    b = paste(vec, collapse ="+")
    c = paste("SFE ~ ",b,sep = "")
    c = as.formula(c)
    mod = lm(c, data = SFE_dataset)
    a11 = list()
    a11 = summary(mod)
    square = as.numeric(a11[8])
    if (square > max12) {
      max12 = square
      for (k in c(1:l)) {
          q22[k] = vec[k]
      }
    }
  }
  print (q22)
}

######Sequential Forward Search ########
########################################

col_name <- c(1,2,4,5,6,7)
###We know from earlier result Element 3 is has the highest R^2 value###
###       The Element is "Ni"          #################################
data_names = names(SFE_dataset)
vec_seq <- vector("numeric",5)
vec_seq[1] = data_names[3]

for (i in c(2:5)) {
  list2 = list()
  max2 = 0
  q2 = 0
  for(j in c(1:length(col_name))){
    vec_seq[i] = data_names[col_name[j]]
    b = paste(vec_seq, collapse ="+")
    c1 = paste("SFE ~ ",b,sep = "")
    c1 = as.formula(c1)
    mod = lm(c1, data = SFE_dataset)
    list2[[j]] = summary(mod)
  }
  #finding the Maximum R^2 Sqaured Combination
  temp = length(list2)
  for (k in c(1:temp)) {
    q1 = as.numeric(list2[[k]][8])
    if (q1 > max2){
      max2 = q1
      q2 = k
    }
  }
  #storing the best combination in vec_seq###
  ##Removing the element from "Choosing Lot"
  vec_seq[i]=data_names[col_name[q2]]
  #print (vec_seq)
  col_name = col_name[-q2]
  #print(col_name)
}

#Elements chosen after sequential search ####
######  "Ni" "N"  "Fe" "Si" "Cr"  ################

########Question:3#####################
#######################################
SFE_matrix = data.matrix(SFE_dataset_pred)
SFE_resp = SFE_dataset_resp
lambda1 = c(50,30,15,7,3,1,0.30,0.10,0.03,0.01)
fit_lasso = glmnet(SFE_matrix,SFE_resp,standardize = FALSE, lambda = lambda1)
fit_ridge = glmnet(SFE_matrix,SFE_resp,standardize = FALSE, lambda = lambda1, alpha = 0)

######Plotting Graph########
pt1<-plot(fit_ridge,main = "Ridge Regression", xlab = "L2 Norm")
pt2<-plot(fit_lasso,main = "Lasso Regression")

####Plotiing Graph 3-b #############
ptu<-plot(SFE ~ Fe, data = SFE_dataset,
          xlab = " Individual Elements(Fe)",
          ylab = "Response SFE",
          main = "Linear vs Lasso", col = "green")
t1 <- ptu + abline(intercept_Fe,coeff_Fe,col = "red")
t1 <- t1 + abline(74.018,-0.5298,col = "blue")

#Lasso
#Df   %Dev Lambda
#[1,]  0 0.0000  50.00
#[2,]  1 0.2433  30.00
#[3,]  2 0.4477  15.00
#[4,]  2 0.5155   7.00
#[5,]  3 0.5355   3.00
#[6,]  4 0.5856   1.00
#[7,]  5 0.6310   0.30
#[8,]  7 0.6639   0.10
#[9,]  7 0.6824   0.03
#[10,]  7 0.6840   0.01

#Ridge
#Df   %Dev Lambda
#[1,]  7 0.5511  50.00
#[2,]  7 0.5627  30.00
#[3,]  7 0.5764  15.00
#[4,]  7 0.5937   7.00
#[5,]  7 0.6163   3.00
#[6,]  7 0.6418   1.00
#[7,]  7 0.6597   0.30
#[8,]  7 0.6727   0.10
#[9,]  7 0.6814   0.03
#[10,]  7 0.6837   0.01

#coef(fit,s=50)
#8 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept) 39.06258
#C            .      
#N            .      
#Ni           .      
#Fe           .      
#Mn           .      
#Si           .      
#Cr           .      

#coef(fit,s=30)
#8 x 1 sparse Matrix of class "dgCMatrix"
#1
#(Intercept)  7.401824e+01
#C            .           
#N            .           
#Ni           2.265515e-16
#Fe          -5.298039e-01
#Mn           .           
#Si           .           
#Cr           .



