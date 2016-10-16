library(MASS)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
data <- read.csv(file="/home/nbansal/Desktop/MTLS/SFE_Dataset.csv",head=TRUE,sep=",")

#Remove entry with SFE value more  than 35 and less than 45
num_row <- nrow(data)
num_row <- c(1:num_row)
i <- 0
temp <- data
u = data$SFE > 35 & data$SFE < 45
for (row in num_row) {
  if (u[row] == TRUE) {
    temp <- temp[-(row-i),]
    i <-i+1
  }
}

## This for removing all the Collumns with 40% or More Zeros
i <- 0
length_datasheet <- length(data)
num <-c(1:length_datasheet)
data <- temp 
temp <- data
for (col in num) {
  a <- data[,col]
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
data <- temp
for (row in num_row) {
  a <- temp[row,]
  p = length(a)
  m <- a == 0
  d <- a[m]
  if (length(d) > 0) {
    data <- data[-(row-i),]
    i <- i+1
  }
}

##Selecting 70% data for Training and 30% for Testing
##indexes = sample(1:nrow(data), size=0.2*nrow(data))
##training_set = data[indexes,]
##test_set = data[-indexes,]
div = floor(nrow(data)*0.2)
indexes = c(1:div)
training_set = data[indexes,]
test_set = data[-indexes,]

########### Preprocessing is complete ##########################

welch_test1<-t.test(training_set[,1],training_set$SFE)
welch_test2<-t.test(training_set[,2],training_set$SFE)
welch_test3<-t.test(training_set[,3],training_set$SFE)
welch_test4<-t.test(training_set[,4],training_set$SFE)
welch_test5<-t.test(training_set[,5],training_set$SFE)

welch_test_list <- list(welch_test1,welch_test2,welch_test3,welch_test4,welch_test5)
t_statistics <- c(welch_test1$statistic, welch_test2$statistic, welch_test3$statistic, welch_test4$statistic,welch_test5$statistic)

t_stat_matrix <- matrix(t_statistics, ncol = 5)
name <- names(training_set)
colnames(t_stat_matrix) = name[1:5]
rownames(t_stat_matrix) = "T_STAT"
order_def <- c(1:5)
t<-sort(t_stat_matrix)[order_def]
predictor = NULL
for (i in c(1:2)){
  for (j in c(1:5)) {
    if (t[i] == t_stat_matrix[j])
      predictor[i] = j
  }
}

##Now we have the collumn number of top two predictors in predictor variable
##and data in training and test set respectively.

#Constructing 2 CLass namely "high" and "Low" for Training set

training_set["Class"] <- NA # Adding a New Collumn to Data Frame Training Set
training_temp = NULL
size = nrow(training_set)
SFE_value <-training_set$SFE
for ( i in c(1:size)) {
  if (SFE_value[i] <= 35){
    training_temp[i] = "low"
  }
  if(SFE_value[i] > 45) {
    training_temp[i] = "high"
  }
}

training_temp = factor(training_temp)
training_set$Class = training_temp

#Constructing 2 CLass namely "high" and "Low" for Test set
test_set["Class"] <- NA # Adding a New COllumn to Data Frame Training Set
training_temp = NULL
size = nrow(test_set)
SFE_value <-test_set$SFE
for ( i in c(1:size)) {
  if (SFE_value[i] <= 35){
    training_temp[i] = "low"
  }
  if(SFE_value[i] > 45) {
    training_temp[i] = "high"
  }
}

training_temp = factor(training_temp)
test_set$Class = training_temp

ncol_train = ncol(training_set)
row_name = names(training_set)
#plot(training_set[,c(predictor[1],predictor[2])], col=training_set[,ncol_train])

#Building the model and testing it!!
training_set.lda <- lda(Class ~ C + Mn, data = training_set,prior=c(0.5,0.5))
testing_set.lda.p <- predict(training_set.lda, newdata = test_set[,c(1,4)])$class

table_1_predictors=table(testing_set.lda.p,test_set$Class)
coeff = training_set.lda $scaling

#Calculating Slope and Intercept
gmean <- training_set.lda$prior%*%training_set.lda$means
const <- drop(gmean%*%training_set.lda$scaling)
slope1 <- -training_set.lda$scaling[1]/training_set.lda$scaling[2]
intercept1 <- const/training_set.lda$scaling[2]

#Plotting the Graph
pt1<-ggplot(training_set, aes(C, Mn,colour = Class)) + geom_point()
total_pt1<- pt1 + geom_abline(intercept = intercept1,slope = slope1,color="pink",size=2)

####Repeating the Process for top 3 of Predictors
training_set.lda <- lda(Class ~ C + Mn + Fe, data = training_set,prior=c(0.5,0.5))
testing_set.lda.p <- predict(training_set.lda, newdata = test_set[,c(1,3,4)])$class

table_2_predictors=table(testing_set.lda.p,test_set$Class)
coeff = training_set.lda $scaling

#Calculating Slope and Intercept
gmean <- training_set.lda$prior%*%training_set.lda$means
const <- drop(gmean%*%training_set.lda$scaling)
slope1 <- -training_set.lda$scaling[1]/training_set.lda$scaling[2]
intercept1 <- const/training_set.lda$scaling[2]

#Plotting the Graph
pt2<-ggplot(training_set, aes(C, Mn, Fe,colour = Class)) + geom_point()
total_pt2<- pt2 + geom_abline(intercept = intercept1,slope = slope1,color="pink",size=2)

####Repeating the Process for top 4 of Predictors
training_set.lda <- lda(Class ~ C + Mn + Fe + Ni, data = training_set,prior=c(0.5,0.5))
testing_set.lda.p <- predict(training_set.lda, newdata = test_set[,c(1,2,3,4)])$class

table_3_predictors=table(testing_set.lda.p,test_set$Class)
coeff = training_set.lda $scaling

#Calculating Slope and Intercept
gmean <- training_set.lda$prior%*%training_set.lda$means
const <- drop(gmean%*%training_set.lda$scaling)
slope1 <- -training_set.lda$scaling[1]/training_set.lda$scaling[2]
intercept1 <- const/training_set.lda$scaling[2]

#Plotting the Graph
pt3<-ggplot(training_set, aes(C, Mn, Fe,Ni,colour = Class)) + geom_point()
total_pt3<- pt3 + geom_abline(intercept = intercept1,slope = slope1,color="pink",size=2)

####Repeating the Process for top 5 of Predictors
training_set.lda <- lda(Class ~ C + Mn + Fe + Ni + Cr, data = training_set,prior=c(0.5,0.5))
testing_set.lda.p <- predict(training_set.lda, newdata = test_set[,c(1,2,3,4,5)])$class

table_4_predictors=table(testing_set.lda.p,test_set$Class)
coeff = training_set.lda $scaling

#Calculating Slope and Intercept
gmean <- training_set.lda$prior%*%training_set.lda$means
const <- drop(gmean%*%training_set.lda$scaling)
slope1 <- -training_set.lda$scaling[1]/training_set.lda$scaling[2]
intercept1 <- const/training_set.lda$scaling[2]

#Plotting the Graph
pt4<-ggplot(training_set, aes(C, Mn, Fe,Ni,Cr,colour = Class)) + geom_point()
total_pt4<- pt4 + geom_abline(intercept = intercept1,slope = slope1,color="pink",size=2)
