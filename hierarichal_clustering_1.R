library(MASS)
library(ggplot2)
library(GGally)
library(ggdendro)

SFE_dataset <- read.csv(file="/home/nbansal/Desktop/MTLS/PROJECT-3/SFE_Dataset.csv",head=TRUE,sep=",")
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


for (i in 1:length(SFE_dataset$SFE)){
  if(SFE_dataset$SFE[i]<=35){
    SFE_dataset$SFE_level[i]="low"
  }
  else if(SFE_dataset$SFE[i]>35 && SFE_dataset$SFE[i]<45){
    SFE_dataset$SFE_level[i]="medium"
  }
  else if(SFE_dataset$SFE[i]>=45){
    SFE_dataset$SFE_level[i]="high"
  }
}

SFE_dataset_clean = SFE_dataset
SFE_dataset_clean$SFE_level=factor(SFE_dataset_clean$SFE_level)

####Question 2-a###
a1 = SFE_dataset_clean$SFE_level
hc_avg <- hclust(dist(SFE_dataset_clean[,1:7]), "average")
hc_complete <- hclust(dist(SFE_dataset_clean[,1:7]), "complete")
hc_single <- hclust(dist(SFE_dataset_clean[,1:7]), "single")

####Ploting the clusters Obtained ######
p_single<- plot(hc_single, label = a1,xlab = 'SFE_DATASET')
p_complete<- plot(hc_complete, label = a1,xlab = 'SFE_DATASET')
p_avg<-plot(hc_avg, label = a1,xlab = 'SFE_DATASET')


###Making a Cut for a cluster size of 3####
###Single Linkage####
groups <- cutree(hc_single, k=3)
r_single<-rect.hclust(hc_single, k=3, border="red") 

###Complete Linkage####
groups <- cutree(hc_complete, k=3)
r_complete<-rect.hclust(hc_complete, k=3, border="green")

###Single Linkage####
groups <- cutree(hc_avg, k=3)
r_avg<-rect.hclust(hc_avg, k=3, border="blue")





