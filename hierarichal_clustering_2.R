library(MASS)
library(ggplot2)
library(GGally)

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


####Question 1-a####
SFE_dataset_numeric=SFE_dataset_clean[,1:7]
prin_comp_SFE=prcomp(SFE_dataset_numeric,scale. = TRUE)
prin_comp_SFE_new=princomp(SFE_dataset_numeric)


####Question 1-b####
prin_comp_SFE_var=prin_comp_SFE$sdev*prin_comp_SFE$sdev
prin_comp_SFE_var_per=100*prin_comp_SFE_var/sum(prin_comp_SFE_var)
prin_comp_SFE_var_df=data.frame(prin_comp_SFE_var_per)
prin_comp_SFE_var_df$pc_num=c(1,2,3,4,5,6,7)

plot_q2=ggplot(prin_comp_SFE_var_df, aes(y = prin_comp_SFE_var_per , x = pc_num ))
plot_q2=plot_q2+geom_line(size=1.5,color="red")+geom_point(size=1.5,color="blue")
plot_q2=plot_q2+xlab("PC_Index")+ylab("variance Percentage")


###Question 1-c######

prin_comp_SFE1=prin_comp_SFE$x[,1:3]
cols <- character(nrow(prin_comp_SFE1))


for(i in 1:length(SFE_dataset_clean[,1])){
  if(SFE_dataset_clean$SFE_level[i]=="high"){
    cols[i]="red"
  }
  else if(SFE_dataset_clean$SFE_level[i]=="medium"){
    cols[i]="blue"
  }
  else{
    cols[i]="green"
  }
}

prin_comp_SFE2 = prin_comp_SFE$x[,1:7]
pairs(prin_comp_SFE2,col = cols)
pairs(prin_comp_SFE1,col=cols)
prin_comp_SFE1=data.frame(prin_comp_SFE1)
prin_comp_SFE1$colors=SFE_dataset_clean$SFE_level

plot_q3=ggpairs(data = prin_comp_SFE1,columns = 1:3, ggplot2::aes(colour=colors))


####Question-1-d ####

rotation_matrix = prin_comp_SFE$rotation


