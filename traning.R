library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(dplyr)

data <- read.table('/Users/duc/Downloads/Bai\ 1/Bài\ 1/junction_postvip_sum_6mon_enr/junction_postvip_sum_6mon_enr.txt', sep = '|')
dataMix <-data[sample(1:nrow(data)),]
dataMix<- dataMix[complete.cases(dataMix),]

dataMix$V1<-NULL
dataMix$V57<-NULL

for(i in c("allService","sumTimesUseService","sumFreeServiceFee","sum_A_charge","sum_B_charge","sum_B_times","sum_A_times","sum_total_use_A","sum_t_A_duration","sum_D_other_charge","sum_C_charge","sum_C_times","sum_E_times","sum_E_duration","sum_F_times","sum_F_charge","sum_E_charge","sum_G_times","sum_G_duration","sum_G_charge","sum_H_times","sum_H_charge","sum_M_volume","sum_M_times","sum_M_charge")){
  dataMix[,i] <- rowSums((dataMix[,2:6])/5)-dataMix[,1:1]
  dataMix[,1:6]<- NULL
}

LogModel <- glm(V153 ~ .,family=binomial(link="logit"),data=dataMix)