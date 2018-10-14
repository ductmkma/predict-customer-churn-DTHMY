dataTest <- read.table('/Users/duc/Downloads/Test\ 2.txt', sep = "|")
dataTest <- dataTest[complete.cases(dataTest),]

dataTest$V1<-NULL
dataTest$V57<-NULL

for(i in c("allService","sumTimesUseService","sumFreeServiceFee","sum_A_charge","sum_B_charge","sum_B_times","sum_A_times","sum_total_use_A","sum_t_A_duration","sum_D_other_charge","sum_C_charge","sum_C_times","sum_E_times","sum_E_duration","sum_F_times","sum_F_charge","sum_E_charge","sum_G_times","sum_G_duration","sum_G_charge","sum_H_times","sum_H_charge","sum_M_volume","sum_M_times","sum_M_charge")){
  dataTest[,i] <- rowSums((dataTest[,2:6])/5)-dataTest[,1:1]
  dataTest[,1:6]<- NULL
}

#du doan tap testing
fitted.results <- predict(LogModel,newdata=dataTest,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != dataMix$V153)


print(paste('Logistic Regression Accuracy',1-misClasificError))

