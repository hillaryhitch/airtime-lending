# airtime-lending
###random forest algorithm with grid search for best parameters to predict good customers to lend realtime airtime

#Set working directory
setwd("C:/Users/hmwangila/Documents/Okoa Data")
#Import data (Pipe separated). Used read.delim function. Convert blanks to NA
okoa<-read.delim("okoa_data_model_2.csv",sep = "|",na.strings = c("","NA"))
#10% data
ind<-sample(2,nrow(okoa),replace=TRUE, prob = c(0.9,0.1))
okoa10<-okoa[ind==2,]
df1<-okoa10
df1$HANDSET_NAME<- ifelse(is.na(df1$HANDSET_NAME)==TRUE,0,df1$HANDSET_NAME)
df1$DUAL_SIM<- ifelse(is.na(df1$DUAL_SIM)==TRUE,0,df1$DUAL_SIM)
df1$SFC_CLASS<- ifelse(is.na(df1$SFC_CLASS)==TRUE,0,df1$SFC_CLASS)
#Replace missing values for call fields with zeros
df1$PROD_NU_201609<-ifelse(is.na(df1$PROD_NU_201609)==TRUE,0,df1$PROD_NU_201609)
df1$PROD_NU_201610<-ifelse(is.na(df1$PROD_NU_201610)==TRUE,0,df1$PROD_NU_201610)
df1$PROD_NU_201611<-ifelse(is.na(df1$PROD_NU_201611)==TRUE,0,df1$PROD_NU_201611)
df1$PROD_NU_201612<-ifelse(is.na(df1$PROD_NU_201612)==TRUE,0,df1$PROD_NU_201612)
df1$NU_SIMCARDS<-ifelse(is.na(df1$NU_SIMCARDS)==TRUE,1,df1$NU_SIMCARDS)
df1$NUMBER_CALLS_201609<-ifelse(is.na(df1$NUMBER_CALLS_201609)==TRUE,0,df1$NUMBER_CALLS_201609)
df1$NUMBER_CALLS_201610<-ifelse(is.na(df1$NUMBER_CALLS_201610)==TRUE,0,df1$NUMBER_CALLS_201610)
df1$NUMBER_CALLS_201611<-ifelse(is.na(df1$NUMBER_CALLS_201611)==TRUE,0,df1$NUMBER_CALLS_201611)
df1$NUMBER_CALLS_201612<-ifelse(is.na(df1$NUMBER_CALLS_201612)==TRUE,0,df1$NUMBER_CALLS_201612)
df2<-df1[!(df1$DEBT_AGE_201609==-7398|df1$DEBT_AGE_201610==-7367|df1$DEBT_AGE_201611==-7337|df1$DEBT_AGE_201612==-7306),]


str(df1)
View(df1)
df1$HANDSET_NAME<- ifelse(is.na(df1$HANDSET_NAME)==TRUE,0,df1$HANDSET_NAME)
df1$DUAL_SIM<- ifelse(is.na(df1$DUAL_SIM)==TRUE,0,df1$DUAL_SIM)
df1$SFC_CLASS<- ifelse(is.na(df1$SFC_CLASS)==TRUE,0,df1$SFC_CLASS)
df1$DUAL_SIM<-as.factor(df1$DUAL_SIM)

df1$Target<-ifelse(df1$DEBT_AGE_201701>30,1,0)
#REMOVE ALL NAS
install.packages("data.table")
library(data.table)
rem =function(DT) {
  # either of the following for loops

  # by name :
  for (j in names(DT))
    set(DT,which(is.na(DT[[j]])),j,0)

  # or by number (slightly faster than by name) :
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}

rem(df1)

    #Dev & Train
    ind<-sample(2,nrow(df1),replace=TRUE, prob = c(0.7,0.3))
    okoatrainingdata<-df1[ind==1,]
    okoaval<-df1[ind==2,]
View(okoatrainingdata)
str(okoatrainingdata)

library(ROCR)
library(randomForest)
okoatrainingdata$Target <- as.factor(okoatrainingdata$Target)
y<-okoatrainingdata[,c("Target")]
summary(okoatrainingdata)
ncol(okoatrainingdata)
x<-subset(okoatrainingdata,select=-c(1,2,Target,
    SFC_CLASS,HANDSET_NAME,
    DEBT_AGE_201612,
    DEBT_AGE_201611,
    DEBT_AGE_201610,
    DEBT_AGE_201609,
    DEBT_AGE_201701,
    FORCED_REPAYMENT_CNT_201612,
    FORCED_REPAYMENT_CNT_201611,
    FORCED_REPAYMENT_CNT_201610,
    FORCED_REPAYMENT_CNT_201609,
    FORCED_REPAYMENT_CNT_201701,
    X_ETU_BLACKLIST_FLAG,
    ALL_GROSS_AMT_201701,
    CNT_RCHG_201701,
    DOU_201701,
    LAST_OKOA_MNTH_201701,
    LAST_RCHG_MNTH_201701,
    MPESA_GROSS_201701,
    MPESA_QT_201701,
    NUMBER_CALLS_201701,
    NUMBER_CALLS_201702,
    OKOA_AMT_201701,
    OKOA_QT_201701,
    OKOA_REPAYMENT_CNT_201701,
    OKOA_REVN_201701,
    PROD_NU_201701,
    VL_BLNC_201701))

View(x)
mymodel2<-randomForest(x,y,ntree=100,mtry=10,importance=TRUE)
importance(mymodel2)
mymodel2
# DT Dev data stats
pred<-predict(mymodel2,okoatrainingdata,type="class")
table(okoatrainingdata$Target ,pred)

pred<-predict(mymodel2,okoaval,type="class")
table(okoaval$Target ,pred)

dev<-okoatrainingdata
val<-okoaval

results_df <- data.frame(matrix(ncol = 8))

colnames(results_df)[1]="No. of trees"
colnames(results_df)[2]="No. of variables"
colnames(results_df)[3]="Dev_AUC"
colnames(results_df)[4]="Dev_Hit_rate"
colnames(results_df)[5]="Dev_Coverage_rate"
colnames(results_df)[6]="Val_AUC"
colnames(results_df)[7]="Val_Hit_rate"
colnames(results_df)[8]="Val_Coverage_rate"


trees = c(50,100,150)
variables = c(8,10,12,15,18)

for(i in 1:length(trees))
{
  ntree = trees[i]
  for(j in 1:length(variables))
  {
    mtry = variables[j]
    rf<-randomForest(x,y,ntree=ntree,mtry=mtry)
    pred<-as.data.frame(predict(rf,type="class"))
    class_rf<-cbind(dev$Target,pred)

    colnames(class_rf)[1]<-"actual_values"
    colnames(class_rf)[2]<-"predicted_values"
    dev_hit_rate = nrow(subset(class_rf, actual_values ==1&predicted_values==1))/nrow(subset(class_rf, predicted_values ==1))
    dev_coverage_rate = nrow(subset(class_rf, actual_values ==1&predicted_values==1))/nrow(subset(class_rf, actual_values ==1))

    pred_prob<-as.data.frame(predict(rf,type="prob"))
    prob_rf<-cbind(dev$Target,pred_prob)
    colnames(prob_rf)[1]<-"target"
    colnames(prob_rf)[2]<-"prob_0"
    colnames(prob_rf)[3]<-"prob_1"
    pred<-prediction(prob_rf$prob_1,prob_rf$target)
    auc <- performance(pred,"auc")
    dev_auc<-as.numeric(auc@y.values)

    pred<-as.data.frame(predict(rf,val,type="class"))
    class_rf<-cbind(val$Target,pred)

    colnames(class_rf)[1]<-"actual_values"
    colnames(class_rf)[2]<-"predicted_values"
    val_hit_rate = nrow(subset(class_rf, actual_values ==1&predicted_values==1))/nrow(subset(class_rf, predicted_values ==1))
    val_coverage_rate = nrow(subset(class_rf, actual_values ==1&predicted_values==1))/nrow(subset(class_rf, actual_values ==1))

    pred_prob<-as.data.frame(predict(rf,val,type="prob"))
    prob_rf<-cbind(val$Target,pred_prob)
    colnames(prob_rf)[1]<-"target"
    colnames(prob_rf)[2]<-"prob_0"
    colnames(prob_rf)[3]<-"prob_1"
    pred<-prediction(prob_rf$prob_1,prob_rf$target)
    auc <- performance(pred,"auc")
    val_auc<-as.numeric(auc@y.values)
    results_df = rbind(results_df,c(ntree,mtry,dev_auc,dev_hit_rate,dev_coverage_rate,val_auc,val_hit_rate,val_coverage_rate))
  }
}

pred<-predict(mymodel2 ,okoaval,type="class")
table(okoaval$Target ,pred)
summary(mymodel2)
  ls(x)

varImpPlot(mymodel2)
write.table(results_df, "C:/Users/hmwangila/Documents//okoa Data/RF_results_11trees.txt", sep=",")
