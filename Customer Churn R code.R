setwd("C:/Users/DELL/Desktop/Akshay/Group Assignments/Group Assignment 5 PM")
getwd()
cellphones<-read.csv("cellphones.csv",header = T)
summary(cellphones)
cellphones$Churn<-as.factor(cellphones$Churn)
cellphones$ContractRenewal<-as.factor(cellphones$ContractRenewal)
cellphones$DataPlan<-as.factor(cellphones$DataPlan)
str(cellphones)
attach(cellphones)
barplot(table(cellphones$Churn),main = "CHURN RATE",col="Blue",border="Red",density=50,ylim = c(0,3000),xlab="1 = Churned & 0 = Not churned")
summary(Churn)
#percentage of customer churned as per the data 
(483/(483+2850))*100
##Data indicates that 14.49% of customers have churned 
barplot(table(cellphones$ContractRenewal),main = "Contract Renewal Rate",density = 50,col="Orange",ylim = c(0,3500))
summary(ContractRenewal)
#percentage of customer who did not renewed their contract
(323/(3010+323))*100
##9.69% of customers did not renewed their contract
##Active data plan users
barplot(table(cellphones$DataPlan),main = "Active Data Plan users",density = 50,col="gold",ylim = c(0,2500))
summary(DataPlan)
(922/(922+2411))*100
##Only 27.66% customer are using the dataplans or have active data plans

##Check for the multicollinearity
numericdata<-cellphones[,c(-1,-3,-4)]
print(cor(numericdata),digits = 3)
library(corrplot)
corrplot(cor(numericdata))

##From the above corplot we can see that the variables monthly charge have a high positive corelation with data usage and daymins
boxplot(AccountWeeks,DayMins,DayCalls,MonthlyCharge,col = "orange")
plot(MonthlyCharge,DayMins)
##from the graphs it is evident that the monthlycharge increases with increase calling minutes

##Dividing the train & test data 70:30 ratio
set.seed(1104)
library(caTools)
split=sample.split(cellphones$Churn,SplitRatio = 0.70)
train.logit<-subset(cellphones,split==TRUE)
test.logit<-subset(cellphones,split==FALSE)

##Checking the data division 
dim(train.logit)
dim(test.logit)
summary(train.logit$Churn)
summary(test.logit$Churn)
##from the above it train & test we can see that both data have equal amount of churn rate ie 14% approx

##Backward Modelling - FUll model with all variables

logit<-glm(Churn~.,data=train.logit,family = binomial)
summary(logit)
library(car)
x<-vif(logit)
x
write.csv(x,"vif.csv")
library(lmtest)
lrtest(logit)
library(pscl)
pR2(logit)

##From the corelation plot and VIF we observed that the monthlycharge,DayMins & Datausage have a high VIF values and they are positively corelated 
##Hence modeling without those 3 variables

logit1<-glm(Churn~AccountWeeks+ContractRenewal+DataPlan+CustServCalls+DayCalls+OverageFee+RoamMins,data = train.logit,family = binomial())
summary(logit1)
vif(logit1)
lrtest(logit1)
pR2(logit1)
##Now we can see that the vif score for all variables are less than 4 hence the multicollinearity is well handled
##However though the VIF is passed in pseudo R test the Mcfaden score has drooped
##Hence now check with adding Daymins and cross checking the VIF score & pR2 test for validation
logit2<-glm(Churn~AccountWeeks+ContractRenewal+DayMins+DataPlan+CustServCalls+DayCalls+OverageFee+RoamMins,data = train.logit,family = binomial())
summary(logit2)
lrtest(logit2)
pR2(logit2)
vif(logit2)
odds<-exp(coef(logit2))
odds
write.csv(odds,"oddsbk.csv")
##From the above test we can now see that the Mcfaden test score has improved and now the VIF is in control too
##It is also passing the loglikelihood test since P-value is significant
# from this o/p (odd/1+odd)
##Checking the prediction accuracy on the test data 
predict.logit<-predict.glm(logit2, newdata=test.logit, type="response")
summary(predict.logit)

##Checking the accuracy of prediction through classification matrix

library(caret)
library(SDMTools)
library(pROC)
library(Hmisc)

table.logit<-confusion.matrix(test.logit$Churn,predict.logit,threshold = 0.5)
table.logit

##Accuracy of model 
ModelAccuracy<-(829+33)/(829+112+33+26)
ModelAccuracy

##The accuracy of model is 86.2% which is good

##ROC curve for test data
accuracy.logit<-
  roc.logit<-roc(test.logit$Churn,predict.logit)
roc.logit
plot(roc.logit,main="ROC Curve")

##From the output it is seen that the area under the curve is 86.2% percent which is good and correct classifcation is done

##KS score 
library(ROCR)
ks1<-prediction(predict.logit,test.logit$Churn)
perf_m3_train<-performance(ks1,"tpr","fpr")
ks_stats<-round(max(attr(perf_m3_train,'y.values')[[1]]-attr(perf_m3_train,'x.values')[[1]]),4)*100
ks_stats                   

## Since the KS stats is 52.01% which is >20% indicates good separation between 
##the cumulative good rate & bad rate

##Gini values for logistcs regression
install.packages("ineq")
library(ineq)
gini_stats=round(ineq(predict.logit,type="Gini"),4)*100
gini_stats

##The gini value for the model is 53.59 which is a fair fit 

##Hence by comparing all the validation models the output is as below and the model is fit to predict 
