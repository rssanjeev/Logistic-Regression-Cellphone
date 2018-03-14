
#Logistic Regression Assignment
celldata<-read.csv(file.choose())
str(celldata)
table(celldata$Churn)
missdata<-na.omit(celldata)
str(missdata)
summary(celldata)

#splitting the dataset
set.seed(100)
desired_rows<-sample(3333,2333)
Train_1<-celldata[desired_rows,]
Valid_1<-celldata[-desired_rows,]


str(Train_1)

#checking the num variables
summary(Train_1)
hist (Train_1$DataUsage)
install.packages("ggplot2")
library(ggplot2)
#install.packages("colorspace")
#install.packages("C:/Users/448571/Downloads/colorspace_1.3-2.tar.gz")

ggplot(Train_1,aes(x=DataUsage, fill=Churn)) + geom_histogram()
ggplot(Train_1,aes(x=AccountWeeks, fill=Churn)) + geom_histogram() 


iv<-function(predit,target,details) 
{
  data<-data.frame(predit,target);
  data_sort<-data[order(predit),]
  
  temp_str <- c("Bin", "Good_pct", "Bad_pct", "IV_Bin")
  ifelse((details=="Yes"),print(temp_str),"")
  
  ttl_num<-length(target);
  bin<-10;
  n<-ttl_num%/%bin;
  iv_bin<-rep(0,times=bin);
  good<-rep(0,times=bin);
  bad<-rep(0,times=bin);
  for (i in 1:bin) # calculate PSI for ith bin
  {
    if(i!=bin) {good[i]<-sum(data_sort$target[((i-1)*n+1):(n*i)]);bad[i]<-n-good[i]} else
    {good[i]<-sum(data_sort$target[((i-1)*n+1):ttl_num]);bad[i]<-ttl_num-n*(i-1)-good[i]}
  }
  
  good_pct<-good/sum(good)
  bad_pct<-bad/sum(bad)
  
  for (i in 1:bin)
  {
    iv_bin[i]<-(bad_pct[i]-good_pct[i])*log(bad_pct[i]/good_pct[i])
    
    temp1=    c(round(i,0), round(good_pct[i],2),round(bad_pct[i],2)
                ,round(iv_bin[i],2))
    ifelse((details=="Yes"),print(temp1),"")
    
    
  }
  IV_Data <- read.table(textConnection(
    temp_str), header = TRUE) 
  
  head(IV_Data,10)
  iv=sum(iv_bin)
  return (iv)
}

str(Train_1)
Train_1$Churn<-as.numeric(Train_1$Churn)
Train_1$ContractRenewal<-as.numeric(Train_1$ContractRenewal)
Train_1$CustServCalls<-as.numeric(Train_1$CustServCalls)
Train_1$DataPlan<-as.numeric(Train_1$DataPlan)

train_new$ContractRenewal
str(train_new)
str(Train_2)
table(train_new$ContractRenewal)

iv(Train_1$AccountWeeks,Train_1$Churn,"No")
iv(Train_1$ContractRenewal,Train_1$Churn,"No")
iv(Train_1$DataUsage,Train_1$Churn,"No") 
iv(Train_1$CustServCalls,Train_1$Churn,"No")
iv(Train_1$DayMins,Train_1$Churn,"No")
iv(Train_1$MonthlyCharge,Train_1$Churn,"No")
iv(Train_1$OverageFee,Train_1$Churn,"No")
iv(Train_1$DayCalls,Train_1$Churn,"No")

#removing Accountweeks, Datausage, Overagefee, Daycalls
#removing variables
Train_2<-Train_1[c(-2,-5,-8,-10)]
str(Train_2)


#generate indicator variables
table(Train_2$ContractRenewal,Train_2$Churn)
table(Train_2$CustServCalls,Train_2$Churn)
hist(Train_2$CustServCalls)
table(Train_2$DayMins,Train_2$Churn)
table(Train_2$MonthlyCharge,Train_2$Churn)
table(Train_2$RoamMins,Train_2$Churn)
table(Train_2$DataPlan,Train_2$Churn)

str(Train_2)

#indiacator for Contract renewal
Train_2$ContractRenewal_1<-ifelse(Train_2$ContractRenewal=='1' ,1 , 0)
#Train_2$Dataplan_1<-ifelse(Train_2$da=='1' ,1 , 0)

#indicator for dataplan
Train_2$DataPlan_1<-ifelse(Train_2$DataPlan==2,1, 0)

#indicator for custsercalls (including the higher end)
Train_2$CustServCallslt2<-ifelse(Train_2$CustServCalls<=2,1,0)
Train_2$CustServCalls2_4<-ifelse(Train_2$CustServCalls>2&Train_2$CustServCalls<=4,1,0)
Train_2$CustServCalls4_6<-ifelse(Train_2$CustServCalls>4&Train_2$CustServCalls<=6,1,0)
Train_2$CustServCalls6_8<-ifelse(Train_2$CustServCalls>6&Train_2$CustServCalls<=8,1,0)
Train_2$CustServCalls8_10<-ifelse(Train_2$CustServCalls>8&Train_2$CustServCalls<=10,1,0)

#indicator for Daymins
summary(Train_2$DayMins)
Train_2$Dayminslt70<-ifelse(Train_2$DayMins<=70,1,0)
Train_2$Daymins70_140<-ifelse(Train_2$DayMins>70&Train_2$DayMins<=140,1,0)
Train_2$Daymins140_210<-ifelse(Train_2$DayMins>140&Train_2$DayMins<=210,1,0)
Train_2$Daymins210_280<-ifelse(Train_2$DayMins>210&Train_2$DayMins<=280,1,0)
Train_2$Dayminsgt280<-ifelse(Train_2$DayMins>280,1,0)

#indicator for Monthly Charge
summary(Train_2$MonthlyCharge)
Train_2$MonthlyChargelt25<-ifelse(Train_2$MonthlyCharge<=25,1,0)
Train_2$MonthlyCharge25_50<-ifelse(Train_2$MonthlyCharge>25&Train_2$MonthlyCharge<=50,1,0)
Train_2$MonthlyCharge50_75<-ifelse(Train_2$MonthlyCharge>50&Train_2$MonthlyCharge<=75,1,0)
Train_2$MonthlyCharge75_100<-ifelse(Train_2$MonthlyCharge>75&Train_2$MonthlyCharge<=100,1,0)
Train_2$MonthlyChargegt100<-ifelse(Train_2$MonthlyCharge>100,1,0)

#incator for roammins
summary(Train_2$RoamMins)
Train_2$Roamminslt5<-ifelse(Train_2$RoamMins<=5,1,0)
Train_2$Roammins5_10<-ifelse(Train_2$RoamMins>5&Train_2$RoamMins<=10,1,0)
Train_2$Roammins10_15<-ifelse(Train_2$RoamMins>10&Train_2$RoamMins<=15,1,0)
Train_2$Roammins15_20<-ifelse(Train_2$RoamMins>15&Train_2$RoamMins<=20,1,0)

Train_3<-Train_2[c(-2,-3,-4,-5,-6,-7)]
Train_3$Churn<-ifelse(Train_3$Churn==2,1,0)
#train_4$DataPlan_1<-as.numeric(train_4$DataPlan_1)
#train_4$Churn<-ifelse(train_4$Churn==2,1,0)
str(Train_3)

#Stepwise regression
full_model<-glm(Churn~.,data =Train_3, family=binomial)
summary(full_model)
no_model<-glm(Churn~1,data =Train_3, family=binomial)
summary(no_model)

logistic_steps = step(no_model,
                      scope=list(lower=formula(no_model),
                                 upper=formula(full_model)),direction="both",
                      family=binomial)

select_model_1<-glm(Churn ~ CustServCalls4_6 + ContractRenewal_1 + Dayminsgt280 + Daymins140_210 + CustServCalls6_8 +DataPlan_1,data=Train_3,
                  family=binomial)

summary(select_model_1)

#checking for multi-collinearity
#install.packages("car")
library(car)
vif(select_model_1)

Train_3$predicted_val<-predict(select_model_1)
hist(Train_3$predicted_val)
Train_3$prob<-exp(Train_3$predicted_val)/(1+exp(Train_3$predicted_val))
hist(Train_3$prob)
Train_3$p1000=round(1000*Train_3$prob)
hist(Train_3$p1000)
Train_3$Churn_1<-ifelse(Train_3$Churn==1,"Y","N")
ggplot(Train_3, aes(x=p1000, fill=Churn_1)) + geom_histogram()

# Generate Rank in the data based on p1000
temp1<-na.omit(Train_3)
Train_3$p_rank_tmp=rank(x = Train_3$p1000,na.last =FALSE,
                        ties.method = c("average"))

summary(Train_3$p_rank_tmp)
Train_3$p_rank <- floor(Train_3$p_rank_tmp/233)
summary(Train_3$p_rank)
table(Train_3$p_rank, Train_3$Churn_1)
Train_3$p_rank<-ifelse(Train_3$p_rank>9,9,
                       Train_3$p_rank)
table(Train_3$p_rank, Train_3$Churn_1)
Train_3$cnt <-1
summary(Train_3$p_rank)


#KS Calculation
library("plyr")
ddply(Train_3, "p_rank",
      summarize, min_scr=min(p1000),
      max_scr=max(p1000), No_of_bad=sum(Churn), No_of_acct=sum(cnt)
) 

#coeff stab check
str(Valid_1)
Valid_2<-Valid_1
str(Valid_2)
Valid_2$Churn<-as.numeric(Valid_2$Churn)
Valid_2$ContractRenewal<-as.numeric(Valid_2$ContractRenewal)
Valid_2$CustServCalls<-as.numeric(Valid_2$CustServCalls)
Valid_2$DataPlan<-as.numeric(Valid_2$DataPlan)

Valid_2$Churn<-ifelse(Valid_2$Churn==2,1,0)
Valid_2$CustServCalls4_6<-ifelse(Valid_2$CustServCalls>4&Valid_2$CustServCalls<=6,1,0)
Valid_2$Dayminsgt280<-ifelse(Valid_2$DayMins>280,1,0)
Valid_2$ContractRenewal_1<-ifelse(Valid_2$ContractRenewal==2,1 , 0)

Valid_2<-Valid_2[c(-2,-3,-4,-5,-6,-7,-8,-9,-10,-11)]

Validation_model<-glm(Churn ~ CustServCalls4_6 + ContractRenewal_1 + Dayminsgt280 + Daymins140_210 + CustServCalls6_8 +DataPlan_1,data=Train_3,
                      family=binomial)
summary(Validation_model)
vif(Validation_model)
