getwd()
setwd("C:/Users/drish/downloads")
MOTOR=read.csv("car_insurclaim.csv",header = TRUE,stringsAsFactors = FALSE)
MOTOR
##CLEANING DATA
#converting yes=1 and no=0 in parent1,Mstatus(z_no=0),gender(z_F=1,M=o),CAR_TYPE(PRIVATE=1,COMMERCIAL=0),
#RED_CAR,REVOKED,URBANICITY
MOTOR$PARENT1=ifelse(MOTOR$PARENT1=="Yes",1,0)
MOTOR$MSTATUS=ifelse(MOTOR$MSTATUS=="Yes",1,0)
MOTOR$GENDER=ifelse(MOTOR$GENDER=="z_F",1,0)
MOTOR$CAR_USE=ifelse(MOTOR$CAR_USE=="Private",1,0)
MOTOR$RED_CAR=ifelse(MOTOR$RED_CAR=="yes",1,0)
MOTOR$REVOKED=ifelse(MOTOR$REVOKED=="Yes",1,0)
MOTOR$URBANICITY=ifelse(MOTOR$URBANICITY=="Highly Urban/ Urban",1,0)

#REMOVING $ SIGNS
MOTOR$INCOME = as.numeric(gsub("[\\$,]", "", MOTOR$INCOME))
MOTOR$HOME_VAL = as.numeric(gsub("[\\$,]", "", MOTOR$HOME_VAL))
MOTOR$BLUEBOOK = as.numeric(gsub("[\\$,]", "", MOTOR$BLUEBOOK))
MOTOR$OLDCLAIM = as.numeric(gsub("[\\$,]", "", MOTOR$OLDCLAIM))
MOTOR$CLM_AMT = as.numeric(gsub("[\\$,]", "", MOTOR$CLM_AMT))
MOTOR
#removing the comma
MOTOR$INCOME = as.numeric(gsub("[,,]", "", MOTOR$INCOME))
MOTOR$HOME_VAL = as.numeric(gsub("[,,]", "", MOTOR$HOME_VAL))
MOTOR$BLUEBOOK = as.numeric(gsub("[,,]", "", MOTOR$BLUEBOOK))
MOTOR$OLDCLAIM = as.numeric(gsub("[,,]", "", MOTOR$OLDCLAIM))
MOTOR$CLM_AMT = as.numeric(gsub("[,,]", "", MOTOR$CLM_AMT))
MOTOR
#DELETING ID AND BIRTH COLUMN 
MOTOR$ID = NULL
MOTOR$BIRTH = NULL
#converting education, occupation and car_type into numerical to make it easy for further calculation
MOTOR$EDUCATION=ifelse(MOTOR$EDUCATION=="<High School",0,ifelse(MOTOR$EDUCATION=="z_High School",1,ifelse(MOTOR$EDUCATION=="Bachelors",2,ifelse(MOTOR$EDUCATION=="Masters",3,ifelse(MOTOR$EDUCATION=="PhD",4,MOTOR$EDUCATION)))))
MOTOR$OCCUPATION=ifelse(MOTOR$OCCUPATION=="Clerical",0,ifelse(MOTOR$OCCUPATION=="z_Blue Collar",1,ifelse(MOTOR$OCCUPATION=="Manager",2,ifelse(MOTOR$OCCUPATION=="Professional",3,ifelse(MOTOR$OCCUPATION=="Doctor",4,ifelse(MOTOR$OCCUPATION=="Lawyer",5,ifelse(MOTOR$OCCUPATION=="Student",6,ifelse(MOTOR$OCCUPATION=="Home Maker",7,MOTOR$OCCUPATION))))))))
MOTOR$CAR_TYPE=ifelse(MOTOR$CAR_TYPE=="Minivan",0,ifelse(MOTOR$CAR_TYPE=="Van",1,ifelse(MOTOR$CAR_TYPE=="z_SUV",2,ifelse(MOTOR$CAR_TYPE=="Sports Car",3,ifelse(MOTOR$CAR_TYPE=="Pickup",4,ifelse(MOTOR$CAR_TYPE=="Panel Truck",5,MOTOR$CAR_TYPE))))))
MOTOR
##CONVERTING DATA INTO NUMERIC
num_cols <- unlist(lapply(MOTOR, is.numeric))  
num_cols
MOTOR$EDUCATION = as.numeric(MOTOR$EDUCATION)
MOTOR$OCCUPATION = as.numeric(MOTOR$OCCUPATION)
MOTOR$CAR_TYPE = as.numeric(MOTOR$CAR_TYPE)
#CONVERTING ALL NAs into 0
MOTOR[is.na(MOTOR)]=0
MOTOR
summary(MOTOR)
attach(MOTOR)
library(corrplot)
M = cor(MOTOR)
corrplot(M, method="square",type="upper",bg="grey",is.corr = TRUE)
##correlation value
round(cor(MOTOR),2)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(MOTOR))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(MOTOR)), size = smp_size)

train <- MOTOR[train_ind, ]
test <- MOTOR[-train_ind, ]
summary(train)
summary(test)
#we will use train dataset to develop models
#we will use test dataset to test the models formed from train dataset
attach(train)
##finding the right fit for model on count
fit1=lm(CLM_FREQ~ OLDCLAIM,data = train)
summary(fit1)
##The adjusted R-squared increases only if the new term improves the model more than would be expected by chance. 
##It decreases when a predictor improves the model by less than expected by chance.
#adjusted R²=0.2453 (fit1)
#Residual standard error: 1.007 (fit1)
fit2=lm(CLM_FREQ ~ OLDCLAIM+MVR_PTS)
summary(fit2)
#adjusted R²=0.3228,therefore increase in adjusted R² means an improvement in the model
#Residual standard error: 0.9534
fit3=lm(CLM_FREQ~OLDCLAIM+MVR_PTS+URBANICITY)
summary(fit3)
#adjusted R²= 0.3412,therefore increase in adjusted R² means an improvement in the model we will include urbanicity
#Residual standard error: 0.9403
fit4=lm(CLM_FREQ~OLDCLAIM+MVR_PTS+URBANICITY+CLAIM_FLAG)
summary(fit4)
#adjusted R²= 0.3496,there is an increase in adjusted R² but the increase isn't significant
#Residual standard error: 0.9344, however there is an significant decrease in S.e hence we will include claim_flag as a variable
fit5=lm(CLM_FREQ~OLDCLAIM+MVR_PTS+URBANICITY+CLAIM_FLAG+CAR_AGE)
summary(fit5)
#adjusted R²= 0.3496
#Residual standard error: 0.9343
##there is almost no difference in fit4 and fit5 hince we won't include car_age

#using glm
fit4_int=glm(CLM_FREQ~OLDCLAIM+MVR_PTS+URBANICITY+CLAIM_FLAG)
summary(fit4_int)
#AIC: 17638

fit401_int=glm(CLM_FREQ~OLDCLAIM*MVR_PTS+URBANICITY+CLAIM_FLAG)
summary(fit401_int)
#AIC: 17442
#there is a decrease in AIC from 17638 to 17442 just by OLDCLAIM*MVR_PTS
#testing which is better, fit4_int or fit401_int
anova(fit401_int, test = "Chisq")
#It can be seen that the two main effects (OLDCLAIM and MVR_PTS) are statistically significant, as well as their interaction.
# H0 : model2=model3    v/s     H1 : model2 != model3
anova(fit4,fit401_int,test="F")
#The p-value is lower than the usual threshold of 0.05.Hence we reject null hypothesis
#therefore model2 and model3 are not same
model1=lm(CLM_FREQ~OLDCLAIM*MVR_PTS+URBANICITY+CLAIM_FLAG)
coef(model1)
# (Intercept)         OLDCLAIM          MVR_PTS       URBANICITY       CLAIM_FLAG 
#-2.966283e-02     7.074525e-05     1.893948e-01     3.229187e-01     2.256637e-01 
#OLDCLAIM:MVR_PTS 
#-7.799294e-06 

##testing train dataset model with test dataset
attach(test)
model2=lm(CLM_FREQ~OLDCLAIM*MVR_PTS+URBANICITY+CLAIM_FLAG,data = test)
coef(model2)
#(Intercept)         OLDCLAIM          MVR_PTS       URBANICITY       CLAIM_FLAG 
#-2.508478e-02     7.783406e-05     1.819625e-01     3.157068e-01     1.500551e-01 
#OLDCLAIM:MVR_PTS 
#-7.423109e-06 
##testing model1 by using test data
#taking data from test data [17] oldclaim=6711  MVR_PTS=2  URBANICITY=1  CLAIM_FLAG=0 
newdata= data.frame(OLDCLAIM=6711,MVR_PTS=2,URBANICITY=1, CLAIM_FLAG=0  )
predict(model1,newdata)
#90% CI for CLM_FREQ
#mean of CLM_FREQ
predict(model1,newdata,interval="confidence",level=0.90)
#(1.01549,1.06878)
#since there is no zero in interval we can say there is a difference in response
#Individual of CLM_FREQ
predict(model1,newdata,interval="predict",level=0.90)
#(-0.4721633,2.556433)
#The 90% prediction intervals associated with CLM_FREQ, as the interval consist zero we can say 90% of value prediction is true

#Plot, the residuals against the fitted values
plot(model1,1)
#Q-Q plot of residuals
plot(model1,2)
#there is a linear relationship as we can we the dotted lines,there is increase in residual at the end
plot(model1,3)
#there is an exponential distribution as we see the red line 
plot(model1,4)
#The variability also does not seem to change too much across the range of fitted values.
#The residuals are often quite large (+/- 1 is an order of magnitude since this is log-scale). 
#But that's somewhat expected with the extremely low R^2 even the better model has.

#FINDING THE UNIQUE VALUES OF VARIABLES
sapply(MOTOR, function(x) length(unique(x)))
MOTOR
install.packages("ggplot2")
library(ggplot2)
#MVR POINTS
frequency <- data.frame(table(MOTOR[,"MVR_PTS"]))
colnames(frequency) <- c("value","n")
ggplot(frequency, aes(value,n*100/8161)) + geom_bar(stat = "identity",col="black") + xlab("") + ylab("Percent of records") + ggtitle("MVR_PTS") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#OCCUPATION
for(var in c("EDUCATION","CAR_TYPE","OCCUPATION"))
  frequency <- data.frame(table(MOTOR[,var]))
colnames(frequency) <- c("value","n")
ggplot(frequency,aes(value,n*100/8161)) + geom_bar(stat = "identity",col="black",fill="darkgrey") +xlab("") +
  ylab("Percent of records") + ggtitle(var) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#CAR_TYPE
for(var in c("EDUCATION","OCCUPATION","CAR_TYPE"))
  frequency <- data.frame(table(MOTOR[,var]))
colnames(frequency) <- c("value","n")
ggplot(frequency, aes(value,n*100/8161)) + geom_bar(stat = "identity",col="black",fill="darkgrey") + xlab("") +
  ylab("Percent of records") + ggtitle(var) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#EDUCATION
for(var in c("CAR_TYPE","OCCUPATION","EDUCATION"))
  frequency <- data.frame(table(MOTOR[,var]))
colnames(frequency) <- c("value","n")
ggplot(frequency, aes(value,n*100/8161)) + geom_bar(stat = "identity",col="black",fill="darkgrey") +xlab("") + 
  ylab("Percent of records") + ggtitle(var) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#YOJ
for(var in c("CAR_AGE","TIF","YOJ"))
  frequency <- data.frame(table(MOTOR[,var]))
colnames(frequency) <- c("value","n")
frequency$value <- as.vector(frequency$value)
frequency <- rbind(frequency ,data.frame(value = "Not stated",n = length(which(is.na(MOTOR[,var]) == TRUE)),stringsAsFactors=FALSE))
frequency$value <- factor(frequency$value,levels=c(unique(MOTOR[,var])[order(unique(MOTOR[,var]))],"Not stated"))
ggplot(frequency,
       aes(value,n*100/8161)) +
  geom_bar(stat = "identity",col="black",fill="darkgrey") +
  xlab("") +
  ylab("Percent of records") +
  ggtitle(var) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#AGE, TRAVTIME, BLUEBOOK, HOME_VAL, INCOME, OLDCLAIM
for(var in c("AGE","TRAVTIME","BLUEBOOK","HOME_VAL","INCOME","OLDCLAIM","TARGET_AMT"))
{hist(MOTOR[MOTOR[,var] > 0,var],
      xlab="Values",
      ylab="Number of records",
      main=var,
      labels=TRUE)}
#target amount 
hist(MOTOR$TARGET_AMT[MOTOR$TARGET_AMT >0 & MOTOR$TARGET_AMT < 10000],
     xlab="Values",
     ylab="Number of records",
     main="TARGET AMT < $10,000",
     labels=TRUE)


