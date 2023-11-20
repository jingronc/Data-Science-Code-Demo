lalonde_v2 <- read.csv("lalonde.csv")
library(tableone)
library(Matching)
library(MatchIt)
library(dplyr)
data(lalonde)

###data preparation
library(fastDummies)
data <- dummy_cols(lalonde, select_columns = 'race')
glimpse(data)

###pre-matching balancing check
#covariates that we will use
xvars<-c("age","educ","race_black","race_hispan",
         "married","nodegree","re74","re75")
table<- CreateTableOne(vars=xvars,strata="treat", data=data, test=FALSE)
print(table,smd=TRUE)
mean(data$re78[data$treat==1]) - mean(data$re78[data$treat==0]) 

##Calculate propensity score
#fit a logistic model
psmodel<-glm(treat~age+educ+race_black+race_hispan+
               married+nodegree+re74+re75,
             family=binomial(),data=data)
summary(psmodel)
pscore<-psmodel$fitted.values
min(pscore)
max(pscore)

###Matching
set.seed(931139)
#Using Matching package
psmatch<-Match(Tr=data$treat,M=1,X=pscore,replace=FALSE)
matched<-data[unlist(psmatch[c("index.treated","index.control")]), ]
#get standardized differences
matchedtab0<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab0, smd = TRUE)

#Adding caliper
set.seed(931139)
psmatch_1<-Match(Tr=data$treat,M=1,X=pscore,replace=FALSE,caliper=.1)
matched_1<-data[unlist(psmatch_1[c("index.treated","index.control")]), ]
matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched_1, test = FALSE)
print(matchedtab1, smd = TRUE)

#Outcome analysis
y_trt<-matched_1$re78[matched_1$treat==1]
y_con<-matched_1$re78[matched_1$treat==0]
diffy<-y_trt-y_con
mean(y_trt-y_con)
t.test(diffy)