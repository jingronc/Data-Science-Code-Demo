library(tableone)
library(Matching)
library(ipw) 
library(survey)
library(sandwich)
library(MatchIt)
#Now load the lalonde data (which is in the MatchIt package):
data(lalonde)
library(fastDummies)
data <- dummy_cols(lalonde, select_columns = 'race')
glimpse(data)

#Fit a logistic regression
psmodel <- glm(treat ~ age+educ+race_black+race_hispan+
                 married+nodegree+re74+re75, 
               data = data,
               family  = binomial(link ="logit"))
summary(psmodel)
#Get propensity score for each subject
ps <-predict(psmodel, type = "response")
#Create weight
weight<-ifelse(data$treat==1,1/(ps),1/(1-ps))
min(weight)
max(weight)
#apply weights to data
weighteddata<-svydesign(ids = ~ 1, data =data, weights = ~ weight)
weighteddata
#weighted table 1
xvars<-c("age","educ","race_black","race_hispan",
         "married","nodegree","re74","re75")
weightedtable <-svyCreateTableOne(vars = xvars, strata = "treat", 
                                  data = weighteddata, test = FALSE)
print(weightedtable, smd = TRUE)

#get causal risk difference
glm.obj<-glm(re78 ~ treat,data=data,weights=weight,family=quasi(link="identity"))

#summary(glm.obj)
betaiptw<-coef(glm.obj)
#vcovHC: cov matrix
SE<-sqrt(diag(vcovHC(glm.obj, type="HC0")))

causalrd<-(betaiptw[2])
lcl<-(betaiptw[2]-1.96*SE[2])
ucl<-(betaiptw[2]+1.96*SE[2])
c(lcl,causalrd,ucl)

#############################
#alternative: use ipw package
#############################

#first fit propensity score model to get weights
weightmodel<-ipwpoint(exposure= treat, 
                      family = "binomial", 
                      link ="logit",
                      denominator= ~ age+educ+race_black+race_hispan+
                        married+nodegree+re74+re75, 
                      data=data)
#numeric summary of weights
summary(weightmodel$ipw.weights)
#plot of weights
ipwplot(weights = weightmodel$ipw.weights, logscale = FALSE,
        main = "weights", xlim = c(0, 22))
data$wt<-weightmodel$ipw.weights

#fit a marginal structural model (risk difference)
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~wt,
                                                    data =data)))
coef(msm)
confint(msm)

# fit propensity score model to get weights, but truncated
weightmodel_v2<-ipwpoint(exposure= treat, 
                      family = "binomial", 
                      link ="logit",
                      denominator= ~ age+educ+race_black+race_hispan+
                        married+nodegree+re74+re75, 
                      data=data,
                      trunc=.01)#truncates at 1st and 99th percentiles
#numeric summary of weights
summary(weightmodel_v2$weights.trun)
#plot of weights
ipwplot(weights = weightmodel_v2$weights.trun, logscale = FALSE,
        main = "weights", xlim = c(0, 22))
data$wt_v2<-weightmodel_v2$weights.trun
#fit a marginal structural model (risk difference)
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~wt_v2,
                                                data =data)))
coef(msm)
confint(msm)
