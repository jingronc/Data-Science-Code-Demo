#instrumental variables example


#install package
install.packages("ivpack")
#load package
library(ivpack)

#read dataset
data(card.data)

#IV is nearc4 (near 4 year college)
#outcome is lwage (log of wage)
#'treatment' is educ (number of years of education)

#summary stats
#check what % of people got treatment
mean(card.data$nearc4)
#check distribution of outcome and treatment variables
par(mfrow=c(1,2))
hist(card.data$lwage)
hist(card.data$educ)

#is the IV associated with the treatment? strenght of IV
mean(card.data$educ[card.data$nearc4==1])
mean(card.data$educ[card.data$nearc4==0])


#make education binary
educ12<-card.data$educ>12
#estimate proportion of 'compliers'
#if nearc4=1 and educ12=1:compliers, always takers
#if nearc4=0 and educ12=1:always takers
propcomp<-mean(educ12[card.data$nearc4==1])- 
  mean(educ12[card.data$nearc4==0]) #
propcomp

#intention to treat effect
#effects of instrumental variables on outcomes
itt<-mean(card.data$lwage[card.data$nearc4==1])-
  mean(card.data$lwage[card.data$nearc4==0])
itt

#complier average causal effect
itt/propcomp


#two stage least squares

#stage 1: regress A on Z
s1<-lm(educ12~card.data$nearc4)
summary(s1)
## get predicted value of A given Z for each subject
predtx <-predict(s1, type = "response")
table(predtx)

#stage 2: regress Y on predicted value of A
s2<-lm(card.data$lwage~predtx)
summary(s2)


#2SLS using ivpack
ivmodel=ivreg(lwage ~ educ12, ~ nearc4, x=TRUE, data=card.data)
robust.se(ivmodel)


ivmodel_2=ivreg(lwage ~ educ12 + exper + reg661 + reg662 +
                reg663 + reg664 + reg665+ reg666 + reg667 + reg668, 
              ~ nearc4 + exper +
                reg661+ reg662 + reg663 + reg664 + reg665 + reg666 +
                reg667 + reg668, 
                x=TRUE, 
                data=card.data)
summary(ivmodel_2)
