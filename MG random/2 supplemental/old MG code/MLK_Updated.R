setwd("D:/GA/Scales")
master = read.csv("Meaning_Scales_Meaning_Krause_RR_RN_Paper.csv")
summary(master)

##Reverse Code. None.

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:16], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,17)]
dontcolumn = replacepeople[ , c(1,2,17)]

##no missing data.
nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,17)], 
                    colMeans(nomiss[ , -c(1,2,17)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,17)], use="pairwise.complete.obs"))
mahal
summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,17)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,17)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,17)])

##get the linearity plot
##create the standardized residuals
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1)

##multivariate normality
hist(standardized, breaks=15)

##homogeneity and homoscedaticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

##Scoring
Values<- rowSums(noout[, c(3,4,5)])
Values

Purpose<- rowSums(noout[, c(6,7,8,9)])
Purpose

Goals<- rowSums(noout[, c(10,11,12)])
Goals

RP<- rowSums(noout[, c(13,14,15,16)])
RP

##MGCFA
library(lavaan)
library(semTools)
library(mice)
library(effsize)
library(ltm)

##Factoring source##
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

##Model 
overallmodel = '
Values =~ Q6_1 + Q6_2 + Q6_3
Purpose =~ Q6_4 + Q6_5 + Q6_6 + Q6_7
Goals =~ Q6_8 + Q6_9 + Q6_10
Reflections =~ Q6_11 + Q6_12 + Q6_13 + Q6_14 
'
overall.fit = cfa(overallmodel, 
                  data=nomissnop, 
                  meanstructure = TRUE)

summary(overall.fit, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE)

####separate group models####
##notrandom
overall.fit.nr = cfa(overallmodel, 
                     data=notrandom, 
                     meanstructure = TRUE)

summary(overall.fit.nr, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE)

##random
overall.fit.r = cfa(overallmodel, 
                    data=random, 
                    meanstructure = TRUE)

summary(overall.fit.r, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE)


####multi group testing####
###measurement invariance
options(scipen = 999)

multisteps = measurementInvariance(overallmodel, 
                                   data = nomissnop, 
                                   group = "Source",
                                   strict = T)

fitmeasures(multisteps$fit.configural)
fitmeasures(multisteps$fit.loadings)
fitmeasures(multisteps$fit.intercepts)
fitmeasures(multisteps$fit.residuals)

