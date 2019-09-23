####Set Working Directory####
setwd("~/Desktop/Research/DOOM LAB/DataLinks")

####Upload Data####
master = read.csv("Meaning_Scales_PVSHardiness_RR_Missing_RN_Paper_Fixed.csv")
summary(master)

####Libraries####
library(lavaan)
library(semPlot)
library(semTools)

##fix to 0-3, since qualtrics did 1-4
master[ , c(3:20)] = master[ , c(3:20)] - 1

##reverse coding 2, 4, 5, 7, 10, 12, 13, 15, 18
master[ , c(4,6,7,9,12,14,15,17,20)] = 3 - master[ , c(4,6,7,9,12,14,15,17,20)]
apply(master[ , c(3:20)],2,table)

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:20], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,21)]
dontcolumn = replacepeople[ , c(1,2,21)]

##no missing data.
nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,21)], 
                    colMeans(nomiss[ , -c(1,2,21)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,21)], use="pairwise.complete.obs"))
summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,21)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations

correlations = cor(noout[,-c(1,2,21)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,21)])

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
PVSIII <- rowSums(noout[, c(3:20)])
summary(PVSIII)

####CFA####
##subsetting data
#Zero = notrandom, One = random, Two = paper
##subset the data
nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

####overall model for everyone together##
colnames(nomissnop)
overallmodel = '
PVS =~
Q120_4 + Q120_5 + Q120_6 + Q120_7 + Q120_8 + Q120_9 + Q120_10 + Q120_11 +
Q120_12 + Q120_13 + Q120_14 + Q120_15 + Q120_16 + Q120_17 + Q120_18 +
Q120_19 + Q120_20 + Q120_21
'

overall.fit = cfa(model = overallmodel, 
                  data=nomissnop, 
                  meanstructure = TRUE)

summary(overall.fit, 
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

##notrandom
overall.fit.nr = cfa(overallmodel, 
                     data=notrandom, 
                     meanstructure = TRUE)

summary(overall.fit.nr, 
        standardized=TRUE,
        rsquare=TRUE, 
        fit.measure = TRUE)

####Multigroup Testing####
library(semTools)
options(scipen = 999)
multisteps = measurementInvariance(overallmodel, 
                                   data = nomissnop, 
                                   group = "Source",
                                   strict = T)

fitmeasures(multisteps$fit.configural)
fitmeasures(multisteps$fit.loadings)
fitmeasures(multisteps$fit.intercepts)
fitmeasures(multisteps$fit.residuals)

