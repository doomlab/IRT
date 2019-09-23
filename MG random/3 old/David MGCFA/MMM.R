setwd("G:/GA/Scales")
master = read.csv("Meaning_Scales_MundaneMeaning_RR_RN_Paper.csv")
summary(master)

####DATA SCREENING####
##Reverse Code. No reverse coding. Stopped here for scoring.
##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:18], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,19)]
dontcolumn = replacepeople[ , c(1,2,19)]

nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,19)], 
                    colMeans(nomiss[ , -c(1,2,19)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,19)], use="pairwise.complete.obs"))

cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,19)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,19)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,19)])

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

##scoring 
##Coherence = 1-4 
##Integration of Circumstances = 5-8 
##High Level Action Identification = 9-12 
##Sense of Purpose = 13-16
Coherence = rowSums(filledin_none[ , c(3:6)])
Integration = rowSums(filledin_none[ , c(7:10)])
HLAIdentification = rowSums(filledin_none[ , c(11:14)])
SenseofPurpose = rowSums(filledin_none[ , c(15:18)])

##subsetting data
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

library(lavaan)
##data sets
overallmodel = '
Coherence =~ Q111_1 + Q111_2 + Q111_3 + Q111_4 
Integration =~ Q111_5 + Q111_6 +Q111_7 + Q111_8  
HLAIdentification =~ Q111_9 + Q111_10 + Q111_11 + Q111_12   
SunseofPurpose =~ Q111_13 + Q111_14 + Q111_15 + Q111_16    
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
