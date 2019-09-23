setwd("~/Desktop/Research/DOOM LAB")
master = read.csv("Meaning_Scales_SpiritualMeaningScale_RR_RN_Paper.csv")
summary(master)

####Libraries####
##libraries
library(lavaan)
library(semPlot)
library(semTools)

####DATA SCREENING####
##deal with 6 and 7 should be 1 item, not two
master$Q23_6.5 = (master$Q23_6 + master$Q23_7) / 2

##Reverse Code 1 5 9 13 
library(car)
master$Q23_1 = recode(master$Q23_1, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q23_5 = recode(master$Q23_5, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q23_10 = recode(master$Q23_10, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q23_14 = recode(master$Q23_14, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , c(3:7, 10:18, 20)], 1, percentmiss) 
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

##Scoring
SMS <- rowSums(noout[,c(3:7, 10:18, 20)])
summary(SMS)

####CFA####
##subsetting data
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

####overall model for everyone together##
overallmodel = '
SMS =~
Q23_1 + Q23_2 + Q23_3 + Q23_4 + Q23_5 + Q23_6.5 + 
Q23_8 + Q23_9 + Q23_10 + Q23_11 + Q23_12 + Q23_13 + Q23_14 +
Q23_15 + Q23_16
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


####multi group testing####
###measurement invariance

multisteps = measurementInvariance(overallmodel, 
                                   data = nomissnop, 
                                   group = "Source",
                                   strict = T)

fitmeasures(multisteps$fit.configural)
fitmeasures(multisteps$fit.loadings)
fitmeasures(multisteps$fit.intercepts)
fitmeasures(multisteps$fit.residuals)

##It didn't break down! whoo hoo!
