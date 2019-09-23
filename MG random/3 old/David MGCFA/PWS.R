setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_WellBeingScale_RR_RN_Paper.csv")
summary(master)

##No reverse coded items.
library(car)
master$Q24_5 = recode(master$Q24_5, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_10 = recode(master$Q24_10, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_13 = recode(master$Q24_13, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_14 = recode(master$Q24_14, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_15 = recode(master$Q24_15, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_16 = recode(master$Q24_16,"1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_17 = recode(master$Q24_17, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_18 = recode(master$Q24_18, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_19 = recode(master$Q24_19, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_23 = recode(master$Q24_23, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_26 = recode(master$Q24_26, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_27 = recode(master$Q24_27, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_30 = recode(master$Q24_30,"1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_31 = recode(master$Q24_31, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_32 = recode(master$Q24_32, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_34 = recode(master$Q24_34, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_36 = recode(master$Q24_36,"1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_39 = recode(master$Q24_39, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
master$Q24_41 = recode(master$Q24_41, "1='6'; 2='5'; 3='4'; 4='3'; 5='2'; 6='1'")
####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:44], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,45)]
dontcolumn = replacepeople[ , c(1,2,45)]

##MICE
tempnomiss = mice(replacecolumn)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

##(By your powers) Combine (I am Captain Planet!)
filledin_none = cbind(dontcolumn, nomiss)
summary(filledin_none)

##Small note: When MICE is run on this project the YEAR column
##is moved from the last column to the thrid column.
##This trend is not observed with data that does not need
##to be MICE'd. All code exluding ID columns has been corrected
##for this. 
#~Hannah 

##Outliers##
##Mahal

mahal = mahalanobis(nomiss[ , -c(1:3)], 
                    colMeans(nomiss[ , -c(1:3)], na.rm = TRUE),
                    cov(nomiss[ , -c(1:3)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1:3)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##Additivity: correlations
correlations = cor(noout[,-c(1:3)], use="pairwise.complete.obs")
symnum(correlations)

##Make the random stuff & exclude the ID columns 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1:3)])

##get the linearity plot
##create the standardized residuals
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1)

##multivariate normality
hist(standardized, breaks=30)

##homogeneity and homoscedasticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

##Scoring
AUT <- rowSums(noout[, c("Q24_1", "Q24_7", "Q24_13", "Q24_19", "Q24_25", "Q24_31","Q24_37")])
AUT

EM <- rowSums(noout[, c("Q24_2", "Q24_8", "Q24_14", "Q24_20", "Q24_26", "Q24_32","Q24_38")])
EM

PG <- rowSums(noout[, c("Q24_3", "Q24_9", "Q24_15", "Q24_21", "Q24_27", "Q24_33","Q24_39")])
PG

PR <- rowSums(noout[, c("Q24_4", "Q24_10", "Q24_16", "Q24_22", "Q24_28", "Q24_34","Q24_40")])
PR

PL <- rowSums(noout[, c("Q24_5", "Q24_11", "Q24_17", "Q24_23", "Q24_29", "Q24_35","Q24_41")])
PL

SA <- rowSums(noout[, c("Q24_6", "Q24_12", "Q24_18", "Q24_24", "Q24_30", "Q24_36","Q24_42")])
SA

##CFA with internal + external + noout datasets / need to drop paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

library(lavaan)
overallmodel = '
AUT =~ Q24_1 + Q24_7 + Q24_13 + Q24_19 + Q24_25 + Q24_31 + Q24_37
EM =~ Q24_2 + Q24_8 + Q24_14 + Q24_20 + Q24_26 + Q24_32 + Q24_38
PG =~ Q24_3 + Q24_9 + Q24_15 + Q24_21 + Q24_27 + Q24_33 + Q24_39 
PR =~ Q24_4 + Q24_10 + Q24_16 + Q24_22 + Q24_28 + Q24_34 + Q24_40
PL =~ Q24_5 + Q24_11 + Q24_17 + Q24_23 + Q24_29 + Q24_35 + Q24_41
SA =~ Q24_6 + Q24_12 + Q24_18 + Q24_24 + Q24_30 + Q24_36 + Q24_42
'

##fit for overall (excluding paper)
overall.fit = cfa(model = overallmodel, 
                  data = nomissnop, 
                  meanstructure = TRUE)

summary(overall.fit, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE) ##no heywood cases

##fit for random 
random.fit = cfa(overallmodel, 
                 data = random, 
                 meanstructure = TRUE)
summary(random.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

fitMeasures(random.fit) ##Dr. B, is this line of code necessary?

##fit for not random 
notrandom.fit = cfa(overallmodel, 
                    data = notrandom, 
                    meanstructure = TRUE)
summary(notrandom.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

##invariances 
library(semTools)
multisteps = measurementInvariance(overallmodel, 
                                   data = nomissnop, 
                                   group = 'Source', 
                                   strict = T)

fitmeasures(multisteps)
fitMeasures(multisteps$fit.configural)
fitMeasures(multisteps$fit.loadings)
fitMeasures(multisteps$fit.intercepts)
fitMeasures(multisteps$fit.residuals)
fitMeasures(multisteps$fit.means)