setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_ELQ_RN_RR_Done_Paper.csv")
summary(master)

##reverse coding 3 12, 23, 27 which is items 2, 7, 14, 18
master[ , c(4, 8, 14, 18)] = 6 - master[ , c(4, 8, 14, 18)]

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:21], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,22)]
dontcolumn = replacepeople[ , c(1,2,22)]

nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,22)], 
                    colMeans(nomiss[ , -c(1,2,22)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,22)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(nomiss[ , -c(1,2,22)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,22)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,22)])

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
ELQ <- rowSums(noout[, -c(1:2,22)])
summary(ELQ)

####overall cfa everyone together####
library(lavaan)

nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

overallmodel = '
ELQ =~ Q1_1 + Q1_2 + Q1_3 + Q1_5 + Q1_6 + Q1_7 + 
Q1_9 + Q1_10 + Q1_11 + Q1_12 + Q1_13 + Q1_14 +
Q1_15 + Q1_16 + Q1_17 + Q1_18 + Q1_19 + Q1_20 + Q1_21
'

##CFA

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

##fit for not random 
notrandom.fit = cfa(overallmodel, 
                    data = notrandom, 
                    meanstructure = TRUE)
summary(notrandom.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

library(semTools)

multisteps = measurementInvariance(overallmodel, 
                                   data = nomissnop, 
                                   group = 'Source', 
                                   strict = T)

fitmeasures(multisteps$fit.configural)
fitmeasures(multisteps$fit.loadings)
fitmeasures(multisteps$fit.intercepts)
fitmeasures(multisteps$fit.residuals)

##It broke down on the Scalar Invariance level

partial = partialInvariance(multisteps, 
                            type = "intercepts")
##save only the results for easier viewing
interceptsfree = partial$results

##click on the name, sort by FREE CFI
##release the biggest one first

multisteps2 = measurementInvariance(overallmodel, 
                                    data = nomissnop, 
                                    group = "Source",
                                    strict = T,
                                    group.partial = c("Q1_21~1"))
##Yay! 
