setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_VOL_RR_RN_Paper.csv")
summary(master)

##No reverse coded items.

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:21], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,22)]
dontcolumn = replacepeople[ , c(1,2,22)]

nomiss = replacepeople

##Outliers##
mahal = mahalanobis(nomiss[ , -c(1,2,22)], 
                    colMeans(nomiss[ , -c(1,2,22)], na.rm = TRUE),
                    cov(nomiss[ , -c(1,2,22)], use="pairwise.complete.obs"))

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

##exclude paper people using noout dataset 
nooutnop = subset(noout, Source < 2)
nomissnop = subset(nomiss, Source < 2)

##Scoring
pos = rowSums(noout[,c("Q25_1", "Q25_2", "Q25_3", "Q25_4", "Q25_5", "Q25_6","Q25_7", "Q25_8", "Q25_9", "Q25_10", "Q25_11", "Q25_12","Q25_13")])
pos

#External 
Neg = rowSums(noout[,c("Q25_14", "Q25_15", "Q25_16", "Q25_17", "Q25_18", "Q25_19")])
Neg

##overallmodel 
library(lavaan)
overallmodel = '
POS =~ Q25_1 + Q25_2 + Q25_3 + Q25_4 + Q25_5 + Q25_6 + Q25_7 + Q25_8 + Q25_9 + Q25_10 + Q25_11 + Q25_12 + Q25_13
NEG =~ Q25_14 + Q25_15 + Q25_16 + Q25_17 + Q25_18 + Q25_19
'
fit = cfa(overallmodel, 
          data = nomissnop, 
          meanstructure = TRUE)
fitMeasures(fit)
table(nomissnop$Source)

##subset for other two cfa
random = subset(nomissnop, Source == "1")
notrandom = subset(nomissnop, Source =="0")

##fit for random 
random.fit = cfa(overallmodel, 
                 data = random, 
                 meanstructure = TRUE)
fitMeasures(random.fit)

##fit for not random 
notrandom.fit = cfa(overallmodel, 
                    data = notrandom, 
                    meanstructure = TRUE)
fitMeasures(notrandom.fit)

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

##partials 
partial = partialInvariance(multisteps, 
                            type = "scalar")
strictfree = partial$results
##Q25_16


partialstrict = measurementInvariance(overallmodel, 
                                      data = nomissnop, 
                                      group = 'Source', 
                                      strict = T, 
                                      group.partial = c("	Q25_16~1"))

fitmeasures(partialstrict$fit.intercepts)
fitmeasures(partialstrict$fit.residuals)

##partials agin bc broke down at strict

partial2 = partialInvariance(partialstrict, type = "strict")
strictfree = partial2$results

partialstrict2 = measurementInvariance(overallmodel, 
                                      data = nomissnop, 
                                      group = 'Source', 
                                      strict = T, 
                                      group.partial = c("	Q25_16~1","Q25_3~~Q25_3"))
fitMeasures(partialstrict2$fit.residuals)

partial3 = partialInvariance(partialstrict2, type = "strict")
strictfree3 = partial3$results

partialstrict3 = measurementInvariance(overallmodel, 
                                       data = nomissnop, 
                                       group = 'Source', 
                                       strict = T, 
                                       group.partial = c("	Q25_16~1","Q25_3~~Q25_3", "Q25_16~~Q25_16"))
fitMeasures(partialstrict3$fit.residuals)

##strict 4 
partial4 = partialInvariance(partialstrict3, type = "strict")
strictfree4 = partial4$results

partialstrict4 = measurementInvariance(overallmodel, 
                                       data = nomissnop, 
                                       group = 'Source', 
                                       strict = T, 
                                       group.partial = c("	Q25_16~1","Q25_3~~Q25_3", "Q25_16~~Q25_16", "Q25_6~~Q25_6"))
fitMeasures(partialstrict4$fit.residuals)
