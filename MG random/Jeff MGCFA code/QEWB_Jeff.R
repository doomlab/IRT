setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_QuestionnaireEudaimonic_RR_RN_Paper.csv")
summary(master)

##No reverse coded items.
library(car)
master$Q106_3 = recode(master$Q106_3, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_7 = recode(master$Q106_7, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_11 = recode(master$Q106_11, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_12 = recode(master$Q106_12, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_16 = recode(master$Q106_16, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_19 = recode(master$Q106_19, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
master$Q106_20 = recode(master$Q106_20, "1='5'; 2='4'; 3='3'; 4='2'; 5='1'")
####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:23], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
names(notypos)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,23)]
dontcolumn = replacepeople[ , c(1,2,23)]

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
names(filledin_none)

mahal = mahalanobis(filledin_none[ , -c(1:3)], 
                    colMeans(filledin_none[ , -c(1:3)], na.rm = TRUE),
                    cov(filledin_none[ , -c(1:3)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(filledin_none[ , -c(1:3)])) 
summary(mahal < cutoff)
noout = nomiss[ mahal < cutoff, ]

##Additivity: correlations
correlations = cor(filledin_none[,-c(1:3)], use="pairwise.complete.obs")
symnum(correlations)

##Make the random stuff & exclude the ID columns 
random = rchisq(nrow(filledin_none), 7)
fake = lm(random~., data=filledin_none[ , -c(1:3)])

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
QWEB <- rowSums(noout[, c(1:21)])
QWEB

##exclude paper people using noout dataset 
nomissnop = subset(filledin_none, Source < 2)

##overall model 
library(lavaan)
names(nomissnop)
table(nomissnop$Source)
names(filledin_none)
overallmodel = '
QEWB =~ Q106_1 + Q106_2 + Q106_3 + Q106_4 + Q106_5 + Q106_6 + Q106_7 + Q106_8 + Q106_9 + Q106_10 + Q106_11 + Q106_12 + Q106_13 + Q106_14 + Q106_15 + Q106_16 + Q106_17 + Q106_18 + Q106_19 + Q106_20
'

fit = cfa(overallmodel, 
          data = nomissnop, 
          meanstructure = TRUE)

summary(fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)
fitMeasures(fit)
table(nomissnop$Source)

##subset for other two cfa
random = subset(nomissnop, Source == "1")
notrandom = subset(nomissnop, Source =="0")

##fit for random 
random.fit = cfa(overallmodel, 
                 data = random, 
                 meanstructure = TRUE)
summary(random.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)
fitMeasures(random.fit)

##fit for not random 
notrandom.fit = cfa(overallmodel, 
                    data = notrandom, 
                    meanstructure = TRUE)
summary(notrandom.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE) 
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

#partial invariances 
partial = partialInvariance(multisteps, 
                            type = "metric")
strictfree = partial$results


##after letting it go 
partialstrict = measurementInvariance(overallmodel, 
                                      data = nomissnop, 
                                      group = 'Source', 
                                      strict = T, 
                                      group.partial = c("QEWB=~Q106_16"))

fitMeasures(partialstrict$fit.loadings)
fitMeasures(partialstrict$fit.intercepts)

##partials2 breaks on intercepts
partial2 = partialInvariance(partialstrict, type = "scalar")
strictfree2 = partial2$results

partialstrict2 = measurementInvariance(overallmodel, 
                                       data = nomissnop, 
                                       group = 'Source', 
                                       strict = T, 
                                       group.partial = c("QEWB=~Q106_16","Q106_6~1" ))
fitMeasures(partialstrict2$fit.intercepts)
fitMeasures(partialstrict2$fit.residuals)

##partials3 
partial3 = partialInvariance(partialstrict2, type = "strict")
strictfree3 = partial3$results

partialstrict3 = measurementInvariance(overallmodel, 
                                       data = nomissnop, 
                                       group = 'Source', 
                                       strict = T, 
                                       group.partial = c("QEWB=~Q106_16","Q106_6~1", "Q106_19~~Q106_19" ))
fitMeasures(partialstrict3$fit.residuals)
