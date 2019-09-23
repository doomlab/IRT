##Set working directory 
setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_STMS_RR_RN_Paper.csv")
summary(master)

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:36], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,37)]
dontcolumn = replacepeople[ , c(1,2,37)]

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

mahal = mahalanobis(filledin_none[ , -c(1:3)], 
                    colMeans(filledin_none[ , -c(1:3)], na.rm = TRUE),
                    cov(filledin_none[ , -c(1:3)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(filledin_none[ , -c(1:3)])) 
summary(mahal < cutoff)
noout = filledin_none[ mahal < cutoff, ]

##Additivity: correlations
correlations = cor(noout[,-c(1:3)], use="pairwise.complete.obs")
symnum(correlations)

##Make the random stuff & exclude the ID columns 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1:3)])

##Linearity plot
##Create the standardized residuals
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1)

##Multivariate normality
hist(standardized, breaks=15)

##Homogeneity and homoscedaticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

##exclude paper people using noout dataset 
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

##Scoring
PH <- rowSums(noout[, c(20,27,19,22,23,29,25,24,26,30,18,33,31,28,32,21,36,11,35)])
PH


AH<- rowSums(noout[, c(5,6,15,13,12,7,4,10,14,9,8)])
AH


NLE <- rowSums(noout[, c(17,16,34)])
NLE

##overallmodel 
names(nomissnop)
overallmodel = '
PH =~ Q80_17 + Q80_24 + Q80_16 + Q80_19 + Q80_20 + Q80_26 + Q80_22 + Q80_21 + Q80_23 + Q80_27 + Q80_15 + Q80_30 + Q80_28 + Q80_25 + Q80_29 + Q80_18 + Q80_33 + Q80_8 + Q80_32
AH =~ Q80_2 + Q80_3 + Q80_12 + Q80_10 + Q80_9 + Q80_4 + Q80_1 + Q80_7 + Q80_11 + Q80_6 + Q80_5 
NLE =~ Q80_14 + Q80_13 + Q80_31
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

##partials at intercepts scalar~1 
partial = partialInvariance(multisteps, 
                            type = "scalar")
strictfree = partial$results ##Q80_1

partialstrict = measurementInvariance(overallmodel, 
                                      data = nomissnop, 
                                      group = "Source", 
                                      strict = T, 
                                      group.partial = c("Q80_1~1"))
fitmeasures(partialstrict$fit.intercepts)
fitmeasures(partialstrict$fit.residuals)
