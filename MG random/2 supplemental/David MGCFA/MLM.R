setwd("G:/GA/Scales")
master = read.csv("Meaning_Scales_MeaningfulLifeMeasure_RR_RN_Paper.csv")
summary(master)

##reverse coding for 5, 9, 19
library(car)
master$Q16_2 = recode(master$Q16_2, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7 = '1'")
master$Q16_6 = recode(master$Q16_6, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7 = '1'")
master$Q19_1 = recode(master$Q19_1, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7 = '1'")
master$Q19_2 = recode(master$Q19_2, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7 = '1'")
master$Q19_3 = recode(master$Q19_3, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7 = '1'")
master$Q19_4 = recode(master$Q19_4, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7 = '1'")
master$Q19_5 = recode(master$Q19_5, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7 = '1'")
master$Q19_6 = recode(master$Q19_6, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7 = '1'")

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:25], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,25)]
dontcolumn = replacepeople[ , c(1,2,25)]

##let's mice it!
tempnomiss = mice(replacecolumn)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

##put everything back together
filledin_none = cbind(dontcolumn, nomiss)
summary(filledin_none)

##Outliers##
mahal = mahalanobis(filledin_none[ , -c(1:3)], 
                    colMeans(filledin_none[ , -c(1:3)], na.rm = TRUE),
                    cov(filledin_none[ , -c(1:3)], use="pairwise.complete.obs"))

cutoff = qchisq(.999,ncol(filledin_none[ , -c(1:3)])) 
summary(mahal < cutoff)
noout = filledin_none[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1:3)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:3)])

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
##make sure 5, 9, 19 are reverse coded (this should be up top)
##exciting life = 1-5
##accomplished life = 6-10 
##principled life = 11-15 
##purposeful life = 16-19 
##valued life = 20-23

names(noout)
Exciting = rowSums(filledin_none[ , c(4:7)])
Accomplished = rowSums(filledin_none[ , c(8:12)])
Principled = rowSums(filledin_none[ , c(13:17)])
Purposeful = rowSums(filledin_none[ , c(18:21)])
Valued = rowSums(filledin_none[ , c(22:25)])

##subsetting data
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

library(lavaan)
##data sets
overallmodel = '
Exciting =~ Q13 + Q14 + Q15 + Q16_1 + Q16_2  
Accomplished =~ Q16_3 + Q16_4 + Q16_5 + Q16_6 + Q16_7
Principled =~ Q16_8 + Q16_9 + Q16_10 + Q16_11 + Q16_12
Purposeful =~ Q17 + Q18 + Q19_1 + Q19_2  
Valued =~ Q19_3 + Q19_4 + Q19_5 + Q19_6
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

##Broke down at strict Invariance
partial = partialInvariance(multisteps, 
                            type = "residuals")
redidualsfree = partial$results
multisteps2 = measurementInvariance(overallmodel,  
                                    data = nomissnop, 
                                    group = "Source",
                                    strict = T,
                                    group.partial = c("Q19_4~~Q19_4"))