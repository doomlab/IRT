setwd("G:/GA/Scales")
master = read.csv("Meaning_Scales_MAPA_RR_RN_Paper.csv")
summary(master)

##recode 0 to 6 for frequency Q4
names(master)
library(car)
master$Q4_1 = recode(master$Q4_1, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_2 = recode(master$Q4_2, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_3 = recode(master$Q4_3, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_4 = recode(master$Q4_4, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_5 = recode(master$Q4_5, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_6 = recode(master$Q4_6, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_7 = recode(master$Q4_7, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_8 = recode(master$Q4_8, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_9 = recode(master$Q4_9, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_10 = recode(master$Q4_10, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_11 = recode(master$Q4_11, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_12 = recode(master$Q4_12, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_13 = recode(master$Q4_13, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_14 = recode(master$Q4_14, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_15 = recode(master$Q4_15, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_16 = recode(master$Q4_16, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_17 = recode(master$Q4_17, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_18 = recode(master$Q4_18, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_19 = recode(master$Q4_19, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_20 = recode(master$Q4_20, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_21 = recode(master$Q4_21, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_22 = recode(master$Q4_22, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_23 = recode(master$Q4_23, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_24 = recode(master$Q4_24, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_25 = recode(master$Q4_25, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_26 = recode(master$Q4_26, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_27 = recode(master$Q4_27, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")
master$Q4_28 = recode(master$Q4_28, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'; 6='5'; 7 = '6'")

##recode 0 to 4 for rating Q5
library(car)
master$Q4_1 = recode(master$Q4_1, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_2 = recode(master$Q4_2, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_3 = recode(master$Q4_3, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_4 = recode(master$Q4_4, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_5 = recode(master$Q4_5, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_6 = recode(master$Q4_6, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_7 = recode(master$Q4_7, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_8 = recode(master$Q4_8, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_9 = recode(master$Q4_9, "1='0'; 2='1'; 3='2'; 4='3'; 5='4''")
master$Q4_10 = recode(master$Q4_10, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_11 = recode(master$Q4_11, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_12 = recode(master$Q4_12, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_13 = recode(master$Q4_13, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_14 = recode(master$Q4_14, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_15 = recode(master$Q4_15, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_16 = recode(master$Q4_16, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_17 = recode(master$Q4_17, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_18 = recode(master$Q4_18, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_19 = recode(master$Q4_19, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_20 = recode(master$Q4_20, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_21 = recode(master$Q4_21, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_22 = recode(master$Q4_22, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_23 = recode(master$Q4_23, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_24 = recode(master$Q4_24, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_25 = recode(master$Q4_25, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_26 = recode(master$Q4_26, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_27 = recode(master$Q4_27, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")
master$Q4_28 = recode(master$Q4_28, "1='0'; 2='1'; 3='2'; 4='3'; 5='4'")

##missing
library(mice)
percentmiss = function (x){ sum(is.na(x))/length(x) * 100}

##Going by ROWS only
notypos = master
missing = apply(notypos[ , 3:58], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,59)]
dontcolumn = replacepeople[ , c(1,2,59)]

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
names(finalscores)
freq = master[ , c(3:30)]
rating = master[ , c(31: 58)]
finalscores = freq*rating
MAPAfinal = rowSums(finalscores[ , c(1:28)])

##subsetting data
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

library(lavaan)
##data sets
overallmodel = '
MAPA =~ Q4_1 + Q4_2 + Q4_3 + Q4_4 + Q4_5 + Q4_6 + Q4_7 + Q4_8 + Q4_9 + Q4_10
+ Q4_11 + Q4_12 + Q4_13 + Q4_14 + Q4_15 + Q4_16 + Q4_17 + Q4_18 + Q4_19 + Q4_20
+ Q4_21 + Q4_22 + Q4_23 + Q4_24 + Q4_25 + Q4_26 + Q4_27 + Q4_28
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

##Broke Down on Scalar and Strict Invariance
##Scalar First
partial = partialInvariance(multisteps, 
                            type = "intercepts")
interceptsfree = partial$results
multisteps2 = measurementInvariance(overallmodel,  
                                    data = nomissnop, 
                                    group = "Source",
                                    strict = T,
                                    group.partial = c("Q4_19~1"))
##Now Strict
partial2 = partialInvariance(multisteps2, 
                            type = "residuals")
residualsfree = partial2$results
multisteps3 = measurementInvariance(overallmodel,  
                                    data = nomissnop, 
                                    group = "Source",
                                    strict = T,
                                    group.partial = c("Q4_19~1", "Q4_5~~Q4_5"))
