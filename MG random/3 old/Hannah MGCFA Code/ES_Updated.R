##Set working directory
setwd("D:/GA/Scales")
master = read.csv("Meaning_Scales_Existance_RN_RR_Done_Paper-1.csv")
summary(master)

##Reverse Code. 2, 15, 21, 26, 36
library(car)
master$Q2_2 = recode(master$Q2_2, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q2_15 = recode(master$Q2_15, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q2_21 = recode(master$Q2_21, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q2_26 = recode(master$Q2_26, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")
master$Q2_36 = recode(master$Q2_36, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
##here I excluded the first two columns because they were 
##included for everyone as IDs, so I don't want to count that
##as part of the percent toward what they did
notypos = master
missing = apply(notypos[ , 3:48], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,49)]
dontcolumn = replacepeople[ , c(1,2,49)]

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

summary(mahal)
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

#get the linearity plot
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

sum - SD: 3, 5, 19, 32, 40, 42, 43, 44 ; ST: 2, 4, 11, 12, 13, 14, 21, 27, 33, 34, 35, 36, 41, 45 ; F: 9, 10, 15, 17, 18, 23, 24, 26, 28, 31, 46 ; R: 1, 6, 7, 8, 16, 20, 22, 25, 29, 30, 37, 38, 39  

##Scoring 
selfdis <- rowSums(noout[, c(6,8,22,35,43,45,46,47)])
summary(selfdis)

selftran <- rowSums(noout[, c(5,7,14,15,16,17,24,30,36,37,38,39,44,48)])
summary(selftran)

freedom <- rowSums(noout[, c(12,13,18,20,21,26,27,29,31,34,49)])
summary(freedom)

response <- rowSums(noout[, c(4,9,10,11,19,23,25,28,32,33,40,41,42)]) 
summary(response)

#MGCFA
library(lavaan)
library(semTools)

##Factoring source##
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

##Model 
overallmodel = '
SelfDistance =~ Q2_3 + Q2_5 + Q2_19 + Q2_32 + Q2_40 + Q2_42 + Q2_43 + Q2_44
SelfTrans =~ Q2_2 + Q2_4 + Q2_11 + Q2_12 + Q2_13 + Q2_14 + Q2_21 + Q2_27 + Q2_33 + Q2_34 + Q2_35 + Q2_36 + Q2_41 + Q2_45
Freedom =~ Q2_9 + Q2_10 + Q2_15 + Q2_17 + Q2_18 + Q2_23 + Q2_24 + Q2_26 + Q2_28 + Q2_31 + Q2_46
Responsibility =~ Q2_1 + Q2_6 + Q2_7 + Q2_8 + Q2_16 + Q2_20 + Q2_22 + Q2_25 + Q2_29 + Q2_30 + Q2_37 + Q2_38 + Q2_39
'
overall.fit = cfa(overallmodel, 
                  data=nomissnop, 
                  meanstructure = TRUE)

summary(overall.fit, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE)

####separate group models####
##notrandom
overall.fit.nr = cfa(overallmodel, 
                     data=notrandom, 
                     meanstructure = TRUE)

summary(overall.fit.nr, 
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


####multi group testing####
###measurement invariance
options(scipen = 999)

multisteps = measurementInvariance(overallmodel, 
                                   data = nomissnop, 
                                   group = "Source",
                                   strict = T)

fitmeasures(multisteps$fit.configural)
fitmeasures(multisteps$fit.loadings)
fitmeasures(multisteps$fit.intercepts)
fitmeasures(multisteps$fit.residuals)

