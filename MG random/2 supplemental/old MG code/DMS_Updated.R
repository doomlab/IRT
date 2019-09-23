##Set working directory 
setwd("D:/GA/Scales")
master = read.csv("Meaning_Scales_DailyMeaningScale_RN_RR_Done.csv")
summary(master)

##Reverse Code. No reverse coding. Stopped here for scoring.
##fix text columns
master$Q86_8_TEXT = as.numeric(master$Q86_8_TEXT)
master$Q86_11_TEXT = as.numeric(master$Q86_11_TEXT)
master$Q86_13_TEXT = as.numeric(master$Q86_13_TEXT)

####DATA SCREENING####
##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:24], 1, percentmiss) 
table(missing)

##Replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##Figure out the columns to exclude 
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,25)]
dontcolumn = replacepeople[ , c(1,2,25)]

##MICE
tempnomiss = mice(replacecolumn)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

##(By your powers) Combine (I am Captain Planet!)
filledin_none = cbind(dontcolumn, nomiss)
summary(filledin_none)

##Outliers##
##Mahal
mahal = mahalanobis(filledin_none[ , -c(1,2,3)], 
                    colMeans(filledin_none[ , -c(1,2,3)], na.rm = TRUE),
                    cov(filledin_none[ , -c(1,2,3)], use="pairwise.complete.obs"))

summary(mahal)
cutoff = qchisq(.999,ncol(filledin_none[ , -c(1,2,3)])) 
summary(mahal < cutoff)
noout = filledin_none[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,3)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff 
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout[ , -c(1:2,3)])

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
##Daily Meaning average Q1 & Q2 (Col 4&5 in dataset)
daymean <- rowMeans(noout[,c("Q85_1", "Q85_2")])
summary(daymean)

daylife <- noout$Q85_3
summary(daylife)

Eud <- rowSums(noout[,c(7:13)])
summary(Eud)

Hed <- rowSums(noout[,c(14:25)])
summary(Hed)


##MGCFA
library(lavaan)
library(semTools)

##Factoring source##
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

##Model 
##we will need to exclude the two smaller subscales
##do not have enough questions for CFA
overallmodel = '
EudaimonicBehaviors =~ Q86_1_TEXT + Q86_2_TEXT + Q86_3_TEXT + Q86_4_TEXT + Q86_5_TEXT + Q86_6_TEXT + Q86_7_TEXT
HedonicBehaviors =~ Q86_8_TEXT + Q86_9_TEXT + Q86_10_TEXT + Q86_11_TEXT + Q86_12_TEXT + Q86_13_TEXT + Q86_14_TEXT +  Q86_15_TEXT + Q86_16_TEXT + Q86_17_TEXT + Q86_18_TEXT + Q86_19_TEXT
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

