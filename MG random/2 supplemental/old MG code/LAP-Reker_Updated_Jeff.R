##this data has the questions coded as Q22 but the script ha them as 103_
##wrong csv

setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_LAP_Reker_RR_RN_Paper.csv")
summary(master)

##Reverse Code. No Reverse Coding.
library(car)
master$Q22_16 = recode(master$Q22_16, "1='7'; 2='6'; 3='5'; 4='4'; 5='3'; 6='2'; 7='1'")

####DATA SCREENING####

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
##here I excluded the first two columns because they were 
##included for everyone as IDs, so I don't want to count that
##as part of the percent toward what they did
notypos = master
missing = apply(notypos[ , 3:46], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,47)]
dontcolumn = replacepeople[ , c(1,2,47)]

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
LP <- rowSums(noout[, c(14,22,23,30,31,38,39,46)])
LP

EV <- rowSums(noout[, c(4,12,16,20,28,34,40)])
EV

LC <- rowSums(noout[, c(17,25,41)])
LC

Death <- rowSums(noout[, c(19,27,35,43)])
Death

WM <- rowSums(noout[, c(7,9,15,36,44,47)])
WM

GS <- rowSums(noout[, c(21,24,29,37)])
GS

FMF <- rowSums(noout[, c(5,8,13,45)])
FMF

##exclude paper people using noout dataset 
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

##overallmodel 
names(nomissnop)
library(lavaan)
overallmodel = '
LP =~ Q22_11 + Q22_19 + Q22_20 + Q22_27 + Q22_28 + Q22_35 + Q22_36 + Q22_43
EV =~ Q22_1 + Q22_9 + Q22_13 + Q22_17 + Q22_25 + Q22_31 + Q22_37
LC =~ Q22_14 + Q22_22 + Q22_38
Death =~ Q22_16 + Q22_24 + Q22_32 + Q22_40
WM =~ Q22_4 + Q22_6 + Q22_12 + Q22_33 + Q22_41 + Q22_44 
GS =~ Q22_18 + Q22_21 + Q22_26 + Q22_34 
FMF =~ Q22_2 + Q22_5 + Q22_10 + Q22_42
'
fit = cfa(overallmodel, 
          data = nomissnop, 
          meanstructure = TRUE)

fitMeasures(fit)
table(nomissnop$Source)
inspect(fit,"cov.lv")

##subset 
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
