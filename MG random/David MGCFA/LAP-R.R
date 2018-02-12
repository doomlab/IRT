setwd("G:/GA/Scales")
master = read.csv("Meaning_Scales_LAP_Revised_RR_RN_Paper.csv")
summary(master)
master = revised

##Reverse coded items 
library(car)
master$Q103_16 = recode(master$Q103_16, "1='7'; 2='6'; 3='5'; 4='4'; 5 ='3'; 6 ='2'; 7 ='1'")
master$Q103_49 = recode(master$Q103_49, "1='7'; 2='6'; 3='5'; 4='4'; 5 ='3'; 6 ='2'; 7 ='1'")

##missing

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

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
mahal = mahalanobis(filledin_none[ , -c(1,2,59)], 
                    colMeans(filledin_none[ , -c(1,2,59)], na.rm = TRUE),
                    cov(filledin_none[ , -c(1,2,59)], use="pairwise.complete.obs"))

cutoff = qchisq(.999,ncol(filledin_none[ , -c(1,2,59)])) 
summary(mahal < cutoff)
noout = filledin_none[ mahal < cutoff, ]

##additivity: correlations
correlations = cor(noout[,-c(1,2,59)], use="pairwise.complete.obs")
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
##be sure here not to include the ID columns!
fake = lm(random~., data=noout[ , -c(1:2,59)])

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
names(revised)
PersonalMeaning = rowSums(filledin_none[ , c(3, 4, 7, 9, 14, 18, 30, 31, 37, 39, 40, 50, 22)])
ExistentialVacuum = rowSums(filledin_none[ , c(8, 11, 35, 42, 6, 15, 44)])
DeathAcceptance = rowSums(filledin_none[ , c(10, 24, 27, 30, 34, 49, 46)])
GoalSeeking = rowSums(filledin_none[ , c(12, 16, 26, 36, 38, 43, 45)])
ChoiceResponsibleness = rowSums(filledin_none[ , c(5, 13, 19, 21, 25, 32, 41, 47 )])
SocialDesirability = rowSums(filledin_none[ , c(17, 20, 23, 28, 33, 48, 51, 52, 53, 54, 55, 56, 57, 58)])

##subsetting data
#Zero = notrandom, One = random, Two = paper
nooutnop = subset(noout, Source < 2)
nomissnop = subset(filledin_none, Source < 2)

notrandom = subset(nomissnop, Source == 0)
random = subset(nomissnop, Source == 1)

##model
overallmodel ='
PersonalMeaning =~ Q103_1 + Q103_2 + Q103_5 + Q103_7 + Q103_12 + Q103_16 + Q103_27 + Q103_29 + Q103_35 + Q103_37 + Q103_38 + Q103_48 + Q103_20
ExistentialVacuum =~ Q103_6 + Q103_9 + Q103_33 + Q103_40 + Q103_42 + Q103_13 + Q103_4
DeathAcceptance =~Q103_8 + Q103_22 + Q103_25 + Q103_28 + Q103_32 + Q103_47 + Q103_44
GoalSeeking =~Q103_10 + Q103_14 + Q103_24 + Q103_34 + Q103_36 + Q103_41 + Q103_43
ChoiceResponsibleness =~Q103_3 + Q103_11 + Q103_17 + Q103_19 + Q103_23 + Q103_30 + Q103_39 + Q103_45
SocialDesirability =~ Q103_15 +Q103_18 + Q103_21 + Q103_26 + Q103_31 + Q103_46 + Q103_49 + Q103_50 + Q103_51 + Q103_52 + Q103_53 + Q103_54 + Q103_55 + Q103_56
'
##our data did not include 57:63 but did include 4, 13, 20, 44, which were excluded on the thesis 


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