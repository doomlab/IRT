setwd("E:/GA/Scales")
master = read.csv("Meaning_Scales_PIL_RR_RN_Paper.csv")
summary(master)

####DATA SCREENING####
##accuracy##

##Missing Data##
library(mice)
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##Going by rows ONLY
notypos = master
missing = apply(notypos[ , 3:22], 1, percentmiss) 
table(missing)

##replace only the data that you should
replacepeople = notypos[ missing <= 5 , ]  
dontpeople = notypos[ missing > 5 , ]

##figure out the columns to exclude (survey data)
apply(replacepeople, 2, percentmiss)
replacecolumn = replacepeople[ , -c(1,2,23)]
dontcolumn = replacepeople[ , c(1,2,23)]

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

##Scoring
PIL <- rowSums(noout[, c(4:23)])
PIL

EL<- rowSums(noout[, c(5,8,10,13,20:22)])
EL

PL<- rowSums(noout[, c(6,11,23)])
PL

SF<- rowSums(noout[, c(6,7,11,23)])
SF

###RAN INTO A PROBLEM HERE!!!!  FOR SOME REASON THE CODE WON'T RUN FOR THIS AND
#I CAN'T SEEM TO FIGURE OUT WHY ALTHOUGH IT SEEMS LIKE IT SHOULD BE OBVIOUS...
###reliability 
#exciting = c("Q31_1", "Q35_1", "Q37_1", "Q40_1", "Q47_1", "Q48_1", "Q49_1")
#purposeful = c("Q33_1", "Q38_1", "Q50_1")
#shortform = c("Q33_1", "Q34_1", "Q38_1", "Q50_1")
#PILtot= (noout[ , ])

#library(psych)
#alpha(noout[, exciting])
#alpha(noout[ , purposeful])
#alpha(noout[ , shortform])
#alpha(noout[ , PILtot])

##totals
#harmony2 = c("Q111_3", "Q111_2", "Q111_6", "Q111_1")
#spirit2 = c("Q111_4", "Q111_5")
#library(car)


#noout$Q111_1 = recode(noout$Q110_1, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")
#noout$Q111_2 = recode(noout$Q110_2, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")
#noout$Q111_3 = recode(noout$Q110_3, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")
#noout$Q111_4 = recode(noout$Q110_4, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")
#noout$Q111_5 = recode(noout$Q110_5, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")
#noout$Q111_6 = recode(noout$Q110_6, "0 = 1.00; 1 = 2.25; 2 = 3.5; 3 = 4.75; 4 = 6.00")

#noout$haravg = apply(noout[ , harmony2], 1, mean)
#noout$lifeavg = apply(noout[ , life], 1, mean)
#noout$conavg = apply(noout[ , confusion], 1, mean)
#noout$spiritavg = apply(noout[ , spirit2], 1, mean)



