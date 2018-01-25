##Set working directory
setwd("G:/GA/Scales")
master = read.csv("Meaning_Scales_Existance_RN_RR_Done_Paper.csv")
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
selfdis <- rowSums(noout[, c(6,8,22,35,43,45,46,47)])
summary(selfdis)

selftran <- rowSums(noout[, c(5,7,14,15,16,17,24,30,36,37,38,39,44,48)])
summary(selftran)

freedom <- rowSums(noout[, c(12,13,18,20,21,26,27,29,31,34,49)])
summary(freedom)

response <- rowSums(noout[, c(4,9,10,11,19,23,25,28,32,33,40,41,42)]) 
summary(response)


