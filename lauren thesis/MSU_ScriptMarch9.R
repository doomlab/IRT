##And so it begins....

setwd("~/Dropbox")
master = read.csv("all_data_combined_edit.csv")

####Libraries Needed####
library(mice) 
library(lavaan)
library(psych) 
library(car)

##get the columns you need
columns = c(grep("PIL", colnames(master)), grep("PVS", colnames(master)), 
            grep("RS", colnames(master)), grep("MLQ", colnames(master)),
            grep("MILQ", colnames(master)), 14, 16:19 )

master = master[ , columns]
master = master[ , -c(1:5)]

##figure out which mlq to use
master$MLQ1[!is.na(master$MILQ1)] = master$MILQ1[!is.na(master$MILQ1)]
master$MLQ2[!is.na(master$MILQ2)] = master$MILQ2[!is.na(master$MILQ2)]
master$MLQ3[!is.na(master$MILQ3)] = master$MILQ3[!is.na(master$MILQ3)]
master$MLQ4[!is.na(master$MILQ4)] = master$MILQ4[!is.na(master$MILQ4)]
master$MLQ5[!is.na(master$MILQ5)] = master$MILQ5[!is.na(master$MILQ5)]
master$MLQ6[!is.na(master$MILQ6)] = master$MILQ6[!is.na(master$MILQ6)]
master$MLQ7[!is.na(master$MILQ7)] = master$MILQ7[!is.na(master$MILQ7)]
master$MLQ8[!is.na(master$MILQ8)] = master$MILQ8[!is.na(master$MILQ8)]
master$MLQ9[!is.na(master$MILQ9)] = master$MILQ9[!is.na(master$MILQ9)]
master$MLQ10[!is.na(master$MILQ10)] = master$MILQ10[!is.na(master$MILQ10)]

##drop the other MLQ
master = master[ , -c(69:78)]

##get the short form 3, 4, 8, 20 
master = master[ , -c(1,2,5,6,7,9:19)]

notypos = master

####recode#####
##recodeBRS
notypos$BRS2 = car::recode(notypos$BRS2, "1=5; 2=4; 3=3; 4=2; 5=1")
notypos$BRS4 = car::recode(notypos$BRS4, "1=5; 2=4; 3=3; 4=2; 5=1")
notypos$BRS6 = car::recode(notypos$BRS6, "1=5; 2=4; 3=3; 4=2; 5=1")

##recodeMLQ
notypos$MLQ9 = car::recode(notypos$MLQ9, "1=7; 2=6; 3=5;4=4; 5=3; 6=2; 7=1")

##recodePVS
notypos$PVS1 = car::recode(notypos$PVS1, "1=0; 2=1;3=2; 4=3")
notypos$PVS2 = car::recode(notypos$PVS2, "1=0; 2=1;3=2; 4=3")
notypos$PVS3 = car::recode(notypos$PVS3, "1=0; 2=1;3=2; 4=3")
notypos$PVS4 = car::recode(notypos$PVS4, "1=0; 2=1;3=2; 4=3")
notypos$PVS5 = car::recode(notypos$PVS5, "1=0; 2=1;3=2; 4=3")
notypos$PVS6 = car::recode(notypos$PVS6, "1=0; 2=1;3=2; 4=3")
notypos$PVS7 = car::recode(notypos$PVS7, "1=0; 2=1;3=2; 4=3")
notypos$PVS8 = car::recode(notypos$PVS8, "1=0; 2=1;3=2; 4=3")
notypos$PVS9 = car::recode(notypos$PVS9, "1=0; 2=1;3=2; 4=3")
notypos$PVS10 = car::recode(notypos$PVS10, "1=0; 2=1;3=2; 4=3")
notypos$PVS11 = car::recode(notypos$PVS11, "1=0; 2=1;3=2; 4=3")
notypos$PVS12 = car::recode(notypos$PVS12, "1=0; 2=1;3=2; 4=3")
notypos$PVS13 = car::recode(notypos$PVS13, "1=0; 2=1;3=2; 4=3")
notypos$PVS14 = car::recode(notypos$PVS14, "1=0; 2=1;3=2; 4=3")
notypos$PVS15 = car::recode(notypos$PVS15, "1=0; 2=1;3=2; 4=3")
notypos$PVS16 = car::recode(notypos$PVS16, "1=0; 2=1;3=2; 4=3")
notypos$PVS17 = car::recode(notypos$PVS17, "1=0; 2=1;3=2; 4=3")
notypos$PVS18 = car::recode(notypos$PVS18, "1=0; 2=1;3=2; 4=3")

##reverse coding after recoding
notypos$PVS2 = car::recode (notypos$PVS2, "0=3; 1=2; 2=1; 3=0")
notypos$PVS4 = car::recode(notypos$PVS4, "0=3; 1=2; 2=1; 3=0")
notypos$PVS5 = car::recode (notypos$PVS5, "0=3; 1=2; 2=1; 3=0")
notypos$PVS7 = car::recode (notypos$PVS7, "0=3; 1=2; 2=1; 3=0")
notypos$PVS10 = car::recode (notypos$PVS10, "0=3; 1=2; 2=1; 3=0")
notypos$PVS12 = car::recode (notypos$PVS12, "0=3; 1=2; 2=1; 3=0")
notypos$PVS13 = car::recode (notypos$PVS13, "0=3; 1=2; 2=1; 3=0")
notypos$PVS15 = car::recode (notypos$PVS15, "0=3; 1=2; 2=1; 3=0")
notypos$PVS18 = car::recode (notypos$PVS18, "0=3; 1=2; 2=1; 3=0")

####accuracy categorical items (gender, Year in School)---table--checking for coding, incorrect label#### 
summary(notypos)
notypos$Gender = factor(notypos$Gender,
                        levels = c("male", "female"), 
                        labels = c("Male", "Female"))
colnames(notypos)[59] = "Grade"
notypos$Grade = factor(notypos$Grade, 
                       levels = c("freshman", "sophomore", "junior", "senior", "grad/other"), 
                       labels = c("Freshman", "Sophomore", "Junior", "Senior", "Graduate/Other"))

####missing brs####
library(mice)
percentmiss = function(x) {sum(is.na(x))/length(x)*100}
summary(notypos)

##participants (row)
####missing brs####
names(notypos)
missingbrs = apply(notypos[ , c(37:42)], 1, percentmiss)
missingbrs
table(missingbrs) # NONE with <= 5% missing - no impute

####missing rs14####
names(notypos)
missingrs14 = apply(notypos[ , c(23:36)], 1, percentmiss)
missingrs14
table(missingrs14)  # NONE with <= 5% missing - no impute

####missing MLQ####
names(notypos)
missingmlq = apply(notypos[ , c(43:52)], 1, percentmiss)
missingmlq
table(missingmlq)  # NONE with <= 5% missing - no impute

####missing PILSF####
names(notypos)
missingpilsf = apply(notypos[ , c(1:4)], 1, percentmiss)
missingpilsf
table(missingpilsf) # NONE with <= 5% missing - no impute

####missing PVS####
names(notypos)
missingpvs = apply(notypos[ , c(5:22)], 1, percentmiss)
missingpvs
table(missingpvs)  # NONE with <= 5% missing - no impute

## count NAs across all measures
notypos$totNA = apply(notypos[,c(1:52)], 1, function(x) sum(is.na(x)))
totalmissing = apply(notypos[,c(1:52)], 1, percentmiss)
table(totalmissing)

## count BRS across all measures
notypos$BRSna = apply(notypos[,c(37:42)], 1, function(x) sum(is.na(x)))

## count RS14 across all measures
notypos$RS14na = apply(notypos[,c(23:36)], 1, function(x) sum(is.na(x)))

## count NAs for MLQ
notypos$mlqNA = apply(notypos[,c(43:52)], 1, function(x) sum(is.na(x))) 

## count NAs for PILSF
notypos$pilsfNA = apply(notypos[,c(1:4)], 1, function(x) sum(is.na(x))) 

## count NAs for PVS
notypos$pvsA = apply(notypos[,c(5:22)], 1, function(x) sum(is.na(x))) 

##import data
replacepeople = subset(notypos, totalmissing <= 5)
nopeople = subset(notypos, totalmissing > 5) 

apply(replacepeople, 2, percentmiss) ##these are all ok to replace
replacecol = replacepeople[ , 1:52]
nocol = replacepeople[ , 53:63]

tempnomiss = mice(replacecol, seed = 395489) ##set seed so you get the same answer each time
nomiss_rows = complete(tempnomiss, 1)

nomiss_cols = cbind(nomiss_rows, nocol)

#here i combined the people back who aren't complete
#they will get excluded in the mahal section if you exclude outliers
#that's fine, I might just write that you only 
#used people who were complete at this point 
nomiss = rbind(nomiss_cols, nopeople)

##outliers
##outliers screening##
##outliers can bias a parameter estimate and the error associated with that estimate
##outlier - case with extreme value on one vairable or multiples variables caused by data input error, not a population meant to sample, from the population but has really longs tails and extreme values
##remember there are two types of outliers , Lauren 
##Univariate-one DV; you may have lots of data and don't want to look each column at a time aka do multivariate
##Multivariable-use when you have multiple continuous variables or lots of DVS

##mahalanobis allow screen all the things all at once
##mahalanobis--> creates a distance from the mean of the mean aka mean of all the columns
##no set cut off rules: use chi square table, DF = number of variables (DV variables that you used to calculate Mahalanobis), use p < .001
##delete outliers? yes and no and it depends: I need the sample size aka fish...but like don't be that person who fishes
##first, figure out the factor columns, as all columns need to be int or num
mahal = mahalanobis(nomiss[, c(1:52)],
                    colMeans(nomiss[, c(1:52)], na.rm = T),
                    cov(nomiss[, c(1:52)], use = "pairwise.complete.obs"))
summary(mahal)

cutoff = qchisq(.999, ncol(nomiss[,c (1:52)]))
summary(mahal < cutoff) ##exclude falses, the outliers and create a noout data set
noout = subset(nomiss, mahal < cutoff) 

##Assumptions
##Additivity
correl = cor(noout [ , 1:52]) 
symnum(correl)

##set up for assumptions
random = rchisq(nrow(noout[ , 1:52]), 7)
fake = lm(random ~ ., data = noout[ , 1:52])
standardized = rstudent(fake)


##get the linearity plot
qqnorm(standardized)
abline(0,1)

##normality
##dont worry about skew kurtosis with this sample size
##multivariate normality
hist(standardized, breaks=15)

##homogeneity and homoscedaticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

final = noout

####you are good up to here####















##Randomly select half of the data
##https://stat.ethz.ch/pipermail/r-help/2007-February/125860.html
##Do I need to split it up into training and testing

###I need particpant descriptives                                        

####efa####
##see comments below - maybe do this

####bifactor model####

##basically we will have those three factors, do efa to see what loads, then the bifactor with those 3 and the overall factor
bifactormodel = '
resilience =~ Q3 + Q5 + Q10 + Q13 + Q16 + Q17 + Q21
hardiness =~ Q2 + Q4 + Q7 + Q9 + Q15 + Q19 + Q20
meaning =~ Q1 + Q6 + Q8 + Q11 + Q12 + Q14 + Q18
onefactor =~ Q3 + Q5 + Q10 + Q13 + Q16 + Q17 + Q21 + Q2 + Q4 + Q7 + Q9 + Q15 + Q19 + Q20 + Q1 + Q6 + Q8 + Q11 + Q12 + Q14 + Q18
'

###Scree plot???


##running model 
bifactormodelfit = cfa(bifactormodel, data = hierarchialcfa, orthogonal = TRUE, std.lv = TRUE)

##pics 
semPaths(bifactormodelfit, whatLabels = "std", layout = "tree")

##goodness of fit: RMSEA, SRMR, TLI, CFI

##model fit 
fitMeasures(bifactormodelfit)

##loadings 
summary(bifactormodelfit, standardized = TRUE, rsquare = TRUE)


##bifactor cfa
