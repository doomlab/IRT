library(car);library(memisc)

##set working directory for Dr. B
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/IRT/NCHC riley/data/rachel PIL things") 
PIL = read.csv("PIL.csv")
week1.file.04.29.10 = read.delim("week1 file 04.29.10.csv")
meaning.working.file.5.10.10 = read.delim("meaning working file 5-10-10.csv")
SES.Mike.and.Mike.Data = read.delim("SES Mike and Mike Data.csv")
logotherapy_data_factor_analysis_study = read.delim("logotherapy_data_factor_analysis_study.csv")

##Rachel import data
PIL <- read.csv("~/Downloads/PIL.csv")
week1.file.04.29.10 <- read.delim("~/Downloads/week1 file 04.29.10.csv")
meaning.working.file.5.10.10 <- read.delim("~/Downloads/meaning working file 5-10-10.csv")
SES.Mike.and.Mike.Data <- read.delim("~/Downloads/SES Mike and Mike Data.csv")
logotherapy_data_factor_analysis_study <- read.delim("~/Downloads/logotherapy_data_factor_analysis_study.csv")

##reverse coding the PIL
PIL[ , c("PIL2", "PIL5", "PIL7", "PIL10", "PIL14", "PIL15", "PIL17", "PIL18", "PIL19")] = 8 - PIL[ , c("PIL2", "PIL5","PIL7", "PIL10", "PIL14", "PIL15", "PIL17", "PIL18", "PIL19")]

##Getting Demo Info###
Student.Master <- read.csv("~/Downloads/Student Master.csv")
colnames(Student.Master)
Student.Master = Student.Master[, c(3,4,14,15)]


summary(PIL)
summary(week1.file.04.29.10)
table(week1.file.04.29.10$age)
table(week1.file.04.29.10$gender)
table(week1.file.04.29.10$ethnicit)

summary(meaning.working.file.5.10.10)
table(meaning.working.file.5.10.10$age)
table(meaning.working.file.5.10.10$gender)
table(meaning.working.file.5.10.10$ethnic)

summary(SES.Mike.and.Mike.Data)
table(SES.Mike.and.Mike.Data$AGE)
table(SES.Mike.and.Mike.Data$SEX)
table(SES.Mike.and.Mike.Data$RaceEthnicity)

summary(logotherapy_data_factor_analysis_study)
table(logotherapy_data_factor_analysis_study$sex)
table(logotherapy_data_factor_analysis_study$years)
table(logotherapy_data_factor_analysis_study$ethnicit)

##subsetting the PIL
##Zero = notrandom, One = random, Two = paper
PIL$Source = factor(PIL$Source, levels = c(0,1,2),
                             labels = c("Zero", "One", "Two"))
Zero = subset(PIL, Source == "Zero")
One = subset(PIL, Source == "One")
Two = subset(PIL, Source == "Two")

##removing all columns except PIL questions
notrandomcomputer = Zero[, c(3:22)]
randomcomputer = One[, c(3:22)]
paper = Two[, c(3:22)]

##removing all columns except PIL for other data sets
Paper1dropcolumns = week1.file.04.29.10[, c(8:27)]
colnames(meaning.working.file.5.10.10)
Paper2dropcolumns = meaning.working.file.5.10.10[, c(55:74)]
Paper3dropcolumns = SES.Mike.and.Mike.Data[, c(2:21)]
Paper4dropcolumns = logotherapy_data_factor_analysis_study[, c(1:20)]


###combinging all paper data sets
allpaper = rbind(Paper1dropcolumns, Paper2dropcolumns, Paper3dropcolumns, Paper4dropcolumns, paper)

##data screening
##each dataset separately (and separate not random/random/paper)
summary(randomcomputer)
summary(notrandomcomputer)
summary(allpaper)

apply(randomcomputer, 2, table)
apply(notrandomcomputer, 2, table)
apply(allpaper, 2, table)

##All paper - PIL2 has a decimal,PIL7 has a decimal,PIL10 has a decimal, PIL13 has a decimal,PIL14, PIL15, PIL16, PIL17
allpaper$PIL2 = as.integer(allpaper$PIL2)
allpaper$PIL5 = as.integer(allpaper$PIL5)
allpaper$PIL7 = as.integer(allpaper$PIL7)
allpaper$PIL10 = as.integer(allpaper$PIL10)
allpaper$PIL13 = as.integer(allpaper$PIL13)
allpaper$PIL14 = as.integer(allpaper$PIL14)
allpaper$PIL15 = as.integer(allpaper$PIL15)
allpaper$PIL16 = as.integer(allpaper$PIL16)
allpaper$PIL17 = as.integer(allpaper$PIL17)
allpaper$PIL18 = as.integer(allpaper$PIL18)
allpaper$PIL19 = as.integer(allpaper$PIL19)


######Paper#####
##missing data
percentmiss = function(x){ sum(is.na(x)/length(x)) * 100}

##participants (row)
missing = apply(allpaper, 1, percentmiss)
table(missing)
replacepeople = subset(allpaper, missing <= 5)
nopeople  = subset(allpaper, missing > 5)
summary(replacepeople)
summary(nopeople)

##variables (columns)
apply(replacepeople, 2, percentmiss)

library(mice)
tempnomiss = mice(replacepeople)
replacedallpaper = complete(tempnomiss, 1)
summary(replacedallpaper)
##outliers
mahal = mahalanobis(replacedallpaper[ , ],
                    colMeans(replacedallpaper[ , ], na.rm = T),
                    cov(replacedallpaper[ , ], use = "pairwise.complete.obs"))

cutoff = qchisq(1-.001, ncol(replacedallpaper[ , ]))
ncol(replacedallpaper[ , ])
cutoff
summary(mahal < cutoff)
nooutpaper = subset(replacedallpaper, mahal < cutoff)

##assumptions
##additivity
correl = cor(nooutpaper[ , ])
symnum(correl)

finalpaper = nooutpaper

##set up for assumptions
random = rchisq(nrow(finalpaper), 7)
fake = lm(random ~ ., data = finalpaper)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
library(moments)
skewness(finalpaper[ , ], na.rm = T)
kurtosis(finalpaper[ , ], na.rm = T)
hist(standardized)


##linearity
qqnorm(standardized)
abline(0,1)

##homog + s
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

######Random#####
##missing data
##participants (row)
missingrandomcomputer = apply(randomcomputer, 1, percentmiss)
table(missingrandomcomputer)
replacepeoplerandom = subset(randomcomputer, missingrandomcomputer <= 5)
nopeoplerandom  = subset(randomcomputer, missingrandomcomputer > 5)
summary(replacepeoplerandom)
summary(nopeoplerandom)

##variables (column)
apply(replacepeoplerandom, 2, percentmiss)
replacecolumnrandom = replacepeoplerandom[ , -15]
dontcolumnrrandom = replacepeoplerandom[ , 15]

library(mice)
tempnomissrandom = mice(replacecolumnrandom)
replacedrandom_temp = complete(tempnomissrandom, 1)

replacedrandom = cbind(replacedrandom_temp[ , 1:14], 
                       PIL15 = dontcolumnrrandom, 
                       replacedrandom_temp[ , 15:19])
summary(replacedrandom)



##outliers
mahalrandom = mahalanobis(replacedrandom[ , ],
                    colMeans(replacedrandom[ , ], na.rm = T),
                    cov(replacedrandom[ , ], use = "pairwise.complete.obs"))

cutoff = qchisq(1-.001, ncol(replacedrandom[ , ]))
ncol(replacedrandom[ , ])
cutoff
summary(mahalrandom < cutoff)
nooutrandom = subset(replacedrandom, mahalrandom < cutoff)

##assumptions
##additivity
correlrandom = cor(nooutrandom[ , ])
symnum(correlrandom)

finalrandom = nooutrandom

##set up for assumptions
randomrandom = rchisq(nrow(finalrandom), 7)
fakerandom = lm(randomrandom ~ ., data = finalrandom)
standardizedrandom = rstudent(fakerandom)
fittedrandom = scale(fakerandom$fitted.values)

##normality
library(moments)
skewness(finalrandom[ , ], na.rm = T)
kurtosis(finalrandom[ , ], na.rm = T)
hist(standardizedrandom)

##linearity
qqnorm(standardizedrandom)
abline(0,1)

##homog + s
plot(fittedrandom, standardizedrandom)
abline(0,0)
abline(v = 0)

######NOTRandom#####
##missing data
##participants (row)
missingnotrandomcomputer = apply(notrandomcomputer, 1, percentmiss)
table(missingnotrandomcomputer)
replacepeoplenotrandom = subset(notrandomcomputer, missingnotrandomcomputer <= 5)
nopeoplenotrandom  = subset(notrandomcomputer, missingnotrandomcomputer > 5)
summary(replacepeoplenotrandom)
summary(nopeoplenotrandom)

##variables (columns)
apply(replacepeoplenotrandom, 2, percentmiss)

library(mice)
tempnomissnotrandom = mice(replacepeoplenotrandom)
replacednotrandom = complete(tempnomissnotrandom, 1)
summary(replacednotrandom)
##outliers
mahalnotrandom = mahalanobis(replacednotrandom[ , ],
                          colMeans(replacednotrandom[ , ], na.rm = T),
                          cov(replacednotrandom[ , ], use = "pairwise.complete.obs"))

cutoff = qchisq(1-.001, ncol(replacednotrandom[ , ]))
ncol(replacednotrandom[ , ])
cutoff
summary(mahalnotrandom < cutoff)
nooutnotrandom = subset(replacednotrandom, mahalnotrandom < cutoff)

##assumptions
##additivity
correlnotrandom = cor(nooutnotrandom[ , ])
symnum(correlnotrandom)

finalnotrandom = nooutnotrandom

##set up for assumptions
randomnotrandom = rchisq(nrow(finalnotrandom), 7)
fakenotrandom = lm(randomnotrandom ~ ., data = finalnotrandom)
standardizednotrandom = rstudent(fakenotrandom)
fittednotrandom = scale(fakenotrandom$fitted.values)

##normality
library(moments)
skewness(finalnotrandom[ , ], na.rm = T)
kurtosis(finalnotrandom[ , ], na.rm = T)
hist(standardizednotrandom)

##linearity
qqnorm(standardizednotrandom)
abline(0,1)

##homog + s
plot(fittednotrandom, standardizednotrandom)
abline(0,0)
abline(v = 0)




####covariance testing####
library(monomvn)
###make covariance tables
not_cor = cov(finalnotrandom, use="pairwise")
rand_cor = cov(finalrandom, use="pairwise")
paper_cor = cov(finalpaper, use="pairwise")

#mean tables
notrandomm = unlist(sapply(finalnotrandom, function(cl) list(means=mean(cl,na.rm=TRUE))))
randomm = unlist(sapply(finalrandom, function(cl) list(means=mean(cl,na.rm=TRUE))))
paperm = unlist(sapply(finalpaper, function(cl) list(means=mean(cl,na.rm=TRUE))))

##rsme mean, cov, mean, cov
rmse.muS(notrandomm, not_cor, paperm, paper_cor)
rmse.muS(notrandomm, not_cor, randomm, rand_cor)
##figure out how to do standardized residuals

####t test by item###

##i think this should be paired T because the items are paired
library(effsize)
t.test(notrandomm, 
       randomm,
       var.equal = T,
       paired = T)

cohen.d(notrandomm, 
        randomm,
        pooled = T,
        paired = T)

mean(notrandomm)
mean(randomm)
sd(notrandomm)
sd(randomm)
options(scipen = 999)
t.test(notrandomm, 
       paperm,
       var.equal = T,
       paired = T)

cohen.d(notrandomm, 
       paperm,
       pooled = T,
       paired = T)

mean(notrandomm)
mean(paperm)
sd(notrandomm)
sd(paperm)

####effect sizes####
##run effsize.R first to get this to work (put in folder)
##not random and random
d.deptavg(m1 = 5.15, sd1 = 0.48,
          m2 = 5.00, sd2 = 0.73, n = 20,
          a = .05, k = 2)

##notrandom and paper
d.deptavg(m1 = 5.15, sd1 = 0.48,
          m2 = 5.44, sd2 = 0.46, n = 20,
          a = .05, k = 2)

##if significant and effect size CI does not cross zero 
##run individual t-tests by item 

cohen.d(finalnotrandom$PIL1, 
       finalpaper$PIL1,
       pooled = T,
       paired = F)

cohen.d(finalnotrandom$PIL2, 
        finalpaper$PIL2,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL3, 
        finalpaper$PIL3,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL4, 
        finalpaper$PIL4,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL5, 
        finalpaper$PIL5,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL6, 
        finalpaper$PIL6,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL7, 
        finalpaper$PIL7,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL8, 
        finalpaper$PIL8,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL9, 
        finalpaper$PIL9,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL10, 
        finalpaper$PIL10,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL11, 
        finalpaper$PIL11,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL12, 
        finalpaper$PIL12,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL13, 
        finalpaper$PIL13,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL14, 
        finalpaper$PIL14,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL15, 
        finalpaper$PIL15,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL16, 
        finalpaper$PIL16,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL17, 
        finalpaper$PIL17,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL18, 
        finalpaper$PIL18,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL19, 
        finalpaper$PIL19,
        pooled = T,
        paired = F)

cohen.d(finalnotrandom$PIL20, 
        finalpaper$PIL20,
        pooled = T,
        paired = F)


####total scores####
###No total scores - need to create these
finalpaper$total = apply(finalpaper, 1, sum)
finalnotrandom$total = apply(finalnotrandom, 1, sum)
finalrandom$total = apply(finalrandom, 1, sum)

t.test(finalpaper$total, finalnotrandom$total,
       var.equal = T,
       paired = F)

cohen.d(finalpaper$total, finalnotrandom$total,
       pooled = T,
       paired = F)

t.test(finalnotrandom$total, finalrandom$total,
       var.equal = T,
       paired = F)

cohen.d(finalnotrandom$total, finalrandom$total,
        pooled = T,
        paired = F)

mean(finalnotrandom$total)
mean(finalrandom$total)
mean(finalpaper$total)
sd(finalnotrandom$total)
sd(finalrandom$total)
sd(finalpaper$total)

####effect sizes####
###not random vs. random##

d.indt(m1 = 103.03, sd1 = 18.29, n1 = 20,
       m2 = 100.06, sd2 = 15.43, n2 =20,
       a = .05, k = 2)

###not random vs. paper##
d.indt(m1 = 103.03, sd1 = 18.29, n1 = 20,
       m2 = 108.96, sd2 = 14.14, n2 =20,
       a = .05, k = 2)

##Not random means by item##
mean(finalnotrandom$PIL1)
mean(finalnotrandom$PIL2)
mean(finalnotrandom$PIL3)
mean(finalnotrandom$PIL4)
mean(finalnotrandom$PIL5)
mean(finalnotrandom$PIL6)
mean(finalnotrandom$PIL7)
mean(finalnotrandom$PIL8)
mean(finalnotrandom$PIL9)
mean(finalnotrandom$PIL10)
mean(finalnotrandom$PIL11)
mean(finalnotrandom$PIL12)
mean(finalnotrandom$PIL13)
mean(finalnotrandom$PIL14)
mean(finalnotrandom$PIL15)
mean(finalnotrandom$PIL16)
mean(finalnotrandom$PIL17)
mean(finalnotrandom$PIL18)
mean(finalnotrandom$PIL19)
mean(finalnotrandom$PIL20)


##Not random sd's by item##
sd(finalnotrandom$PIL1)
sd(finalnotrandom$PIL2)
sd(finalnotrandom$PIL3)
sd(finalnotrandom$PIL4)
sd(finalnotrandom$PIL5)
sd(finalnotrandom$PIL6)
sd(finalnotrandom$PIL7)
sd(finalnotrandom$PIL8)
sd(finalnotrandom$PIL9)
sd(finalnotrandom$PIL10)
sd(finalnotrandom$PIL11)
sd(finalnotrandom$PIL12)
sd(finalnotrandom$PIL13)
sd(finalnotrandom$PIL14)
sd(finalnotrandom$PIL15)
sd(finalnotrandom$PIL16)
sd(finalnotrandom$PIL17)
sd(finalnotrandom$PIL18)
sd(finalnotrandom$PIL19)
sd(finalnotrandom$PIL20)

##Random means by item##
mean(finalrandom$PIL1)
mean(finalrandom$PIL2)
mean(finalrandom$PIL3)
mean(finalrandom$PIL4)
mean(finalrandom$PIL5)
mean(finalrandom$PIL6)
mean(finalrandom$PIL7)
mean(finalrandom$PIL8)
mean(finalrandom$PIL9)
mean(finalrandom$PIL10)
mean(finalrandom$PIL11)
mean(finalrandom$PIL12)
mean(finalrandom$PIL13)
mean(finalrandom$PIL14)
mean(finalrandom$PIL15)
mean(finalrandom$PIL16)
mean(finalrandom$PIL17)
mean(finalrandom$PIL18)
mean(finalrandom$PIL19)
mean(finalrandom$PIL20)

##Random sd'd by item##
sd(finalrandom$PIL1)
sd(finalrandom$PIL2)
sd(finalrandom$PIL3)
sd(finalrandom$PIL4)
sd(finalrandom$PIL5)
sd(finalrandom$PIL6)
sd(finalrandom$PIL7)
sd(finalrandom$PIL8)
sd(finalrandom$PIL9)
sd(finalrandom$PIL10)
sd(finalrandom$PIL11)
sd(finalrandom$PIL12)
sd(finalrandom$PIL13)
sd(finalrandom$PIL14)
sd(finalrandom$PIL15)
sd(finalrandom$PIL16)
sd(finalrandom$PIL17)
sd(finalrandom$PIL18)
sd(finalrandom$PIL19)
sd(finalrandom$PIL20)


##Paper means by item###
mean(finalpaper$PIL1)
mean(finalpaper$PIL2)
mean(finalpaper$PIL3)
mean(finalpaper$PIL4)
mean(finalpaper$PIL5)
mean(finalpaper$PIL6)
mean(finalpaper$PIL7)
mean(finalpaper$PIL8)
mean(finalpaper$PIL9)
mean(finalpaper$PIL10)
mean(finalpaper$PIL11)
mean(finalpaper$PIL12)
mean(finalpaper$PIL13)
mean(finalpaper$PIL14)
mean(finalpaper$PIL15)
mean(finalpaper$PIL16)
mean(finalpaper$PIL17)
mean(finalpaper$PIL18)
mean(finalpaper$PIL19)
mean(finalpaper$PIL20)

###Paper sd's by item###
sd(finalpaper$PIL1)
sd(finalpaper$PIL2)
sd(finalpaper$PIL3)
sd(finalpaper$PIL4)
sd(finalpaper$PIL5)
sd(finalpaper$PIL6)
sd(finalpaper$PIL7)
sd(finalpaper$PIL8)
sd(finalpaper$PIL9)
sd(finalpaper$PIL10)
sd(finalpaper$PIL11)
sd(finalpaper$PIL12)
sd(finalpaper$PIL13)
sd(finalpaper$PIL14)
sd(finalpaper$PIL15)
sd(finalpaper$PIL16)
sd(finalpaper$PIL17)
sd(finalpaper$PIL18)
sd(finalpaper$PIL19)
sd(finalpaper$PIL20)


mean(finalnotrandom$total)
mean(finalrandom$total)
mean(finalpaper$total)
sd(finalnotrandom$total)
sd(finalrandom$total)
sd(finalpaper$total)

library(BayesFactor)
##Bayes factor
library(effsize)
##run it on totals? 
ttestBF(x = finalnotrandom$total, y = finalrandom$total, paired = FALSE)
ttestBF(x = finalnotrandom$total, y = finalpaper$total, paired = FALSE)

library(TOSTER)
TOSTtwo(m1 = mean(finalnotrandom$total), m2 = mean(finalrandom$total),
        sd1 = sd(finalnotrandom$total), sd2 = sd(finalrandom$total),
        n1 = length(na.omit(finalnotrandom$total)), n2 = length(na.omit(finalrandom$total)),
        low_eqbound_d = -.3, high_eqbound_d = .3,
        var.equal = TRUE, alpha = .05)

TOSTtwo(m1 = mean(finalnotrandom$total), m2 = mean(finalpaper$total),
        sd1 = sd(finalnotrandom$total), sd2 = sd(finalpaper$total),
        n1 = length(na.omit(finalnotrandom$total)), n2 = length(na.omit(finalpaper$total)),
        low_eqbound_d = -.3, high_eqbound_d = .3,
        var.equal = TRUE, alpha = .05)
