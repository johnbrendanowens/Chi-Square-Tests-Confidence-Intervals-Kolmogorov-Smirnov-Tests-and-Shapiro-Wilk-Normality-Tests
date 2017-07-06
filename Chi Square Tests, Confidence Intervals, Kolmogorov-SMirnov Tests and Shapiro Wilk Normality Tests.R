#7.32 
#The data set cfb (UsingR) contains a sampling of the data contained in the 
#Survey of Consumer Finances. For the variables AGE and INCOME find 95% confidence 
#intervals for the median.

data("cfb")
attach(cfb)
library(BSDA)
#Finding Confidence Intervals
SIGN.test(AGE, conf.level = 0.95)
SIGN.test(INCOME, conf.level = 0.95)

detach(cfb)

#8.17 The exec. pay (UsingR) data set contains data on the salaries of CEOs at 
#199 top companies in the United States. The amounts are in $ 10,000s. The data is 
#not symmetric. Do a sign test to determine whether the median pay is more than $220,000.

data("exec.pay")
#Confidence Level of Median
SIGN.test(exec.pay, md=22, alternative = "greater", conf.level = 0.95)

#8.18 Repeat the previous exercise, using the signed-rank test on the 
#log-transformed data. Do you reach the same conclusion?

log.execpay =log(exec.pay)
logmu = log(22)
#Confirming Previous results with Wilcoxon Sign Ranked Test 
wilcox.test(log.execpay, alternative = "greater", mu = logmu, conf.int=T)

#8.33 Water-quality researchers wish to measure biomass/chlorophyll ratio 
#for phytoplankton (in milligrams per liter of water). There are two possible 
#tests, one less expensive than the other. To see whether the two tests give 
#the same results, ten water samples were taken and each was measured both ways, 
#providing the data in Table 8.10. Do a t-test to see if there is a difference 
#in the means of the measured amounts. If you assume equal variances or a 
#paired test, explain why.

method.1 =c(45.9, 57.6, 54.9, 38.7, 35.7, 39.2, 45.9, 43.2, 45.4, 54.8)
method.2=c(48.2, 64.2, 56.8, 47.2, 43.7, 45.7,53.0, 52.0, 45.1, 57.5)
t.test(method.1, method.2, paired = T)
wilcox.test(method.1, method.2, paired = T, exact=F)

#9.2 Table 9.2 contains the results of a poll of 787 registered voters and the 
#actual race results (in percentages of total votes) in the 2003 
#gubernatorial recall election in California.
#Is the sample data consistent with the actual results? 
#Answer this using a test of significance.

poll = c(315, 197, 141, 39, 16, 79)
actualp = c(.486, .315, .125, .028, .006, .040)
names(poll)= c("Schwarzenegger", "Bustamante", "McClintock", "Camejo", "Huffington", "Other")

#Chi Square Test
gub.chitest = chisq.test(poll, p=actualp)
gub.chitest
#ChiSquare Expected
gub.chitest$expected
#Comparing Chi Square Expected versus actual Results
difference =gub.chitest$expected - poll
difference

#9.13 The air quality data set contains measurements of air quality in 
#New York City. We wish to see if ozone levels are independent of temperature. 
#First we gather the data, using complete. cases () to remove missing data from our 
#data set.
#Perform a chi-squared test of independence on the two variables te and oz. 
#Does the data support an assumption of independence? ASK

aq = airquality[complete.cases(airquality),]
attach(aq)
#Creating 4*4 Table
te = cut(Temp, quantile(Temp))
oz = cut(Ozone,quantile(Ozone))
table.te.oz = table(te,oz)
table.te.oz
#Conducting Chi Square Test
chisq.test(table.te.oz)

detach(aq)

#9.16 In two examples in Chapter 7, data on CEOs is compared. The data is 
#repeated in Table 9.12. Are the parent distributions the same? 
#Answer this using a test of significance. 

pay.2001 = c(110, 12, 2.5, 98, 1017, 540, 54, 4.3, 150, 432)
pay.2002 = c(312, 316, 175, 200, 92, 201, 428, 51, 289, 1126, 822)

ks.test(pay.2001, pay.2002)

#9.18 The brightness (UsingR) data set contains brightness measurements for 
#966 stars from the Hipparcos catalog. Is the data normal? Compare the result 
#with a significance test to the graphical investigation done by

data("brightness")
shapiro.test(brightness)
hist(brightness, prob=TRUE)
lines(density(brightness))
curve(dnorm(x, mean(brightness), sd(brightness)),
        add=TRUE)
qqnorm(brightness, main ="Brightness Normality"); qqline(brightness)
