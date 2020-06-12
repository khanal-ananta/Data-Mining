#####################################################################################################################
#--------------------- Contingency table or confusion matrix--------------------------------------------------------
#####################################################################################################################
remove(list = ls())


# permutaion = possible ways a number or things can be ordered.in r choose , p(n,r) = n ! / (n - r)!
# combination = order of selection matters here so c(n,r) = n ! / (n-r)! * r!

#####################################################################################################################
#--------------------exercise 1 -------------------------------------------------------------------------------------
# (A)----------------------------------------------------------------------------------------------------------------
# How many possible ways are there of choosing 3 days of one week?
#Choose 3 out of 7
#choose(n, k)
choose(7,3) # 35


ways1 <- (factorial(7) / factorial(4))
ways1 # 210

ways2 <- (factorial(7)/(factorial(4)*factorial(3)))
ways2  # 35

# since 3 days can be arrange in any case so its the case of combination

# (B)----------------------------------------------------------------------------------------------------------------
# If the probability of a medicament being effective for one person X is p=0,9, and if itcan be assumed that the 
# same p holds for all other persons, and if it can be assumed, that whether it works on one person is independent 
# of whether it works for another person (.. how would you formulate this in your capacity as a professional data analyst?)
# then: what's the probability of the medicament being ineffective for more than 2 people, i. e.for two or more people out
# of a (small) population of seven? Write the formula down, then use R and the binom( )- distribution to calculate the above.

#p = 0.9 // Medicant Effective
# CALCULATE P(MEDICANT INEFFECTIVE FOR TWO OR MORE PEOPLE) = P(2 OR MORE FAILURE)
# P(X) = 1 - P(1 FAILURE) - P (ZERO FAILURE)
# n = 7

# dbinom p(k success out of n)
P_TWO_OR_MORE_FAILURE <- 1 - dbinom(1,7,0.1) - dbinom(0,7,0.1)
P_TWO_OR_MORE_FAILURE # 0.1496944
# OR, pbinom (k,n,p) = P(upto k successes out of n)
P_TWO_OR_MORE_FAILURE <- 1 - pbinom(1,7,0.1)
P_TWO_OR_MORE_FAILURE # 0.1496944



#--------------------exercise 2 ------------------------------------------------------------------------------------------# Assume you tested the medicament on 8 people, 4 of whom received it, 4 others received a placebo (something that looks 
# like a medicament, but contains no active ingredient). Assume 3 of 4 med-receivers improved their illness after 1 week.
# But also 3 of 4 plac- receivers improved their illness after one week, .... hmm: obviously this medicament youdon't 
# really need! ok ill

# (A)---------------------------------------------------------------------------------------------------------------------
# Let x be the number of med-receivers that get healthy given the “marginal sums”, 4, 4, 6, 2 (as in the table). 
# Assuming independence, calculate the probability that x = 3, (careful: this has nothing to do with Ex. 1.) 
# Fill out the rest of the table. Do you think independence is true? Do you think independence is wanted?


# create a contigency table

contingencyTable <- function(x){
  matrix(c(x, 6-x, 4-x, x-2), ncol = 2, byrow = FALSE, dimnames = list( c("med", "pla"), c("ok","ill")))
}

# x= 3
table3=contingencyTable(3)
table3


# finding probability
prob <- function(k){
  (choose(4,k)*choose(4,6-k))/choose(8,6)
}


prob3 <- prob(3)
prob3  # 0.5714286

#Do you think independence is true? Do you think independence is wanted?

# since probability is relatively high the independence is true.



# (B)---------------------------------------------------------------------------------------------------------------------
# Fill out the table for x = 2, 3, 4 (more is not possible) and calculate the corresponding probabilities. Do they sum 
# up to 1?

table2 <- contingencyTable(2)
table2

table3 <- contingencyTable(3)
table3

table4 <- contingencyTable(4)
table4

prob2 <- prob(2)
prob2  # 0.2142857

prob3 <- prob(3)
prob3  # 0.5714286

prob4 <- prob(4)
prob4  # 0.2142857

sum <- prob2 + prob3 + prob4
sum  # 1

# the probability sums upto 1


#--------------------exercise 3 ------------------------------------------------------------------------------------------
# Use R and Fisher's exact test to check out the example above. Get help with help(fisher.test) . Use the function 
# BDtest( ) in the bdpv -package to calculate confidence intervals for sensitivty and specificity. Enlargen the 
# numbers to 20 in each group and try different effects. Try to really understand what's going on!

# FISHER's EXACT TEST


f2 <- fisher.test(table2, conf.int = T, alternative = "t")
f3 <- fisher.test(table3, conf.int = T, alternative = "t")
f4 <- fisher.test(table4, conf.int = T, alternative = "t")

summary(f2)
attributes(f2)

# p values

f2$p.value # 0.42
f3$p.value # 1
f4$p.value # 0.42

# since p value is higher so the datas provided are independent.

# one tailed
fisher.test(table2, conf.int = T, alternative="g")$p.value   # 1
fisher.test(table3, conf.int = T, alternative="g")$p.value   # .78
fisher.test(table4, conf.int = T, alternative="g")$p.value   # .21


# SENSITIVITY AND SPECIFICITY
# p(positive test results|illness) i.e probability of test to correctly identify illness called sensitivity
# p(negative test results|non-illness) i.e probability of test to correctly identify non- illness called specificity

help(package="bdpv")
library(bdpv)

# assumed prevalence pr
# nominal confidence level

NewTab<-matrix(c(2, 4, 2, 0), ncol=2)
test <- BDtest(xmat=NewTab, pr=0.1, conf.level = 0.95)
summary(test)
attributes(test)

test$INDAT
#                   True positive True negative
# Test positive             2             2
# Test negative             4             0

test$SESPDAT 
#Estimate                Lower 95% limit  Lower 97.5% limit    Upper 97.5% limit
#Sensitivity 0.3333333      0.06284989        0.04327187         0.7772219
#Specificity 0.0000000      0.00000000        0.00000000         0.8418861
# Conf Int for Spec and Sen and this also give specivity

test$PPVNPVDAT
#Estimate Lower 95% limit Lower 97.5% limit Upper 97.5% limit
#NPV 0.80798724       0.5409860        0.58123420         0.9457484
#PPV 0.05665546       0.0249463        0.02475575         0.1462819



# Englargened Table

NewTab2<-matrix(c(20, 20, 20, 20), ncol=2,byrow = TRUE)
colnames(NewTab2)<-c("OK","ILL")
rownames(NewTab2)<-c("MED","PLA")

NewTab2

# why the sum of left and right tail test is greater then one?
fisher.test(NewTab2, conf.int = T,alternative = "g")$p.value # 0.5883739
fisher.test(NewTab2, conf.int = T,alternative = "t")$p.value # 1
fisher.test(NewTab2, conf.int = T,alternative = "l")$p.value # 0.5883739


#--------------------exercise 4 ------------------------------------------------------------------------------------------
# For the enlargened table use the following formula and χ2-distribution for 1 degree offreedom to establish how a 
# table has to look to be significant.
# CHISQ TEST correct=T // for YATES correction
# n > 40 for one degree of freedom by pearson
# correct = T means apply continuity correction when computing the test statistics

CHI2=chisq.test(NewTab2, correct=T)

attributes(CHI2)
CHI2$expected
#     OK ILL
#MED 20  20
#PLA 20  20


CHI2$p.value  # 1

NewTab3<-matrix(c(20, 20, 20, 20), ncol=2)
chisq.test(NewTab3, correct=T)$p.value # 1 

# trying more extreme values
chisq.test(matrix(c(21, 19, 19, 21), ncol=2), correct=T)$p.value # .82
chisq.test(matrix(c(22, 18, 18, 22), ncol=2), correct=T)$p.value # .5chi
chisq.test(matrix(c(23, 17, 17, 23), ncol=2), correct=T)$p.value # .2
chisq.test(matrix(c(24, 16, 16, 24), ncol=2), correct=T)$p.value # .1175
chisq.test(matrix(c(25, 15, 15, 25), ncol=2), correct=T)$p.value # .044 <-- SIGNIFICANT


