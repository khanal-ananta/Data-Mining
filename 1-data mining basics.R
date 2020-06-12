#####################################################################################################################
#--------------------- Data mining basics ------------------------------------------------------------------------
#####################################################################################################################
remove(list = ls())

#####################################################################################################################

#--------------------exercise 1 ---------------------------------------------------------------------------
# Generate the following graph using the commands:

plot(x=seq(-3,3,0.1),y=dnorm(seq(-3,3,0.1)),type="l",xlab="x-axis",ylab="y-axis", main = "graph")

lines(c(1.645,1.645),c(0,dnorm(1.645)))

polygon(x=c(0,1.645,1.645,seq(1.6,-3,-0.1),0),
        y=c(0,0,dnorm(1.645),dnorm(seq(1.6,-3,-0.1)),0),col="red",xlab=" x-axis",ylab="y-axis")

# Then plot the chi2-distribution for df=7 and F-distribution for df1=df2=7:

# lwd parameter only change the color brightness

# for chi-square distribution
x <- seq(0,20,.01)
y <- dchisq(x,7)
plot(x,y, type = "l", col = "red" ,xlab = "x-value", ylab = "chi-square value",main = "chi-square distribution" ,lwd = 1)
lines(c(7,7), c(0,dchisq(7,7)),col = "red")
lines(c(14,14), c(0,dchisq(14,7)),col = "red")
polygon(x = c(14,20,seq(20,14,-.01)), y = c(0,0,dchisq(seq(20,14,-.01),7)), col = "yellow")

# for f-distribution
x <- seq(0,6,.01)
y <- df(x, df1 = 7, df2 = 7)
plot(x,y, type = "l", col = "red", xlab = "x value", ylab = "f-value",main = "f-distribution " ,lwd = 1)
lines(c(1,1), c(0,df(1,7,7)),col = "red")
lines(c(3.8,3.8), c(0, df(3.8,7,7)),col = "red")
polygon(x = c(3.8,6,seq(6,3.8,-0.1)), y = c(0,0,df(seq(6,3.8, -0.1),7,7)), xlab = "x-value", ylab = "f-value",col = "yellow")



#--------------------exercise 2 ----------------------------------------------------------------------------
# Use R to do the t-test for our favorite data set from last semester --> Import the data using copy in Open-office 
# and then the command: >wash<-read.delim(“clipboard”, dec=”,”) wash is now a so called data frame with two variables 
# probe 1 probe 2. Type > help(t.test) to get information on how to use t-test.


# header = true indicates in has a header
data1 <- read.delim(file = "clipboard", dec =  ", ",sep = "\t", header=TRUE)
data1


# reading from csv file
total_data <- read.csv(file="/home/ananta/Desktop/third semester/data mining/solutions/data1.csv", header=TRUE, sep=",")
total_data

data1  <- subset(total_data, select = c(1))
data1

data2  <- subset(total_data, select = c(2))
data2

t_calc <- (mean(data1$probe1)-mean(data2$probe2))/(sqrt(((sd(data1$probe1))^2 +(sd(data2$probe2))^2)/ 2) * sqrt(2/8))
t_calc  # 4.827092

# here we use right hand one tail test.
t_cric = qt(0.05, df=14 , lower.tail = FALSE)  
t_cric  # 1.76131

# since t_cric is less then t_calc so the test is significant

# by using formula
t_test <- t.test(total_data$probe1,total_data$probe2,conf.level = 0.95)
t_test

summary(t_test)
t_test$statistic # 4.827092 
t_test$p.value   # 0.000355763
t_test$estimate  # mean of x mean of y  151.875    80.625 
t_test$parameter # df 12.66267 

# since p vlaue is smaller then 0.05 so the test is significant.

