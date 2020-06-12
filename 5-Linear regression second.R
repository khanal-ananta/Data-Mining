#####################################################################################################################
#--------------------- Linear regression second--------------------------------------------------------------------
#####################################################################################################################
remove(list = ls())

#####################################################################################################################
#--------------------exercise 1 -------------------------------------------------------------------------------------
# For the shoesize/bodysize example in Assignment 4 construct the following matrices:

shoesize = c(38,38,39,39,40,40,41,41,42,42)
shoesize
bodysize = c(153,161,167,169,173,176,182,181,188,189)
bodysize

# X is a matrix with 2 columns, one of which is filled with 1’s, and the other is the vector of x-values (shoesize-values),
x <- matrix(c(1,1,1,1,1,1,1,1,1,1,shoesize), nrow=10,ncol=2)
x

# X T , which then has two rows (the first of which is just 1’s) consisting of 10 values,
xt = t(x)
xt

# X T X, whichis a 2 by 2 matrix,
xtx = xt%*%x
xtx

# (X T X) -1 , which of course is also 2 by 2,
inv = solve(xtx)
inv

#X T y where y is the vector of y-values (bodysize-values) – X T y will be a 2-vector,
xty = xt%*%bodysize
xty

# b = (X T X) -1 X T y, this will also be a 2-vector of coefficients,
b = inv%*%xty
b


intercept <- b[1]
intercept  # -132.1

slope <- b[2]
slope   # 7.65


# y pred = X(X T X) -1 X T y, this will of course be a 10-vector of bodysize-predictions.
x%*%(solve(xt%*%x)%*%(xt%*%bodysize))

ypred = x%*%b
ypred 

preY <- matrix(ypred, nrow = 1, byrow = TRUE)
preY  # 158.6 158.6 166.25 166.25 173.9 173.9 181.55 181.55 189.2 189.2

# Now calculate the residual standard error using

# sum of squre of residuals
SS_res = sum((ypred-bodysize)^2) 
SS_res  # 52.45

df = (length(bodysize)-2)

# mean squre of error
MS_res = SS_res/df
MS_res # 6.55625

# residual standard error
RSE = sqrt(MS_res)
RSE   # 2.560518


# Using the two diagonal elements of (X T X) -1 , calculate the standard error of each of the two coefficients 
# (don't forget to take sqare roots).

std_intercept = sqrt(inv[1])*RSE
std_intercept  # 22.91627

std_slope = sqrt(inv[4])*RSE
std_slope  # 0.5725491


# Now calculate a t-value for each coefficient (i.e coefficient / its standard error), and fromthis a p-value, 
# using the t-distribution, pt(...,8,lower.tail=FALSE) for 8 degrees of freedom.

t_intercept = intercept/std_intercept
t_intercept  # -5.764462
t_slope = slope/std_slope
t_slope  # 13.3613


p_intrcept1 = pt(t_intercept,df,lower.tail = FALSE)
p_intrcept1  # 0.999789

p_intrcept2 = 2 * pt(t_intercept,df,lower.tail = TRUE)
p_intrcept2 # 0.000421948  # choose this as its so low


p_slope1 = 2 * pt(t_slope,df, lower.tail = FALSE)
p_slope1  # 9.415997e-07

p_slope2 = pt(t_slope,df, lower.tail = TRUE)
p_slope2  # 0.9999995


#--------------------exercise 2 -------------------------------------------------------------------------------------
# Replay the above example using the lm( )- function. Do the p-values coincide? Why or why not?
# The results are the same
y_new = lm(bodysize~shoesize)
summary(y_new)  # p-value: 9.416e-07

lm_pVal_b0 = summary(y_new)$coefficients[1,4] 
lm_pVal_b0  # 0.000421948
lm_pVal_b1 = summary(y_new)$coefficients[2,4] 
lm_pVal_b1  # 9.415997e-07

# as we can see both ways gives the same results.

# Do the following diagnostic plots:
# (a) x-values on the x-axis and residuals on the y-axis

par(mfrow = c(2,2)) # this gives four plots at a time

plot(shoesize, resid(y_new), main="Residuals vs Fitted",xlab="x-values", ylab="Residuals")

# predictions on the x axis and residuls on the y-axis

plot(fitted(y_new), resid(y_new), main="Residuals vs Fitted", xlab="Fitted values", ylab="Residuals")

# predictions on the x axis and observed values on the y-axis

plot(fitted(y_new), bodysize, main="Observed vs Fitted",xlab="Fitted values", ylab="Observed")

# histogram of the residuals,
hist(resid(y_new))

# normal scores plot to check for skewness, kurtosis and outliers
qqnorm(resid(y_new), main="Residuals Rankit Plot")
qqline(resid(y_new))
