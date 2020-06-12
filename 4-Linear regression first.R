##      Assignment 4    ##
remove(list = ls())

#####################################################################################################################
#--------------------exercise 1 -------------------------------------------------------------------------------------
# Use lm ( ) -function to model the connection between shoe-size and body-size:

shoesize<-c(38,38,39,39,40,40,41,41,42,42)
bodysize<-c(153,161,167,169,173,176,182,181,188,189)
shoesize
bodysize

bspl<-data.frame(shoesize,bodysize)
bspl

# fits the model
fm<-lm(bodysize ~ shoesize, data=bspl) 
fm    # (Intercept) -132.10   shoesize  7.65


# outputs the model:
summary(fm) 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.6000 -0.8125  0.1250  1.7625  2.7500 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -132.1000    22.9163  -5.764 0.000422 ***
#   shoesize       7.6500     0.5725  13.361 9.42e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.561 on 8 degrees of freedom
# Multiple R-squared:  0.9571,	Adjusted R-squared:  0.9517 
# F-statistic: 178.5 on 1 and 8 DF,  p-value: 9.416e-07


#more information
anova(fm) # gives the anova table
vcov(fm)  # covarience matrix formed for model parameter
influence(fm) # regression diagonostic


# computes confidence intervals for one or more parameters in a fitted model
confint(fm)

#   2.5 %     97.5 %
# (Intercept) -184.945023 -79.254977
# shoesize       6.329699   8.970301

# graph of the model
plot(shoesize, bodysize, main="Scatterplot")

abline(fm)

cor(shoesize,bodysize) # 0.98 A strong correlation!



#--------------------exercise 2 -------------------------------------------------------------------------------------
# Modify the model by
# (A) fitting the model to the log of body-size, i.e. log(bodysize)
# log of body size

lgbody = log(bodysize)

# combines lgbody to the bspl 
cbind(bspl, lgbody)


lg<-lm(lgbody ~ shoesize, data=bspl)
lg   # (Intercept)   3.38181  shoesize        0.04437  

summary(lg)
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.037252 -0.003335  0.000466  0.011771  0.017844 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.381811   0.149762   22.58 1.57e-08 ***
#   shoesize    0.044365   0.003742   11.86 2.35e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01673 on 8 degrees of freedom
# Multiple R-squared:  0.9462,	Adjusted R-squared:  0.9394 
# F-statistic: 140.6 on 1 and 8 DF,  p-value: 2.349e-06

attributes(lg)
coefficients(lg)
confint(lg)
# 2.5 %     97.5 %
# (Intercept) 3.0364586 3.72716376
# shoesize    0.0357368 0.05299365


plot(shoesize, log(bodysize), col = "red", main="Scatterplot")
abline(lg, col = "red")

# (B) adding a square term to the model
# adding a sq term


sq = lm(bodysize ~ shoesize + I(shoesize^2), data=bspl)
sq
summary(sq)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.5286 -0.8464  0.0643  0.9893  3.4714 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   -988.1714   761.0281  -1.298    0.235
# shoesize        50.5071    38.0865   1.326    0.226
# I(shoesize^2)   -0.5357     0.4760  -1.125    0.298
# 
# Residual standard error: 2.519 on 7 degrees of freedom
# Multiple R-squared:  0.9637,	Adjusted R-squared:  0.9533 
# F-statistic: 92.87 on 2 and 7 DF,  p-value: 9.13e-06

plot( shoesize + I(shoesize^2), bodysize, xlab = "shoesize", ylab = "sq_bodysize", main = "plot")
abline(sq)



#-------------------- exercise 3 -----------------------------------------------------------------------------------------
# Usig the lm( ) -function in R fit the 2-factor-model of the first lecture in linear modelling:
# Calculate coefficients, predictions, residuals for the linear model. Then calculate coeffients for the interaction model.

y <-c(3,5,7,11)
x1 <-c(-1,1,-1,1)
x2 <-c(-1,-1,1,1)

xydata<-data.frame(y, x1,x2)
xydata

# fits the model
xylm<-lm(y ~ x1+x2, data=xydata) 
summary(xylm)

xylm$coefficients
        # (Intercept)          x1          x2 
        # 6.5                  1.5         2.5 

xylm$residuals
        # 1    2    3    4 
        # 0.5 -0.5 -0.5  0.5

xylm$fitted.values
        # 1    2    3    4 
        # 2.5  5.5  7.5 10.5

tData <- data.frame(y,x1,x2,xylm$fitted.values,xylm$residuals)
tData



x1 <-c(20,40,20,40) 
x2 <-c(20,20,30,30)

xydata<-data.frame(y, x1,x2)
xydata

lm1<-lm(y ~ x1+x2, data=xydata) 
summary(lm1)

lm1$coefficients
              # (Intercept)          x1          x2 
              # -10.50             0.15        0.50 

lm1$residuals
              # 1    2    3    4 
              # 0.5 -0.5 -0.5  0.5

lm1$fitted.values
              # 1    2    3    4 
              # 2.5  5.5  7.5 10.5 

# only coefficients get changed


plot(lm1)





