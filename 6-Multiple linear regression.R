#####################################################################################################################
#--------------------- mulitple linear regression-------------------------------------------------------------------
#####################################################################################################################
remove(list = ls())

# since log 0 is indeterminant so we are using log(x + 1)
#####################################################################################################################

# Look at the data in the table Ass6-dm-data.xls. They consist of cell-count data for
# different leukocyte-subtypes of healthy children. The first column is age in age..months.. The
# counts are given in number of cells per μl of blood. They have been measured using
# flow-cytometry, in which cells are marked and coloured according cluster molecules that
# they present on their surface and then counted using powerful microscopes and software.
# Leukocyte is another name for “white blood cells”. Subtypes are Lymphocytes,
# Monocytes and Granulocytes (the latter have been excluded from this study). The
# Abbreviation CD means Cluster Designation and is a state of the art way to classify cells
# in the immune system. In fact CD14+ are monocytes, Lymphocytes have three subtypes:
# CD3+ are T-cells (cells that have been trainied in the thymus); CD19+ are B-cells (grow
# up in the bone marrow and are not trained outside); CD56+ are natural killer cells. T-cells
# are again differentiated into CD3+CD4+, so called T-helper cells and CD3+CD8+, so
# called cyto-toxic cells (details can be found at www.med.uni-duesseldorf.de/praedimm/immu.html)

#--------------------exercise 1 -------------------------------------------------------------------------------------
# Do the following “silly” experiment: Try to predict age from the blood counts. Is this
# possible? Use a model thats linear in all eight markers. (Don't forget to take out line 95,
# which for some reason contains no data.) Study residuals. Are there outliers? Look at the
# plot “residuals vs predicted” values. What do you notice? Look at the histogram of
# residuals. Do you think the residuals are normally distributed?
# Predict age for the children in the table “prediction data”.

# when there is data in .xml then just changed it to .csv by just doing save as

data1 <- read.csv("/home/ananta/me/study/third semester/DM/my solution/assignment/ass6.csv", header = T)
data1

#dropping the last column as it contains nothing
data2 <- data1[,-c(10)]
data2

#dropping row 94 and the last four rows
data <- data2[-c(104),]
data <- data[-c(103),]
data <- data[-c(102),]
data <- data[-c(101),]
my_data <- data[-c(94),]
my_data

# linear modelling of all data
y_Leukos = lm(my_data$age..months. ~ my_data$Leukos)
y_Lymphos = lm(age..months. ~ Lymphos,data=my_data)
y_CD14p = lm(age..months. ~ CD14p,data=my_data)
y_CD3p = lm(age..months. ~ CD3p,data=my_data)
y_CD4p = lm(age..months. ~ CD4p,data=my_data)
y_CD56p = lm(age..months. ~ CD56p,data=my_data)
y_CD8p = lm(age..months. ~ CD8p,data=my_data)
y_CD19p = lm(age..months. ~ CD19p,data=my_data)
  

# for residuals study

plot(fitted(y_Leukos),resid(y_Leukos))
abline(0,0, col = "red") 
points(fitted(y_Lymphos),resid(y_Lymphos),col="Red")
points(fitted(y_CD14p),resid(y_CD14p),col="Green")
points(fitted(y_CD3p),resid(y_CD3p),col="Blue")
points(fitted(y_CD4p),resid(y_CD4p),col="Yellow")
points(fitted(y_CD56p),resid(y_CD56p),col="Purple")
points(fitted(y_CD8p),resid(y_CD8p),col="Brown")
points(fitted(y_CD19p),resid(y_CD19p),col="Grey")

y_pred = lm(age..months. ~ Leukos + Lymphos + CD14p + CD3p + CD4p + CD56p + CD8p + CD19p,data=my_data)
summary(y_pred)

matrix(y_pred$coefficients, ncol = 1)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -203.89  -57.76  -10.91   52.00  295.15 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 219.581329  41.388915   5.305    8e-07 ***
#   Leukos        0.008327   0.008479   0.982  0.32869    
# Lymphos      -0.088213   0.129556  -0.681  0.49769    
# CD14p         0.077632   0.072585   1.070  0.28769    
# CD3p         -0.167760   0.154559  -1.085  0.28064    
# CD4p          0.258618   0.093030   2.780  0.00662 ** 
#   CD56p         0.062693   0.178517   0.351  0.72627    
# CD8p          0.209277   0.103442   2.023  0.04603 *  
#   CD19p        -0.046210   0.142262  -0.325  0.74607    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 92.71 on 90 degrees of freedom
# Multiple R-squared:  0.4963,	Adjusted R-squared:  0.4516 
# F-statistic: 11.09 on 8 and 90 DF,  p-value: 8.934e-11


plot(fitted(y_pred),resid(y_pred))
abline(0,0,col="Red")


plot(fitted(y_pred), my_data$age..months.)


qqnorm(y_pred$residuals, main = "quantile quantile plot")
qqline(y_pred$residuals, col = "red")

hist(y_pred$residuals)

prediction_table <- data.frame(y_pred$fitted.values)
names(prediction_table)[1]<-paste("simple")
prediction_table



#--------------------exercise 2 -------------------------------------------------------------------------------------
#   Instead of taking the raw marker data, use transformed x-data. This means: take log(x+1)
# for all data. (You can either do this in R or in MS-Excel resp. Open-Office). Remodel the
# y-data. Answer the same questions as above. Has the model improved?

y_pred_log = lm(age..months. ~ log(Leukos+1) + log(Lymphos+1) + log(CD14p+1) + log(CD3p+1) + log(CD4p+1) + 
                  log(CD56p+1) + log(CD8p+1) + log(CD19p+1), data=my_data)


summary(y_pred_log)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -169.107  -48.850   -3.843   31.202  269.597 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)       996.974    412.858   2.415  0.01777 * 
# log(Leukos + 1)    21.259     55.634   0.382  0.70328   
# log(Lymphos + 1)   11.785    246.639   0.048  0.96200   
# log(CD14p + 1)     31.494     27.943   1.127  0.26270   
# log(CD3p + 1)    -393.948    236.118  -1.668  0.09870 . 
# log(CD4p + 1)     276.450    100.070   2.763  0.00695 **
# log(CD56p + 1)     -6.271     25.603  -0.245  0.80707   
# log(CD8p + 1)     103.687     57.115   1.815  0.07279 . 
# log(CD19p + 1)   -148.031     56.425  -2.623  0.01022 * 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 81.72 on 90 degrees of freedom
# Multiple R-squared:  0.6087,	Adjusted R-squared:  0.5739 
# F-statistic:  17.5 on 8 and 90 DF,  p-value: 1.881e-15


plot(fitted(y_pred_log),resid(y_pred_log))
abline(0,0,col="Red")

plot(fitted(y_pred_log), my_data$age..months.)

qqnorm(y_pred_log$residuals, main = "quantile quantile plot")
qqline(y_pred$residuals, col = "red")

hist(y_pred_log$residuals)

prediction_table["log x"] <- y_pred_log$fitted.values
prediction_table


#--------------------exercise 3 -------------------------------------------------------------------------------------
## (a) Do the analysis above using square root of age as the dependent variable (and the logs
# as in exercise 2). Again analyse residuals. Answer the same questions as above. Predict
# age for the children in the table “prediction data”.

y_pred_log_sqrt = lm(sqrt(age..months.) ~ log(Leukos+1) + log(Lymphos+1) +  log(CD14p+1) + log(CD3p+1) + log(CD4p+1) + 
                       log(CD56p+1) + log(CD8p+1) + log(CD19p+1), data=my_data)

summary(y_pred_log_sqrt)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.6459 -2.2028 -0.0435  1.6579  8.5547 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)       54.7069    16.1542   3.387  0.00105 **
#   log(Leukos + 1)    2.0526     2.1768   0.943  0.34824   
# log(Lymphos + 1)  -2.7611     9.6504  -0.286  0.77545   
# log(CD14p + 1)     0.1871     1.0934   0.171  0.86448   
# log(CD3p + 1)    -11.6892     9.2387  -1.265  0.20905   
# log(CD4p + 1)      7.5422     3.9155   1.926  0.05723 . 
# log(CD56p + 1)     0.1584     1.0018   0.158  0.87469   
# log(CD8p + 1)      4.2722     2.2348   1.912  0.05909 . 
# log(CD19p + 1)    -5.5118     2.2078  -2.497  0.01436 * 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.197 on 90 degrees of freedom
# Multiple R-squared:  0.6936,	Adjusted R-squared:  0.6664 
# F-statistic: 25.47 on 8 and 90 DF,  p-value: < 2.2e-16

plot(fitted(y_pred_log_sqrt),resid(y_pred_log_sqrt))
abline(0,0,col="Red")

plot(fitted(y_pred_log_sqrt), my_data$age..months.)

qqnorm(y_pred_log_sqrt$residuals, main = "quantile quantile plot")
qqline(y_pred_log_sqrt$residuals, col = "red")

hist(y_pred_log_sqrt$residuals)

prediction_table["squr_root y and log x"] <- y_pred_log_sqrt$fitted.values
prediction_table


#--------------------exercise 4 -------------------------------------------------------------------------------------
## Do the analysis above using log of age as the dependent variable. Again analyse
# residuals. Answer the same questions as above. Predict age for the children in the table
# “prediction data”.

y_pred_log_log = lm(log(age..months.+1) ~ log(Leukos+1) + log(Lymphos+1) + log(CD14p+1) + log(CD3p+1) + log(CD4p+1) + 
                      log(CD56p+1) + log(CD8p+1) + log(CD19p+1), data=my_data)
summary(y_pred_log_log)

# (Intercept)       15.9654     3.6021   4.432 2.62e-05 ***
# log(Leukos + 1)    0.8717     0.4854   1.796   0.0759 .  
# log(Lymphos + 1)  -2.0049     2.1519  -0.932   0.3540    
# log(CD14p + 1)    -0.3846     0.2438  -1.577   0.1182    
# log(CD3p + 1)     -0.6138     2.0601  -0.298   0.7664    
# log(CD4p + 1)      0.2116     0.8731   0.242   0.8090    
# log(CD56p + 1)     0.1838     0.2234   0.823   0.4127    
# log(CD8p + 1)      0.8815     0.4983   1.769   0.0803 .  
# log(CD19p + 1)    -0.7216     0.4923  -1.466   0.1462    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.713 on 90 degrees of freedom
# Multiple R-squared:  0.7269,	Adjusted R-squared:  0.7026 
# F-statistic: 29.94 on 8 and 90 DF,  p-value: < 2.2e-16


plot(fitted(y_pred_log_log),resid(y_pred_log_log))
abline(0,0,col="Red")

plot(fitted(y_pred_log_log), my_data$age..months.)

qqnorm(y_pred_log_log$residuals, main = "quantile quantile plot")
qqline(y_pred_log_log$residuals, col = "red")

hist(y_pred_log_log$residuals)

prediction_table["log y and log x"] <- y_pred_log_log$fitted.values
prediction_table



