#*************************************************************
#### Thesis Statement: Women Travelers in a man's world #####
##   Thesis, 2020 
#*************************************************************



# 1.  CLEAR SESSION ####
'The current command is useful every time we want to clear the memory'


rm(list = ls())                       # Clear previous environment
if(!is.null(dev.list())) dev.off()    # Clear previous plots


# 2.  IMPORTING LIBRARIES YOU NEED ####

library(car)
library(plm)
library(AER)
library(urca)
library(vars)
library(vars)
library(MASS)

library(knitr)
library(stats)
library(dynlm)
library(broom)
library(tsDyn)

library(readxl)
library(gplots)
library(lmtest)
library(carData)

library(tseries)
library(magrittr)
library(forecast)
library(estimatr)

library(skedastic)
library(bvartools)
library(tidyverse)
library(stargazer)

library(tidyselect)
library(fastDummies)
library(multiwayvcov)
library(bacondecomp)


library(MatchIt)
library(dplyr)
library(ggplot2)





# 3.  SET WORKING DIRECTORY ####

setwd('~/University/Econometrics and Operations Research/thesis_andre lucas/gender_discrimination_analysis')

# 4.  READ EXCEL FILE ####

data = read_excel("ceo_official_dataset.xlsx")

# 5.  QUICK DESCRIPTIVE SUMMARY OF THE DATA ####

summary(data)
str(data)



# 6.  CLEAN THE DATA #####

'In this section we are going to clean the data, which means to create dummies, 
to rename variables, to correct the data types and to handle all the missing 
values'


# create dummies
data <- dummy_cols(data, select_columns = 'GENDER')
data <- dummy_cols(data, select_columns = 'SPCODE')
#print(data)


# creating another subset with the CEOS from the general data set
ceo_data <- data[data$CEO_NEW == 'CEO', c('YEAR', 'TRS1YR','ROA','AGE','PRCC',
                                          'GENDER_FEMALE','GENDER_MALE','TDC2',
                                          'LAG_TRS1YR','CO_PER_ROL','SPCODE_EX',
                                          'SPCODE_MD','SPCODE_SM','SPCODE_SP',
                                          'SPINDEX')]



# creating another subset for the CFOS from the general data set
cfo_data <- data[data$CFO_NEW == 'CFO', c('YEAR', 'TRS1YR','ROA','AGE','PRCC',
                                          'GENDER_FEMALE','GENDER_MALE','TDC2',
                                          'LAG_TRS1YR','CO_PER_ROL','SPCODE_EX',
                                          'SPCODE_MD','SPCODE_SM','SPCODE_SP',
                                          'SPINDEX')]



# creating a data set for both the CEO and the CFO combined
ceo_cfo_data <- data[data$CEO_NEW == 'CEO' | data$CFO_NEW == 'CFO', 
                     c('YEAR', 'TRS1YR','ROA','AGE','PRCC',
                       'GENDER_FEMALE','GENDER_MALE','TDC2',
                       'LAG_TRS1YR','CO_PER_ROL','SPCODE_EX',
                       'SPCODE_MD','SPCODE_SM','SPCODE_SP',
                       'SPINDEX')]



                                         
                                                             
                                                             
                                                             



# bind the spcode data for both the data and the ceo_data
spcode<- cbind(data$SPCODE_EX, data$SPCODE_MD, data$SPCODE_SM, 
               data$SPCODE_SP)

spcode_ceo <- cbind(ceo_data$SPCODE_EX, ceo_data$SPCODE_SM, 
                    ceo_data$SPCODE_MD)

spcode_cfo <- cbind(cfo_data$SPCODE_EX, cfo_data$SPCODE_SM, 
                    cfo_data$SPCODE_MD)

spcode_ceo_cfo <- cbind(ceo_cfo_data$SPCODE_EX, ceo_cfo_data$SPCODE_SM, 
                        ceo_cfo_data$SPCODE_MD)



# declaring the new subset as a data frame
ceo_data <- data.frame(ceo_data)
cfo_data <- data.frame(cfo_data)
ceo_cfo_data <- data.frame(ceo_cfo_data)



'CHECKING MISSING VALUES IN THE SUBSET AND THE ENTIRE DATASET'

# check which columns contain Nan values
names(which(colSums(is.na(ceo_data))>0))
names(which(colSums(is.na(cfo_data))>0))
names(which(colSums(is.na(ceo_cfo_data))>0))

# replace all the Nan values with 0
ceo_data[is.na(ceo_data)] = 0


ceo_cfo_data %>% 
  replace(.== 'NULL', 0) 


cfo_data %>% 
  replace(.== 'NULL', 0) 

# recheck which columns contain Nan values
names(which(colSums(is.na(ceo_data))>0))





# 7.  CREATING THE LINEAR MODELS NEEDED FOR THE ANALYSIS ####

'Chapter 7 of the algorithm is dedicated to the creation of the OLS
models that are also going to be used for further analysis and 
investiation, regarding the diagnostic checks that are going to follow. '



# MODEL OF THE CEO DATA SET
lm_ceo <- lm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + 
                       LAG_TRS1YR + spcode_ceo + TDC2, data = ceo_data)
summary(lm_ceo)


# MODEL FOR THE CFO DATA SET
lm_cfo <- lm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + 
                       LAG_TRS1YR + spcode_cfo + TDC2, data = cfo_data)
summary(lm_cfo)


# MODEL FOR CFO AND CEO COMBINED
lm_ceo_cfo <- lm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + 
                      LAG_TRS1YR + spcode_ceo_cfo + TDC2, data = ceo_cfo_data)
summary(lm_ceo_cfo)
 

'But the mothod above ignores the panel structure, it is a simple OLS, 
so we are going to use the plm method that that basically takes the panel
structure into account.'



'OLS WITH PANEL STRUCTURE INTO ACCOUNT'



# MODEL FOR THE CEO DATA
plm_ceo <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + 
                  LAG_TRS1YR + spcode_ceo + TDC2, data = ceo_data, 
                 index = c('YEAR', 'CO_PER_ROL'))
summary(plm_ceo)


# MODEL FOR THE CFO DATA
plm_cfo <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + 
                   LAG_TRS1YR + spcode_cfo + TDC2, data = cfo_data, 
                 index = c('YEAR', 'CO_PER_ROL'))
summary(plm_cfo)









# 8.  TESTING FOR HETEROSKEDACTICITY #####

'In order to check for heteroskedactisity in the sample we are going 
to follow the Breusch - Pagan Test. The current test checks whether 
the variance of the residuals depends in the value of the independent 
variable.


Therefore, the hypothesis of the Breusch - Pagan Test is:
H0: Residuals are distributed with equal variance
(i.e., : Homoskedasticity)
H1: Residuals are distributed with unequal variance
(i.e., : Heteroscedasticity)
'


# test heteroskedactisity in the CEO data set 
lmtest::bgtest(model_ceo)


# test heteroskedactisity in the CFO data set 
lmtest::bgtest(model_cfo)







# 9.  TESTING FOR AUTOCORELATION IN THE DATA #####


'In the current section the diagnostic checks are going to be continued. 
I am going to use the models that were created in the previous section 
and conduct autocorrelation tests. The autocorralation is going to be 
conducted with two ways.'


'CONDUCTING THE AUTOCORRELATION TEST WITH THE DURBIN WATSON TEST'

# Durbin Watson for the CEO data set
lmtest::dwtest(model_ceo)



#Durbin Watson test for the CFO data set
lmtest::dwtest(model_cfo)




'Moving forward, we are conducting the Durbin Watson test in order to confirm
the autocorrelation. 

The hypothesis of the test are the following:

H0: first order autocorrelation does not exist
H1: first order autocorrelation does exist'

'NO autocorrelation signs for any of the three models'






# 10. CHECKING WHETHER OR NOT WE HAVE LINEAR ANALYSIS ####


'CHECKING NORMALITY IN THE DEPENDENT VARIABLE.'


'Non linearity will be confirmed or not by the Shapiro-Wilk test. 
The two main hypotheses of the test are the following:

H0: the variable is normally distributed
H1: The variable is not normally distributed'



# linearity test on the CEO data set
l_test_model_ceo <- studres(model_ceo)
shapiro.test(l_test_model_ceo[0:5000])


# linearity test on the CFO model
l_test_model_cfo <- studres(model_cfo)
shapiro.test(l_test_model_cfo[0:5000])













# 11. CHECKING FOR STATIONARITY IN THE DATASET ####

'Usually in panel data we do not need to worry about stationarity
issues but we have a large dataset that covers a long time period
therefore we need to check for stationarity. We are going to do
that with the KPSS test


The hypotheses of the current test are the following:
H0: The data is stationary
H1: The data is non-stationary.'



'CONDUCTING THE KPSS TEST FOR THE CEO SET'

#TRS1YR variable
kpss.test(ceo_data$TRS1YR, null = c('Level'), lshort=TRUE)
#ROA variable
kpss.test(ceo_data$ROA, null = c('Level'), lshort=TRUE)
#LAGTRS1YR variable
kpss.test(ceo_data$LAG_TRS1YR, null = c('Level'), lshort=TRUE)
# PRCC variable
kpss.test(ceo_data$PRCC, null = c('Level'), lshort=TRUE)
#TDC2 variable
kpss.test(ceo_data$TDC2, null = c('Level'), lshort=TRUE)





'CONDUCTING THE KPSS TEST FOR THE CFO SET'


#TRS1YR variable
kpss.test(cfo_data$TRS1YR, null = c('Level'), lshort=TRUE)
#ROA variable
kpss.test(cfo_data$ROA, null = c('Level'), lshort=TRUE)
#LAGTRS1YR variable
kpss.test(cfo_data$LAG_TRS1YR, null = c('Level'), lshort=TRUE)
# PRCC variable
kpss.test(cfo_data$PRCC, null = c('Level'), lshort=TRUE)
#TDC2 variable
kpss.test(cfo_data$TDC2, null = c('Level'), lshort=TRUE)




































# 12. CREATING THE FE AND RE MODELS ####

'The following code is based on the article of https://medium.com/@manilwagle
/how-to-deal-with-panel-data-practical-application-using-r-18ef95ae99c6'

'The approach that we are going to use is for panel data analysis
We have the city that reveals a cross sectional data and the year
which reveals the time series model.'


'CONDUCT THE FIXED EFFECT AND RANDOM EFFECT MODEL FOR THE CEO DATA '

# fixed effect model CEO data set
fe_model_ceo <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + LAG_TRS1YR + 
                         TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM, 
                         model = 'within', data = ceo_data,
                         index = c('YEAR', 'CO_PER_ROL'))

# random effect model
re_model_ceo <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + LAG_TRS1YR + 
                         TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM, 
                         model = 'random', random.method = 'nerlove', 
                         data = ceo_data, index = c('YEAR', 'CO_PER_ROL'))


'CONDUCT THE FIXED AND RANDOM EFFECT MODEL FOR THE CFO DATA'

# fixed effect model
fe_model_cfo <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + LAG_TRS1YR + 
                           TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM, 
                         model = 'within', data = cfo_data,
                         index = c('YEAR', 'CO_PER_ROL'))

#random effect model
re_model_cfo <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + LAG_TRS1YR + 
                           TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM, 
                         model = 'random', random.method = 'nerlove', 
                         data = cfo_data, index = c('YEAR', 'CO_PER_ROL'))





                           


                           
                        
# 13. CONDUCTING THE HAUSMAN TEST FOR ENDOGENEITY ####

'Basically the Hausman test can be used to differentiate between fixed 
model and random effects model in panel analysis. In this case, random 
effects is preferred under the null hypothesis due to higher efficiency, 
while under the alternative fixed effects is at least as consistent and 
thus preferred. It basically detects endogenous regressors in a regression.
Having endogenous regressors in a model will cause ordinary least squares 
estimators to fail, as one of the assumptions of OLS is that there is no 
correlation between a predictor variable and the error term. Instrumental 
variables estimators can be used as an alternative in this case.

Null hypothesis: Random effects model is preffered

Meaning that: there is not correlation between the unique errors and the
regressors of the model.Meaning that the variables are exogenous.


Alternative hypothesis: Fixed effects model is preffered.

meaning that: there is correlation between the unique errors and the
regressors of the model. Meaning that the variables are endogenous.'


# ENDOGENEITY TEST FOR THE CEO DATA SET
phtest(fe_model_ceo, re_model_ceo)

# ENDOGENEITY TEST FOR THE CFO DATA SET
phtest(fe_model_cfo, re_model_cfo)

'The results are indicating that the null hypothesis should be rejected,
meaning that our model demonstrates endogeneity.'














# 14. CREATING THE FIXED EFFECT MODEL #####

'In the current session we are going to apply the models taht are going to be
reveal the relation between the returns and the independent variables. Based
on the paper of yiwei Li and Zeng, with title: The impact of top executive 
gender on asset prices: Evidence from stock price crach risk.


We are going to apply the fixed effect model first, as it also done in the 
paper. 

In general the fixed effect model is a statistical model in which the model
parameters are fixed or non-random quantities. In general a fixed-effects model
refers to a regression model in which the group means are fixed(non-random) as
opposed to a random effects model in which the group means are a rndom sample
from a population.

In a fixed effects model each group mean is a group-specific fixed quantity. In
panel data analysis the term fixed effect estimator (also known as the within
estimator) is used to refer to an estimator for the coefficients in the
regression model including those fixed effects (one time-invariant intercept for
each subject)



The fixed effect regression model is used to estimate the effect of intrinsic
characteristics are genetics, acumen and cultural factors. Such factors are not
directly observable or measurable but one needs to find a way to estimate their
effects since leaving them out leads to a sub-optimally regression model.The 
Fixed effects model is designed to adress this problem. 
'


# fixed effects for CEO
fixed_ceo_data <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + LAG_TRS1YR + 
                           TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM, 
                         model = 'within', data = ceo_data,
                         index = c('YEAR', 'CO_PER_ROL'),effect = "individual")
summary(fixed_ceo_data)
coeftest(fixed_ceo_data, vcov = vcovHC, type = "HC1")



# fixed effects for CFO
fixed_cfo_data <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + LAG_TRS1YR + 
                        TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM, 
                      model = 'within', data = cfo_data,
                      index = c('YEAR', 'CO_PER_ROL'),effect = "individual")
summary(fixed_cfo_data)
coeftest(fixed_cfo_data, vcov = vcovHC, type = "HC1")







# 15. CREATING THE PROPENSITY SCORE MATCHING ####

'
We estimate the propensity score by running a logit model (probit also works) 
where the outcome variable is a binary variable indicating treatment status. 
What covariates should you include? For the matching to give you a causal 
estimate in the end, you need to include any covariate that is related to both 
the treatment assignment and potential outcomes.'

'Here we have the opposite model, the dependent variable will be the gender, and 
the returns will be with the rest of the independent variables.'


# BEFORE THE MATCHING


# FOR THE CEO DATA SET
propensity_model_ceo <- glm(GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR +
                              LAG_TRS1YR + TDC2 + SPCODE_EX + SPCODE_MD + 
                              SPCODE_SM + SPINDEX, family = binomial(), 
                            data = ceo_data)

summary(propensity_model_ceo)
                              
'RESULTS OF THE ABOVE COMMAND ARE THE FOLLOWING:

WITHOUT THE SPINDEX


Call:
glm(formula = GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR + LAG_TRS1YR + 
    TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM, family = binomial(), 
    data = ceo_data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.6679   0.2819   0.2908   0.3122   1.5888  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.860e+00  6.974e-02  41.013  < 2e-16 ***
ROA         -1.713e-03  7.059e-04  -2.426  0.01526 *  
AGE          6.056e-03  1.115e-03   5.431 5.59e-08 ***
PRCC         1.125e-05  3.067e-05   0.367  0.71368    
TRS1YR       1.609e-04  3.137e-04   0.513  0.60806    
LAG_TRS1YR  -1.771e-04  1.896e-04  -0.934  0.35025    
TDC2         5.755e-07  1.549e-06   0.372  0.71017    
SPCODE_EX    5.068e-03  5.834e-02   0.087  0.93078    
SPCODE_MD   -1.528e-01  6.969e-02  -2.192  0.02839 *  
SPCODE_SM   -2.111e-01  6.535e-02  -3.230  0.00124 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 18421  on 51098  degrees of freedom
Residual deviance: 18371  on 51089  degrees of freedom
AIC: 18391

Number of Fisher Scoring iterations: 6



WITH THE SPINDEX

Call:
glm(formula = GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR + LAG_TRS1YR + 
    TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM + SPINDEX, family = binomial(), 
    data = ceo_data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.6709   0.2818   0.2909   0.3123   1.6011  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.894e+00  9.047e-02  31.990  < 2e-16 ***
ROA         -1.723e-03  7.085e-04  -2.432  0.01500 *  
AGE          5.999e-03  1.116e-03   5.375 7.67e-08 ***
PRCC         1.119e-05  3.052e-05   0.367  0.71398    
TRS1YR       1.573e-04  3.131e-04   0.503  0.61528    
LAG_TRS1YR  -1.789e-04  1.893e-04  -0.945  0.34474    
TDC2         5.625e-07  1.535e-06   0.367  0.71393    
SPCODE_EX    1.189e-03  5.848e-02   0.020  0.98378    
SPCODE_MD   -1.547e-01  6.971e-02  -2.220  0.02644 *  
SPCODE_SM   -2.150e-01  6.550e-02  -3.282  0.00103 ** 
SPINDEX     -8.281e-06  1.630e-05  -0.508  0.61149    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 18415  on 51096  degrees of freedom
Residual deviance: 18365  on 51086  degrees of freedom
  (2 observations deleted due to missingness)
AIC: 18387

Number of Fisher Scoring iterations: 6

'






# FOR THE CFO DATASET

propensity_model_cfo <- glm(GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR +
                              LAG_TRS1YR + TDC2 + SPCODE_EX + SPCODE_MD + 
                              SPCODE_SM + SPINDEX, family = binomial(), 
                            data = cfo_data)

summary(propensity_model_cfo)

# THE RESULTS OF THE ABOVE MODEL ARE THE FOLLOWING:

'
WITHOUT THE SPINDEX:


Call:
glm(formula = GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR + LAG_TRS1YR + 
    TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM, family = binomial(), 
    data = cfo_data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.7906   0.3319   0.3690   0.3923   2.3087  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.208e+00  8.307e-02  26.576  < 2e-16 ***
ROA         -4.970e-03  1.608e-03  -3.091 0.001997 ** 
AGE          6.465e-03  1.414e-03   4.573 4.82e-06 ***
PRCC         2.162e-05  3.018e-05   0.716 0.473837    
TRS1YR      -1.294e-04  2.278e-04  -0.568 0.569895    
LAG_TRS1YR  -3.221e-05  2.529e-04  -0.127 0.898648    
TDC2         1.627e-05  4.831e-06   3.368 0.000757 ***
SPCODE_EX    2.800e-01  6.373e-02   4.394 1.11e-05 ***
SPCODE_MD    1.776e-04  7.275e-02   0.002 0.998052    
SPCODE_SM   -5.548e-02  6.897e-02  -0.804 0.421132    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 14824  on 30510  degrees of freedom
Residual deviance: 14727  on 30501  degrees of freedom
  (1 observation deleted due to missingness)
AIC: 14747

Number of Fisher Scoring iterations: 6


WITH THE SPINDEX:

Call:
glm(formula = GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR + LAG_TRS1YR + 
    TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM + SPINDEX, family = binomial(), 
    data = cfo_data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.8225   0.3306   0.3652   0.3926   2.4503  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.395e+00  1.025e-01  23.362  < 2e-16 ***
ROA         -5.210e-03  1.662e-03  -3.135 0.001720 ** 
AGE          6.562e-03  1.415e-03   4.637 3.54e-06 ***
PRCC         2.151e-05  2.959e-05   0.727 0.467318    
TRS1YR      -1.348e-04  2.284e-04  -0.590 0.555151    
LAG_TRS1YR  -3.657e-05  2.547e-04  -0.144 0.885854    
TDC2         1.581e-05  4.804e-06   3.291 0.000999 ***
SPCODE_EX    2.692e-01  6.382e-02   4.219 2.46e-05 ***
SPCODE_MD    7.225e-04  7.276e-02   0.010 0.992078    
SPCODE_SM   -6.818e-02  6.909e-02  -0.987 0.323722    
SPINDEX     -5.452e-05  1.725e-05  -3.160 0.001577 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 14824  on 30510  degrees of freedom
Residual deviance: 14717  on 30500  degrees of freedom
  (1 observation deleted due to missingness)
AIC: 14739

Number of Fisher Scoring iterations: 6


'



# EXECUTING  A MATCING ALGORITHM

'A simple method for estimating the treatment effect of WOMEN EXECUTIVES is 
to restrict the sample to observations within the region of common support, and 
then to divide the sample within the region of common support into 6 quintiles, 
based on the estimated propensity score. Within each of these 6 quintiles, we 
can then estimate the mean difference in student achievement by treatment 
status. Rubin and others have argued that this is sufficient to eliminate 95% 
of the bias due to confounding of treatment status with a covariate.

However, most matching algorithms adopt slightly more complex methods. 
The method we use below is to find pairs of observations that have very similar 
propensity scores, but that differ in their treatment status. We use the package
MatchIt for this. This package estimates the propensity score in the background 
and then matches observations based on the method of 
choice ("nearest" in this case).'



# set the covariates of the models
cov <- c('ROA', 'AGE', 'PRCC','LAG_TRS1YR','TDC2','SPCODE_EX',
         'SPCODE_MD', 'SPCODE_SM', 'SPINDEX')

# FOR THE CEO DATA 
ceo_data_nomiss <- ceo_data %>%  # MatchIt does not allow missing values
  select('TRS1YR', 'GENDER_MALE', one_of(cov)) %>%
  na.omit()

# without the spindex
ceo_data_match <- matchit(GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR +
                            LAG_TRS1YR + TDC2 + SPCODE_EX + SPCODE_MD + 
                            SPCODE_SM, method = "nearest", 
                          data = ceo_data_nomiss )
#matching the data
ceo_dta_m <- match.data(ceo_data_match)
dim(ceo_dta_m)


# FOR THE CFO DATA SET
cfo_data_nomiss <- cfo_data %>%
  select('TRS1YR', 'GENDER_MALE', one_of(cov)) %>%
  na.omit()

cfo_data_match <- matchit(GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR +
                            LAG_TRS1YR + TDC2 + SPCODE_EX + SPCODE_MD + 
                            SPCODE_SM, method = "nearest", 
                          data = cfo_data_nomiss )

# matching the data
cfo_dta_m <- match.data(cfo_data_match)
dim(cfo_dta_m)


# AFTER THE MATCHING



# FOR THE CEO DATA SET
propensity_model_ceo_AM <- glm(GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR +
                              LAG_TRS1YR + TDC2 + SPCODE_EX + SPCODE_MD + 
                              SPCODE_SM, family = binomial(), 
                            data = ceo_dta_m)

summary(propensity_model_ceo_AM)

'RESULTS OF THE ABOVE COMMAND
Call:
glm(formula = GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR + LAG_TRS1YR + 
    TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM, family = binomial(), 
    data = ceo_dta_m)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-7.5331  -0.0010   0.0000   0.1265   1.2808  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -4.355e+01  2.339e+00 -18.618  < 2e-16 ***
ROA         -1.753e-01  9.757e-03 -17.966  < 2e-16 ***
AGE          6.791e-01  3.545e-02  19.155  < 2e-16 ***
PRCC         5.052e-03  1.876e-03   2.694  0.00707 ** 
TRS1YR       1.520e-02  1.059e-03  14.355  < 2e-16 ***
LAG_TRS1YR  -1.834e-02  2.924e-03  -6.272 3.56e-10 ***
TDC2         5.154e-05  4.782e-06  10.777  < 2e-16 ***
SPCODE_EX    8.549e-01  2.810e-01   3.042  0.00235 ** 
SPCODE_MD   -1.636e+01  1.362e+00 -12.010  < 2e-16 ***
SPCODE_SM   -2.294e+01  1.922e+00 -11.933  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 6221.69  on 4487  degrees of freedom
Residual deviance:  651.56  on 4478  degrees of freedom
AIC: 671.56

Number of Fisher Scoring iterations: 25'




# FOR THE CFO DATA SET
propensity_model_cfo_AM <- glm(GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR +
                                 LAG_TRS1YR + TDC2 + SPCODE_EX + SPCODE_MD + 
                                 SPCODE_SM, family = binomial(), 
                               data = cfo_dta_m)

summary(propensity_model_cfo_AM)

'RESULTS OF THE ABOVE COMMAND:

Call:
glm(formula = GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR + LAG_TRS1YR + 
    TDC2 + SPCODE_EX + SPCODE_MD + SPCODE_SM, family = binomial(), 
    data = cfo_dta_m)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-7.2502  -0.0146   0.0000   0.2469   1.2319  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -2.186e+01  1.083e+00 -20.186  < 2e-16 ***
ROA         -1.394e-01  7.340e-03 -18.986  < 2e-16 ***
AGE          2.278e-01  1.283e-02  17.762  < 2e-16 ***
PRCC         3.604e-04  6.447e-05   5.591 2.26e-08 ***
TRS1YR      -3.844e-03  1.693e-03  -2.270   0.0232 *  
LAG_TRS1YR  -4.344e-04  1.359e-03  -0.320   0.7493    
TDC2         3.942e-04  1.929e-05  20.436  < 2e-16 ***
SPCODE_EX    7.014e+00  4.481e-01  15.652  < 2e-16 ***
SPCODE_MD   -6.587e-01  8.232e-01  -0.800   0.4236    
SPCODE_SM   -3.267e-01  1.489e+00  -0.219   0.8263    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 5575.7  on 4021  degrees of freedom
Residual deviance: 1140.8  on 4012  degrees of freedom
AIC: 1160.8

Number of Fisher Scoring iterations: 11
'
  