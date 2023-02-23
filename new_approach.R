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

setwd('~/University/Econometrics and Operations Research/thesis_andre lucas/gender_discrimination')

# 4.  READ EXCEL FILE ####

data = read_excel("ceo_official_dataset.xlsx")

# 5.  QUICK DESCRIPTIVE SUMMARY OF THE DATA ####

summary(data)
str(data)



# 6.  CLEAN THE DATA #####

'In this section we are going to clean the data, which means to create dummies, 
to rename variables, to correct the data types and to handle all the missing 
values'

# re-code the SPCODE variable in order to have two groups from the current 4
data$SPCODE_new[data$SPCODE == 'SP'] <- 'SP_index'
data$SPCODE_new[data$SPCODE == 'MD'] <- 'non_SP'
data$SPCODE_new[data$SPCODE == 'SM'] <- 'non_SP'
data$SPCODE_new[data$SPCODE == 'EX'] <- 'non_SP'


# replace the NA values as well in order to have only 2 dummies
data$SPCODE_new[is.na(data$SPCODE_new)] = 'non_SP'




# create dummies
data <- dummy_cols(data, select_columns = 'GENDER')
data <- dummy_cols(data, select_columns = 'SPCODE_new')
#print(data)


# creating another subset with the CEOS from the general data set
ceo_data <- data[data$CEO_NEW == 'CEO', c('YEAR','TRS1YR','ROA','AGE',
                                          'MKTVAL', 'PRCC', 'GENDER_FEMALE',
                                          'GENDER_MALE','LAG_TRS1YR',
                                          'CO_PER_ROL','SPCODE_new','SPINDEX',
                                          'SPCODE_new_non_SP', 
                                          'SPCODE_new_SP_index' )]
                                          
                                        
                                         

# creating another subset for the CFOS from the general data set
cfo_data <- data[data$CFO_NEW == 'CFO', c('YEAR','TRS1YR','ROA','AGE',
                                          'MKTVAL','PRCC', 'GENDER_FEMALE',
                                          'GENDER_MALE', 'LAG_TRS1YR',
                                          'CO_PER_ROL','SPCODE_new','SPINDEX',
                                          'SPCODE_new_non_SP', 
                                          'SPCODE_new_SP_index' )]
                            



# creating a data set for both the CEO and the CFO combined
ceo_cfo_data <- data[data$CEO_NEW == 'CEO' | data$CFO_NEW == 'CFO', 
                     c('YEAR', 'TRS1YR','ROA','AGE','MKTVAL','PRCC',
                       'GENDER_FEMALE','GENDER_MALE', 'LAG_TRS1YR','CO_PER_ROL',
                       'SPCODE_new','SPINDEX','SPCODE_new_non_SP', 
                       'SPCODE_new_SP_index' )]
                                                 

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
cfo_data[is.na(cfo_data)] = 0
ceo_cfo_data[is.na(ceo_cfo_data)] = 0



# recheck which columns contain Nan values
names(which(colSums(is.na(ceo_data))>0))
names(which(colSums(is.na(cfo_data))>0))
names(which(colSums(is.na(ceo_cfo_data))>0))



# 7.  CREATING THE LINEAR MODELS NEEDED FOR THE ANALYSIS ####

'Chapter 7 of the algorithm is dedicated to the creation of the OLS
models that are also going to be used for further analysis and 
investiation, regarding the diagnostic checks that are going to follow. '



# MODEL OF THE CEO DATA SET
lm_ceo <- lm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_MALE + LAG_TRS1YR +
               SPCODE_new_SP_index, data = ceo_data)
summary(lm_ceo)

# alternative model with the female variable as independent
lm_ceo_f <- lm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_FEMALE + LAG_TRS1YR +
               SPCODE_new_SP_index, data = ceo_data)
summary(lm_ceo_f)



# MODEL FOR THE CFO DATA SET
lm_cfo <- lm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_MALE + LAG_TRS1YR + 
               SPCODE_new_SP_index, data = cfo_data)
summary(lm_cfo)

# alternative model with the female dummy as independent
lm_cfo_f <- lm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_FEMALE + LAG_TRS1YR + 
               SPCODE_new_SP_index, data = cfo_data)
summary(lm_cfo_f)




# MODEL FOR CFO AND CEO COMBINED
lm_ceo_cfo <- lm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_MALE + LAG_TRS1YR + 
                   SPCODE_new_SP_index, data = ceo_cfo_data)
summary(lm_ceo_cfo)

# ALTERNATIVE MODEL WITH THE FEMALE AS THE DUMMY
lm_ceo_cfo_f <- lm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_FEMALE + LAG_TRS1YR + 
                   SPCODE_new_SP_index, data = ceo_cfo_data)
summary(lm_ceo_cfo_f)
 

'But the mothod above ignores the panel structure, it is a simple OLS, 
so we are going to use the plm method that that basically takes the panel
structure into account.'



# OLS WITH PANEL STRUCTURE INTO ACCOUNT #####


# MODEL FOR THE CEO DATA
plm_ceo <- plm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_MALE + LAG_TRS1YR +
                 SPCODE_new_SP_index, data = ceo_data, 
                 index = c('YEAR', 'CO_PER_ROL'))
summary(plm_ceo)

# ALTERNATIVE MODEL WITH THE FEMALE AS THE DUMMY
plm_ceo_F<- plm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_FEMALE + LAG_TRS1YR +
                 SPCODE_new_SP_index, data = ceo_data, 
               index = c('YEAR', 'CO_PER_ROL'))
summary(plm_ceo_F)



# MODEL FOR THE CFO DATA
plm_cfo <- plm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_MALE + LAG_TRS1YR +
                 SPCODE_new_SP_index, data = cfo_data, 
                 index = c('YEAR','CO_PER_ROL'))
summary(plm_cfo)

#ALTERNATIVE MODEL WITH THE FEMALE AS THE DUMMY
plm_cfo_F <- plm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_FEMALE + LAG_TRS1YR +
                 SPCODE_new_SP_index, data = cfo_data, 
               index = c('YEAR','CO_PER_ROL'))
summary(plm_cfo_F)


# MODEL FOR THE CEO & CFO DATA COMBINED

plm_ceo_cfo <- plm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_MALE + LAG_TRS1YR +
                     SPCODE_new_SP_index, data = ceo_cfo_data, 
                   index = c('YEAR','CO_PER_ROL'))
summary(plm_ceo_cfo)

# ALTERNATIVE MODEL WITH THE FEMALE AS THE DUMMY
plm_ceo_cfo_F <- plm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_FEMALE + LAG_TRS1YR +
                     SPCODE_new_SP_index, data = ceo_cfo_data, 
                   index = c('YEAR','CO_PER_ROL'))
summary(plm_ceo_cfo_F)






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
lmtest::bgtest(lm_ceo)


# test heteroskedactisity in the CFO data set 
lmtest::bgtest(lm_cfo)







# 9.  TESTING FOR AUTOCORELATION IN THE DATA #####


'In the current section the diagnostic checks are going to be continued. 
I am going to use the models that were created in the previous section 
and conduct autocorrelation tests. The autocorralation is going to be 
conducted with two ways.'


'CONDUCTING THE AUTOCORRELATION TEST WITH THE DURBIN WATSON TEST'

# Durbin Watson for the CEO data set
lmtest::dwtest(lm_ceo)



#Durbin Watson test for the CFO data set
lmtest::dwtest(lm_cfo)




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
l_test_model_ceo <- studres(lm_ceo)
shapiro.test(l_test_model_ceo[0:5000])


# linearity test on the CFO model
l_test_model_cfo <- studres(lm_cfo)
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




'CONDUCTING THE KPSS TEST FOR THE CFO SET'


#TRS1YR variable
kpss.test(cfo_data$TRS1YR, null = c('Level'), lshort=TRUE)
#ROA variable
kpss.test(cfo_data$ROA, null = c('Level'), lshort=TRUE)
#LAGTRS1YR variable
kpss.test(cfo_data$LAG_TRS1YR, null = c('Level'), lshort=TRUE)
# PRCC variable
kpss.test(cfo_data$PRCC, null = c('Level'), lshort=TRUE)






























# 12. CREATING THE FE AND RE MODELS ####

'The following code is based on the article of https://medium.com/@manilwagle
/how-to-deal-with-panel-data-practical-application-using-r-18ef95ae99c6'

'The approach that we are going to use is for panel data analysis
We have the city that reveals a cross sectional data and the year
which reveals the time series model.'


'CONDUCT THE FIXED EFFECT AND RANDOM EFFECT MODEL FOR THE CEO DATA '

# fixed effect model CEO data set
fe_model_ceo <- plm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_MALE + LAG_TRS1YR +
                      SPCODE_new_SP_index, model = 'within', data = ceo_data,
                      index = c('YEAR', 'CO_PER_ROL'))
                      
 
# random effect model
re_model_ceo <- plm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_MALE + LAG_TRS1YR + 
                      SPCODE_new_SP_index, model = 'random',
                    random.method = 'nerlove', data = ceo_data,
                    index = c('YEAR', 'CO_PER_ROL'))
                      

                  

'CONDUCT THE FIXED AND RANDOM EFFECT MODEL FOR THE CFO DATA'

# fixed effect model
fe_model_cfo <- plm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_MALE + LAG_TRS1YR + 
                      SPCODE_new_SP_index, model = 'within', 
                    data = cfo_data, index = c('YEAR', 'CO_PER_ROL'))
                      
                        

#random effect model
re_model_cfo <- plm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_MALE + LAG_TRS1YR + 
                      SPCODE_new_SP_index, model = 'random',
                    random.method = 'nerlove', data = cfo_data, 
                    index = c('YEAR', 'CO_PER_ROL'))

                      
                      
                             
   
                           
                        
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

'In the current session we are going to apply the models that are going to be
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
fixed_ceo_data <- plm(TRS1YR ~ ROA + AGE + MKTVAL + GENDER_MALE + LAG_TRS1YR + 
                        SPCODE_new_SP_index, model = 'within', data = ceo_data,
                      index = c('YEAR', 'CO_PER_ROL'),effect = "individual")
                         

coeftest(fixed_ceo_data, vcov = vcovHC, type = "HC1")

# ALTERNATIVE MODEL WITH THE FEMALE AS THE DUMMY
fixed_ceo_data_F <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_FEMALE + LAG_TRS1YR + 
                        SPCODE_new_SP_index, model = 'within', data = ceo_data,
                      index = c('YEAR', 'CO_PER_ROL'),effect = "individual")


coeftest(fixed_ceo_data_F, vcov = vcovHC, type = "HC1")



# fixed effects for CFO
fixed_cfo_data <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + LAG_TRS1YR + 
                        SPCODE_new_SP_index, model = 'within', data = cfo_data,
                      index = c('YEAR', 'CO_PER_ROL'),effect = "individual")
                        

coeftest(fixed_cfo_data, vcov = vcovHC, type = "HC1")

# ALTERNATIVE MODEL WITH THE FEMALE AS THE DUMMY
fixed_cfo_data_F <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_FEMALE + LAG_TRS1YR + 
                        SPCODE_new_SP_index, model = 'within', data = cfo_data,
                      index = c('YEAR', 'CO_PER_ROL'),effect = "individual")


coeftest(fixed_cfo_data_F, vcov = vcovHC, type = "HC1")


# fixed effect for the CEO&CFO data
fixed_ceo_cfo <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_MALE + LAG_TRS1YR + 
                       SPCODE_new_SP_index, model = 'within', 
                     data = ceo_cfo_data, index = c('YEAR', 'CO_PER_ROL'),
                     effect = "individual")


coeftest(fixed_ceo_cfo, vcov = vcovHC, type = "HC1")

# ALTERNATIVE MODEL WITH THE FEMALE AS THE DUMMY
fixed_ceo_cfo_F <- plm(TRS1YR ~ ROA + AGE + PRCC + GENDER_FEMALE + LAG_TRS1YR + 
                       SPCODE_new_SP_index, model = 'within', 
                     data = ceo_cfo_data, index = c('YEAR', 'CO_PER_ROL'),
                     effect = "individual")


coeftest(fixed_ceo_cfo_F, vcov = vcovHC, type = "HC1")
                     







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
pr_model_ceo_BM<- glm(GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR +
                              LAG_TRS1YR, SPCODE_new_SP_index, 
                            family = binomial(), data = ceo_data)
                    
summary(propensity_model_ceo)
coeftest(pr_model_ceo_BM, vcov = vcovHC, type = "HC1")

# ALTERNATIVE WITH THE FEMALE AS THE DUMMY
pr_model_ceo_fe_bm <- glm(GENDER_FEMALE ~ ROA + AGE + PRCC + TRS1YR +
                              LAG_TRS1YR, SPCODE_new_SP_index, 
                            family = binomial(), data = ceo_data)

summary(pr_model_ceo_fe_bm)
coeftest(pr_model_ceo_fe_bm, vcov = vcovHC, type = "HC1")
                            


# FOR THE CFO DATASET
pr_model_cfo_bm <- glm(GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR +
                              LAG_TRS1YR + SPCODE_new_SP_index, 
                            family = binomial(), data = cfo_data)
                            
summary(propensity_model_cfo)
coeftest(pr_model_cfo_bm, vcov = vcovHC, type = "HC1")

# ALTERNATIVE WITH THE FEMALE AS THE DUMMY
pr_model_cfo_f_bm <- glm(GENDER_FEMALE ~ ROA + AGE + PRCC + TRS1YR +
                              LAG_TRS1YR + SPCODE_new_SP_index, 
                            family = binomial(), data = cfo_data)

summary(pr_model_cfo_f_bm)
coeftest(pr_model_cfo_f_bm, vcov = vcovHC, type = "HC1")




# for the ceo_cfo data
propensity_ceo_cfo_bm <- glm(GENDER_MALE ~ ROA + AGE + PRCC + TRS1YR +
                            LAG_TRS1YR + SPCODE_new_SP_index, 
                          family = binomial(), data = ceo_cfo_data)
summary(propensity_ceo_cfo_bm)


# ALTERNATIVE WITH THE FEMALE AS THE DUMMY


propensity_ceo_cfo_F_bm <- glm(GENDER_FEMALE ~ ROA + AGE + PRCC + TRS1YR +
                            LAG_TRS1YR + SPCODE_new_SP_index, 
                          family = binomial(), data = ceo_cfo_data)
summary(propensity_ceo_cfo_F)
coeftest(propensity_ceo_cfo_F_bm, vcov = vcovHC, type = "HC1")


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
cov <- c('ROA', 'AGE', 'MKTVAL','LAG_TRS1YR','SPCODE_new_SP_index')

# FOR THE CEO DATA 
ceo_data_nomiss <- ceo_data %>%  # MatchIt does not allow missing values
  select('TRS1YR', 'GENDER_MALE', one_of(cov)) %>%
  na.omit()

# without the spindex
ceo_data_match <- matchit(GENDER_MALE ~ ROA + AGE + MKTVAL + TRS1YR +
                            LAG_TRS1YR + SPCODE_new_SP_index , method = "nearest",
                          distance = 'glm', ratio = 1, replace = FALSE, 
                          data = ceo_data_nomiss )
#matching the data
ceo_dta_m <- match.data(ceo_data_match)
dim(ceo_dta_m)


# ALTERNATIVE DATA WITH THE FEMALE AS THE DUMMY
ceo_data_nomiss_F <- ceo_data %>%  # MatchIt does not allow missing values
  select('TRS1YR', 'GENDER_FEMALE', one_of(cov)) %>%
  na.omit()

ceo_data_match_F <- matchit(GENDER_FEMALE ~ ROA + AGE + MKTVAL + TRS1YR +
                            LAG_TRS1YR + SPCODE_new_SP_index , method = "nearest",
                          distance = 'glm', ratio = 1, replace = FALSE, 
                          data = ceo_data_nomiss_F )
#matching the data
ceo_dta_m_F <- match.data(ceo_data_match_F)
dim(ceo_dta_m_F)



# THE RESULTS INDICATED HERE THAT WE HAVE 4490 OBSERVATIONS, MEANING THAT WE 
# HAVE 4490/2 PAIRS


# FOR THE CFO DATA SET
cfo_data_nomiss <- cfo_data %>%
  select('TRS1YR', 'GENDER_MALE', one_of(cov)) %>%
  na.omit()

cfo_data_match <- matchit(GENDER_MALE ~ ROA + AGE + MKTVAL + TRS1YR +
                            LAG_TRS1YR + SPCODE_new_SP_index, 
                          method = "nearest", distance = 'glm', ratio = 1, 
                          replace = FALSE,
                          data = cfo_data_nomiss )

# matching the data
cfo_dta_m <- match.data(cfo_data_match)
dim(cfo_dta_m)


# WITH THE FEMALE AS THE DUMMY VARIABLE
cfo_data_nomiss_F <- cfo_data %>%
  select('TRS1YR', 'GENDER_FEMALE', one_of(cov)) %>%
  na.omit()

cfo_data_match_F <- matchit(GENDER_FEMALE ~ ROA + AGE + MKTVAL + TRS1YR +
                            LAG_TRS1YR + SPCODE_new_SP_index, 
                          method = "nearest", distance = 'glm', ratio = 1, 
                          replace = FALSE,
                          data = cfo_data_nomiss_F )

# matching the data
cfo_dta_m_F <- match.data(cfo_data_match_F)
dim(cfo_dta_m_F)

# CEO CFO MODEL
ceo_cfo_data_nomiss_M <- ceo_cfo_data %>%
  select('TRS1YR', 'GENDER_MALE', one_of(cov)) %>%
  na.omit()

ceo_cfo_data_match_M <- matchit(GENDER_MALE ~ ROA + AGE + MKTVAL + TRS1YR +
                                LAG_TRS1YR + SPCODE_new_SP_index, 
                              method = "nearest", distance = 'glm', ratio = 1, 
                              replace = FALSE,
                              data = ceo_cfo_data_nomiss_M)

# matching the data
ceo_cfo_dta <- match.data(ceo_cfo_data_match_M)
dim(ceo_cfo_dta)




# ALTERNATIVE MODEL WITH THE FEMALE AS THE DUMMY
ceo_cfo_data_nomiss <- ceo_cfo_data %>%
  select('TRS1YR', 'GENDER_FEMALE', one_of(cov)) %>%
  na.omit()

ceo_cfo_data_match <- matchit(GENDER_FEMALE ~ ROA + AGE + MKTVAL + TRS1YR +
                            LAG_TRS1YR + SPCODE_new_SP_index, 
                          method = "nearest", distance = 'glm', ratio = 1, 
                          replace = FALSE,
                          data = ceo_cfo_data_nomiss )

# matching the data
ceo_cfo_dta_m <- match.data(ceo_cfo_data_match)
dim(ceo_cfo_dta_m)

# FOR THE CURRENT DATASET WE HAVE 4024 OBSERVATIONS, MEANING 4024/2 PAIRS.


# AFTER THE MATCHING

# FOR THE CEO DATA SET
propensity_model_ceo_AM <- glm(GENDER_MALE ~ ROA + AGE + MKTVAL + TRS1YR +
                                 LAG_TRS1YR + SPCODE_new_SP_index,
                               family = binomial(), data = ceo_dta_m)
summary(propensity_model_ceo_AM)
coeftest(propensity_model_ceo_AM, vcov = vcovHC, type = "HC1")



# ALTERNATIVE WITH THE FEMALE AS THE DUMMY
propensity_model_ceo_AM_F <- glm(GENDER_FEMALE ~ ROA + AGE + MKTVAL + TRS1YR +
                                 LAG_TRS1YR + SPCODE_new_SP_index,
                               family = binomial(), data = ceo_dta_m_F)
summary(propensity_model_ceo_AM_F)
coeftest(propensity_model_ceo_AM_F, vcov = vcovHC, type = "HC1")
                      


# FOR THE CFO DATA SET
propensity_model_cfo_AM <- glm(GENDER_MALE ~ ROA + AGE + MKTVAL + TRS1YR +
                                 LAG_TRS1YR + SPCODE_new_SP_index,
                               family = binomial(), 
                               data = cfo_dta_m)

summary(propensity_model_cfo_AM)
coeftest(propensity_model_cfo_AM, vcov = vcovHC, type = "HC1")


# ALTERNATIVE FOR THE CFO DATA WITH THE FEMALE AS THE DUMMY
propensity_model_cfo_AM_F <- glm(GENDER_FEMALE ~ ROA + AGE + MKTVAL + TRS1YR +
                                 LAG_TRS1YR + SPCODE_new_SP_index,
                               family = binomial(), 
                               data = cfo_dta_m_F)

summary(propensity_model_cfo_AM_F)
coeftest(propensity_model_cfo_AM_F, vcov = vcovHC, type = "HC1")


# FOR THE CEO CFO DATASET
propensity_model_ceo_cfo_AM_M <- glm(GENDER_MALE ~ ROA + AGE + MKTVAL + TRS1YR +
                                     LAG_TRS1YR + SPCODE_new_SP_index,
                                   family = binomial(), 
                                   data = ceo_cfo_dta)

summary(propensity_model_ceo_cfo_AM_M)
coeftest(propensity_model_ceo_cfo_AM_M, vcov = vcovHC, type = "HC1")





# ALTERNATIVE MODEL WITH THE FEMALE AS THE DUMMY
propensity_model_ceo_cfo_AM_F <- glm(GENDER_FEMALE ~ ROA + AGE + MKTVAL + TRS1YR +
                                 LAG_TRS1YR + SPCODE_new_SP_index,
                               family = binomial(), 
                               data = ceo_cfo_dta_m)

summary(propensity_model_ceo_cfo_AM_F)
coeftest(propensity_model_ceo_cfo_AM_F, vcov = vcovHC, type = "HC1")


  
