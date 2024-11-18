#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of Woody Cover
# Paper:
# browseURL("https://drive.google.com/file/d/1aasI7uIFj7zcjuu0A3QhPDU4X-oQtcnx/view?usp=sharing")

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
library(lavaan)

# dataset:
#browseURL("https://docs.google.com/spreadsheets/d/1kq0e4E_kSH6C6sbEnl4cbqeh7t6eAeUqsiZ_Z1YIQK0/edit?gid=2140893522#gid=2140893522")

# read the data from the google docs link:
SEMdata <-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQSjeAeOypBrecE8VKoGLyJ076SytDGt8_sQSFdX-9wZwLRUBJMXmVA6GxMxF956Pwl0FCJTrcRSYbw/pub?gid=2140893522&single=true&output=csv") 
names(SEMdata)
SEMdata

# standardize all variables to mean 0 and standard deviation 1
SEMstd <- SEMdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEMstd

# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEMdata %>% dplyr::select(dist2river, elevation, rainfall, 
                                                   hills, cec, burnfreq, treecover,
                                                   woody),
                    stars = T, ellipses = F)
psych::pairs.panels(SEMstd %>% dplyr::select(dist2river, elevation, rainfall, 
                                              hills, cec, burnfreq, treecover,
                                              woody),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
multreg_std <- lm(woody ~ dist2river + elevation + rainfall +
                    hills + cec + burnfreq + treecover, data = SEMstd)
summary(multreg_std)

#you have not accounted for internal relations between the variables
#so, now we go from this stupid model, to the smart model 

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
woody_model <- 'woody ~ dist2river + elevation + rainfall + hills + cec + burnfreq + treecover
                dist2river ~ rainfall  
                elevation ~ hills 
                rainfall ~ elevation + hills
                cec ~ burnfreq + rainfall + dist2river
                burnfreq ~ rainfall 
                treecover ~ dist2river + rainfall + cec'
woody_model
woody_model_fit <- lavaan::sem(woody_model, data = SEMstd)

# show the model results
summary(woody_model_fit, standardized = T, fit.measures = T, rsquare = T)
#Use these values to assign to your drawn model 
#Also use the R-squared values to assign to the model

# goodness of fit (should be >0.9): CFI and TLI
#CFI = 0.815, TLI = 0.529 --> not good
# badness of fit: ( should be <0.1): RMSEA, SRMR
#RMSEA = 0.203, SRMR = 0.106 --> not good


<<<<<<< HEAD
# visualise the model
=======
  >>>>>>> 8a237fe2317acaad42b557f15ab08d729405ba65


