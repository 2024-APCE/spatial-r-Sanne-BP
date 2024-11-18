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
Anderson2007std <- Anderson2007 |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
Anderson2007std
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(Anderson2007 %>% dplyr::select(RES_LHU, BIOMASS, FIRE_FRQ, 
                                                   NMS,LF_N),
                    stars = T, ellipses = F)
psych::pairs.panels(Anderson2007std %>% dplyr::select(RES_LHU, BIOMASS, FIRE_FRQ, 
                                                      NMS,LF_N),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
multreg_std <- lm(LF_N ~ RES_LHU + BIOMASS + FIRE_FRQ + NMS, data = Anderson2007std)
summary(multreg_std)

#you have not accounted for internal relations between the variables
#so, now we go from this stupid model, to the smart model 

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
Leaf_N_model <- 'LF_N ~ BIOMASS + RES_LHU + FIRE_FRQ + NMS
                BIOMASS ~ FIRE_FRQ + RES_LHU
                NMS ~ FIRE_FRQ + RES_LHU'
Leaf_N_model
Leaf_N_model_fit <- lavaan::sem(Leaf_N_model, data = Anderson2007std)

# show the model results
summary(Leaf_N_model_fit, standardized = T, fit.measures = T, rsquare = T)
#Use these values to assign to your drawn model 
#Also use the R-squared values to assign to the model

# goodness of fit (should be >0.9): CFI and TLI
#CFI = 0.995, TLI = 0.953
# badness of fit: ( should be <0.1): RMSEA, SRMR
#RMSEA = 0.065, SRMR = 0.043


<<<<<<< HEAD
# visualise the model
=======
  >>>>>>> 8a237fe2317acaad42b557f15ab08d729405ba65

# also explore the models as shown in fig 5b and 5c of the Anderson2007 paper
# so repeat the model for leaf P content

#Make a new model, now for Leaf_P
multreg_std_P <- lm(LF_P ~ RES_LHU + BIOMASS + FIRE_FRQ + NMS, data = Anderson2007std)
summary(multreg_std)

Leaf_P_model <- 'LF_P ~ BIOMASS + RES_LHU + FIRE_FRQ + NMS
                BIOMASS ~ FIRE_FRQ + RES_LHU
                NMS ~ FIRE_FRQ + RES_LHU'
Leaf_P_model
Leaf_P_model_fit <- lavaan::sem(Leaf_P_model, data = Anderson2007std)

summary(Leaf_P_model_fit, standardized = T, fit.measures = T, rsquare = T)

# goodness of fit (should be >0.9): CFI and TLI
#CFI = 0.995, TLI = 0.952
# badness of fit: ( should be <0.1): RMSEA, SRMR
#RMSEA = 0.065, SRMR = 0.044
