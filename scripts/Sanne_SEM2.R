#Best fitting model thus far

multreg2_std <- lm(woody ~ dist2river + elevation + rainfall +
                     cec + burnfreq + treecover, data = SEMstd)
summary(multreg2_std)

#you have not accounted for internal relations between the variables
#so, now we go from this stupid model, to the smart model 

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
woody_model2 <- 'woody ~ burnfreq + rainfall + cec + dist2river + treecover 
                burnfreq ~ rainfall + treecover 
                rainfall ~ elevation 
                cec ~ rainfall + elevation 
                treecover ~ rainfall + cec'
woody_model2
woody_model2_fit <- lavaan::sem(woody_model2, data = SEMstd)


# show the model results
summary(woody_model2_fit, standardized = T, fit.measures = T, rsquare = T)
#Use these values to assign to your drawn model 
#Also use the R-squared values to assign to the model


# goodness of fit (should be >0.9): CFI and TLI
#CFI = 0.850, TLI = 0.625 --> not good, BUT better
# badness of fit: ( should be <0.1): RMSEA, SRMR
#RMSEA = 0.214, SRMR = 0.071 --> not good, BUT better!!!


#TRYING TO IMPROVE:
#Removed effect of elevation on cec + effect of rainfall on woody directly 

multreg5_std <- lm(woody ~ dist2river + elevation + rainfall +
                     cec + burnfreq + treecover, data = SEMstd)
summary(multreg5_std)

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
woody_model5 <- 'woody ~ burnfreq + cec + dist2river + treecover 
                burnfreq ~ rainfall + treecover 
                rainfall ~ elevation 
                cec ~ rainfall 
                treecover ~ rainfall + cec'
woody_model5
woody_model5_fit <- lavaan::sem(woody_model5, data = SEMstd)

# show the model results
summary(woody_model5_fit, standardized = T, fit.measures = T, rsquare = T)

#Goodness of fit:
#CFI = 0.852, TLI = 0.705
#Badness of fit: 
#RMSEA = 0.190, SRMR = 0.069








#BEST MODEL NOW:
#added correlation between dist2river and rainfall

multreg6_std <- lm(woody ~ dist2river + elevation + rainfall +
                     cec + burnfreq + treecover, data = SEMstd)
summary(multreg6_std)

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
woody_model6 <- 'woody ~ burnfreq + cec + dist2river + treecover 
                burnfreq ~ rainfall + treecover 
                rainfall ~ elevation 
                cec ~ rainfall 
                treecover ~ rainfall + cec 
                rainfall ~~ dist2river'
woody_model6
woody_model6_fit <- lavaan::sem(woody_model6, data = SEMstd)

# show the model results
summary(woody_model6_fit, standardized = T, fit.measures = T, rsquare = T)

#Goodness of fit:
#CFI = 0.896, TLI = 0.781
#Badness of fit: 
#RMSEA = 0.160, SRMR = 0.070
























#TRYING TO IMPROVE:
# replacing dist2river by evaporation = not GOOD


multreg7_std <- lm(woody ~ evaporation + elevation + rainfall +
                     cec + burnfreq + treecover, data = SEMstd)
summary(multreg7_std)

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
woody_model7 <- 'woody ~ burnfreq + cec + evaporation + treecover 
                burnfreq ~ rainfall + treecover 
                rainfall ~ elevation 
                cec ~ rainfall 
                treecover ~ rainfall + cec
                rainfall ~~ evaporation'
woody_model7
woody_model7_fit <- lavaan::sem(woody_model7, data = SEMstd)

# show the model results
summary(woody_model7_fit, standardized = T, fit.measures = T, rsquare = T)
