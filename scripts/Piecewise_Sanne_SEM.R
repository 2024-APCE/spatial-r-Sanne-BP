# Piecewise SEM

library(piecewiseSEM)

# read the pointdata
pointdata_init<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQSjeAeOypBrecE8VKoGLyJ076SytDGt8_sQSFdX-9wZwLRUBJMXmVA6GxMxF956Pwl0FCJTrcRSYbw/pub?gid=412994482&single=true&output=csv")
pointdata <- pointdata_init |> # Remove rows with missing values
  na.omit() |>   # keep complete cases
  dplyr:: filter(woody>0, woody<20)   # remove 2 extreme values and avoid interpolated negative values

# note that you should not standardize your data for a PicewiseSEM as then eg logistic regression cannot be used

# Check for missing values
sum(is.na(pointdata))
colSums(is.na(pointdata))

#FILTER OUT 0's FOR CEC IN THE LAKE
colSums(pointdata == 0)
sum(pointdata$cec == 0)
pointdata$cec[pointdata$cec == 0] <- NA
pointdata <- na.omit(pointdata)


psych::pairs.panels(pointdata,stars = T, ellipses = F)


# Define the models

# Model 1: woody predicted by burnfreq, cec, treecover and dist2river 
model_woody <- glm(woody ~ burnfreq  + cec  + treecover + dist2river,
                  family= poisson,
                  data = pointdata)

# Calculate dispersion statistic
dispersion_stat <- summary(model_woody)$deviance / summary(model_woody)$df.residual
dispersion_stat #3, so overdispersion
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
library(MASS)
model_woody <- MASS::glm.nb(woody ~ burnfreq  + cec  + treecover + dist2river, 
                               data = pointdata)
summary(model_woody)

p1<-ggplot(data=pointdata,aes(y=woody,x=burnfreq))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p1

p2<-ggplot(data=pointdata,aes(y=woody,x=cec))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p2

p3<-ggplot(data=pointdata,aes(y=woody,x=treecover))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p3

p4<-ggplot(data=pointdata,aes(y=woody,x=dist2river))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p4

#Model 2: cec predicted by rainfall 
model_cec <- lm(cec ~ rainfall, 
                data = pointdata)
summary(model_cec)

p5<-ggplot(data=pointdata,aes(y=cec,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p5

p6<-ggplot(data=pointdata,aes(y=cec,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p6


#Model 3: treecover predicted by cec, rainfall and burnfreq
model_treecover <- glm(treecover ~ cec  + rainfall  + burnfreq,
                       family= poisson,
                       data = pointdata)

# Calculate dispersion statistic
dispersion_stat <- summary(model_treecover)$deviance / summary(model_treecover)$df.residual
dispersion_stat #3, so overdispersion
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
library(MASS)
model_treecover <- MASS::glm.nb(treecover ~ cec  + rainfall  + burnfreq, 
                            data = pointdata)
summary(model_treecover)

p7 <- ggplot(data=pointdata,aes(y=treecover,x=cec))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p7

p8 <- ggplot(data=pointdata,aes(y=treecover,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p8

p9 <- ggplot(data=pointdata,aes(y=treecover,x=burnfreq))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p9
 
#Model 4: burnfreq predicted by rainfall
model_burnfreq <- glm(burnfreq ~  rainfall, 
                           family=poisson, 
                           data = pointdata)
# Calculate dispersion statistic
dispersion_stat <- summary(model_burnfreq)$deviance / summary(model_burnfreq)$df.residual
dispersion_stat
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
library(MASS)
model_burnfreq <- MASS::glm.nb(burnfreq ~ rainfall, 
                               data = pointdata)
summary(model_burnfreq)

p10<-ggplot(data=pointdata,aes(y=burnfreq,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p10


#Model 5: distriver predicted by rainfall
model_distriver <- lm(dist2river ~  rainfall, 
                      data = pointdata)
summary(model_distriver)

p11 <- ggplot(data=pointdata,aes(y=dist2river,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p11



#Model 6: rainfall predicted by dist2river and elevation



# combine the figures
library(patchwork)
allplots<-p1+p2+p3+p4+p5+p6+p7+
  patchwork::plot_layout(ncol=3) +
  patchwork::plot_annotation(title="Relations in model 1")
allplots

####### Combine all models into a single piecewise SEM
psem_model <- piecewiseSEM::psem(model_woody,
                                 model_burnfreq,
                                 model_cec,
                                 model_CorProtAr,
                                 model_rainfall)

# Summarize the SEM results
summary(psem_model)

#Not a good model --> So, you can start reconstructing
#Tests of directed separation are helpful!
#For example: burnfreq ~ elevation is highly significant, but it is not in our                     initial model 
#So, there is also a direct effect of elevation on burnfreq, instead of only an                   indirect effect 
#So, you can add this path to the model
#You want to develop a model, where Chi-square is not significant, and where the test of           directed separation are not significant. However, you do not want to make a saturated             model. So, balance is important. Makes a nice puzzle. 


# a Significant (P<0.05) global goodness of fit means that your model does not fit well, 
# indicating potential problems like missing paths, mis-specfied relations, 
# or unaccounted-for correlations

# update the model based on your results
# significant tests of directed separation could mean a missing direct effect between the variables

# Best Practices:
# - Hypothesize Carefully:
#   Construct the initial model based on theoretical or empirical understanding.
# - Evaluate d-Separation Results:
#   Add or adjust paths based on significant independence test results.
# - Refit and Validate:
#   Rerun the model after adjustments and recheck the Fisher’s C statistic and independence claims.
# - Avoid Overfitting:
#   Add paths only when justified by theory or strong evidence, not purely to improve fit.
# Common pitfall: 
# - ignofing significant d-separation tests and failing to modify the model
# - adding too many variables and pathways, leading to overfitting and loss of parsimony

