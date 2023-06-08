
###Generalized Linear Mixed Models ####
library(MASS)
library(pscl)
install.packages("AER")

library(readxl)
Cdata <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Reviews/Dataset for paper with Ethylene Forchibe/Cdata.xlsx")
View(Cdata)
library(AER)
attach(Cdata)

#Define the variables
Y <- cbind(Ladybird)
X <- cbind(Rainfall, Air_temperature, Relative_humidity, Plant_age, Myzus, Lipaphis)
X1 <- cbind(Hoverfly, Spider, Ladybird)
X3 <- cbind(Location)
#Descriptive statistics
summary(Y)
summary(X)
summary(X1)

plot(Myzus ~ X1)


#Poisson model coefficients
Ptest <- glm(Y ~ X, family = poisson)
summary(Ptest)

# The rule of thumb for determining overdispersion is that the ratio of 
# residual defiance to its df should be 1. If its more than 1, there is 
# overdispersion. 

#Test for overdispersion using AER package
library(AER)
dispersiontest(Ptest)
#This returns "alternate hypothesis: true dispersion is greater than 1".
# which suggests overdispersion. 

#Lets test for overdispersion using DHARMa package (we can visualise it too)
library(DHARMa)
sim_Ptest <- simulateResiduals(Ptest, refit = T)
testOverdispersion(sim_Ptest)  #This returns "dispersion test significant"

#Plot simulated residuals
plotSimulatedResiduals(sim_Ptest)

#This conforms overdispersion,meaning poisson distribution does not work for our data. Overdispersion means the assumptions of the model
# are not met so we cannot trust the output of the model.

# Now lets fix overdispersion. First, lets check if we can use negative 
# binomial GLMM's: used when the mean != variance
mean(Y)
var(Y)
mean(X)
var(X)
mean(X1)
var(X1)

# The means are not equal to the varinaces. 

# This is where the hurdle model comes in. The Hurdle model is of two parts: 
# one that specifies one process for zero counts and another process for 
# positive counts.By fitting a hurdle model to Cdata, it essentially means one 
# process governs whether a Myzus is present and the other governs how many 
# counts are made.
#Now let's address the overdispersion with the negative binomial model

library(pscl)
# For Myzuz
Myz1.nb <- hurdle( Y~ X | X3, data = Cdata, dist = "negbin")
summary(Myz1.nb)

#For Lipaphis
Lip1.bn <- hurdle(Y ~ X | X3, data=Cdata, dist = "negbin")
summary(Lip1.bn)

#For Hoverfly
Hov1.bn <- hurdle(Y ~ X | X3, data=Cdata, dist = "negbin")
summary(Hov1.bn)

#For Spider
Spi1.bn <- hurdle(Y ~ X | X3, data=Cdata, dist = "negbin")
summary(Spi1.bn)

# For Ladybird
Lady1.bn <- hurdle(Y ~ X | X3, data=Cdata, dist = "negbin")
summary(Lady1.bn)

#Plot simulated residuals
plotSimulatedResiduals(Myz.nb)

