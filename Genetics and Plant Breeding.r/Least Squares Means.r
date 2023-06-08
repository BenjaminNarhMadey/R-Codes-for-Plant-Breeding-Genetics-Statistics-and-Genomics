##Least Square Means####
#------- Please read this first-------------

#LS means, or Least-Squares Means, can be confusing to understand, but they are an important concept 
#in statistical analysis. Here are some key points to remember: 
#LS means are computed relative to a reference grid. This means that they are calculated based on
#a predefined set of reference levels for each predictor in the model.
#Once the reference grid is established, LS means are essentially predictions on this grid. 
#They represent the marginal averages of a table of these predictions.
#Understanding these points is crucial for determining whether LS means are appropriate for your analysis.
#Let's dive deeper into the concept of the reference grid: The reference grid forms the foundation of LS
#means calculations. For each predictor in the model, a set of one or more reference levels is defined. 
#The reference grid consists of all possible combinations of these reference levels. If not explicitly specified, 
#default reference levels are used, which are obtained as follows:
#For categorical predictors, the reference levels are the unique levels of that predictor.
#For numeric predictors, there is only one reference level, which is the mean value of that predictor across the dataset.
#It's important to note that the reference grid depends on both the model being used and the dataset itself.

#===== Example ========================================================================================================
#Let's consider a dataset called "Oranges Sales" included with the lsmeans package. This dataset contains information
#on the sales of two varieties of oranges (sales1 and sales2) in six different stores (store) over a period of six days (day).
#The prices of the oranges (price1 and price2) vary across stores and days. There is only one observation for each store on each day.
#For the sake of illustration, let's focus on an additive covariance model for sales of the first variety. We include both
#factors (store and day) and both price1 and price2 as covariates since the price of the other variety may also impact sales.
#===========================================================================================================


### Now, let's set the overall options. This does not display any result.
#This allows you to set a variety of global opyions which affects the way the results are
#computed and displayed in R. 

options(show.signif.stars=FALSE, prompt="R> ", continue="   ", 
        useFancyQuotes=FALSE, width=100, digits=6)


# The reference grid
install.packages("lsmeans")
library(lsmeans)
oranges.lm1 <- lm(sales1 ~ price1 + price2 + day + store, data = oranges)
anova(oranges.lm1)

( oranges.rg1 <- ref.grid(oranges.lm1) )

##Lets run the summary statistics ####
summary(oranges.rg1)

###Altering the reference grid
#The "at" argument may be used to override defaults in defining the reference grid

lsmeans(oranges.lm1, "day") #or

lsmeans(oranges.rg1, "day")

with(oranges, tapply(sales1, day, mean))


### Least Square Means as marginal averages

lsmeans(oranges.lm1, "day", at = list(price1 = 50, 
                                      price2 = c(40, 60), day = c("2", "3", "4")) )

org.lsm <- lsmeans(oranges.lm1, "day", by = "price2", 
                   at = list(price1 = 50, price2 = c(40, 60), day = c("2", "3", "4")) )
org.lsm

###################################################
### Alternative forms (may or may not work), please adjust as appropriate using 
#the code on line 59
## lsmeans(oranges.lm1, ~ day | price, at = ... )
## lsmeans(oranges.lm1, c("day","price2"), at = ... )
## lsmeans(oranges.lm1, ~ day * price, at = ... )


### Manipulating the results
#Lets check the structure of the results in org.lsm
str(org.lsm)

( org.sum <- summary(org.lsm, infer = c(TRUE,TRUE), null = 7.50,
                     level = .90, adjust = "bon", by = "day") )

#Lets check the class of org.sum
class(org.sum)

### (This code does not display, it only makes the output more compact)
saveop = options(digits = 5)


transform(org.sum, lsrubles = lsmean * 78.47)

### (Lets restore what we did)
op = options(saveop)


org.lsm2 <- update(org.lsm, by.vars = NULL, level = .99)
org.lsm2



###################################################
###  Contrasts and comparisons

### Contrasts in general

contrast(org.lsm, method = "eff")


days.lsm <- lsmeans(oranges.rg1, "day")
contrast(days.lsm, "trt.vs.ctrl", ref = c(5, 6))

### Overriding default to display t tests 
confint(contrast(days.lsm, "trt.vs.ctrlk"))


### Lets do pairwise comparisons
#Frequently, individuals seek to perform pairwise comparisons of LS means or 
#calculate other contrasts between them. To accomplish this, the contrast function 
#is utilized, which takes a "ref.grid" or "lsmobj" object as input.

pairs(org.lsm, reverse = TRUE)

cld(days.lsm, alpha = .10)



### Two-sided formulas #####

lsmeans(org.lsm, poly ~ day)


### More examples

data("Oats", package = "nlme")
library("lme4")
Oats.lmer <- lmer(log(yield) ~ Variety * factor(nitro) + (1 | Block / Variety), 
                  data = Oats)
anova(Oats.lmer)


contrast(lsmeans(Oats.lmer, "nitro"), "poly")


Oats.lmer2 <- lmer(log(yield) ~ Variety + poly(nitro, 2) 
                   + (1 | Block / Variety),  data = Oats)


Oats.lsm2 <- lsmeans(Oats.lmer2, ~ nitro | Variety, cov.reduce = FALSE)
Oats.lsm2

### Graphical display of means

plot(Oats.lsm2, intervals = TRUE, int.adjust = "none", comparisons = TRUE)

lsmip(Oats.lmer, Variety ~ nitro, ylab = "Observed log(yield)")


lsmip(Oats.lsm2, Variety ~ nitro, ylab = "Predicted log(yield)")


### Section 5.2 Transformations
#When fitting a model with a transformation or link function, 
#the ref.grid function (also called by lsmeans) retains and stores
#that information within the returned object.

str(Oats.lsm2)

summary(Oats.lsm2, type = "response")


### Section 5.3 Trends
install.packages("lattice")
library(lattice)
xyplot(weight ~ Time | Diet, groups = ~ Chick, data = ChickWeight, 
       type = "o", layout = c(4, 1))

Chick.lmer <- lmer(weight ~ Diet * Time + (0 + Time | Chick), 
                   data = ChickWeight)


( Chick.lst <- lstrends (Chick.lmer, ~ Diet, var = "Time") )

cld (Chick.lst)

### Dealing with messy data

nutr.lm <- lm(gain ~ (age + group + race)^2, data = nutrition)
library("car")
Anova(nutr.lm)

lsmip(nutr.lm, race ~ age | group)

lsmeans(nutr.lm, ~ group * race)

nutr.lsm <- lsmeans(nutr.lm, ~ group * race, weights = "proportional",
                    at = list(age = c("2", "3"), race = c("Black", "White")))
nutr.lsm    

summary(pairs(nutr.lsm, by = "race"), by = NULL)

summary(pairs(nutr.lsm, by = "group"), by = NULL)

### Comparison with **effects** package
###################################################
install.packages("effects")
library(effects)
nutr.eff <- Effect(c("group", "race"), nutr.lm)
as.data.frame(nutr.eff)

lsmeans(nutr.lm, ~ group * race, weights = "proportional")

### Section 6. Interfacing with **multcomp**

( days.consec <- contrast(days.lsm, "consec", adjust = "mvt") )

install.packages("multcomp")
library(multcomp)
summary(as.glht(days.consec), test = adjusted("Westfall"))

Oats.glht <- glht(Oats.lmer, lsm(~ nitro | Variety))

names(Oats.glht)
confint(Oats.glht[[1]])

###################################################
### This code may or may not work, adjust as appropriate (lines 227-233)

### Two sets of 3 contrasts...
summary(as.glht(pairs(org.lsm)))

### All 6 contrasts as one family...
summary(as.glht(pairs(org.lsm), by = NULL))

### All 15 pairwise comparisons...
summary(as.glht(pairs(org.lsm, by = NULL)))


### Extensions for other types of models

###Multivariate models #####

oranges.mlm <- lm(cbind(sales1,sales2) ~ price1 + price2 + day + store, 
                  data = oranges)
ref.grid(oranges.mlm)

org.mlsm <- lsmeans(oranges.mlm, ~ day | variety, mult.name = "variety")
cld(org.mlsm, sort = FALSE)

org.vardiff <- update(pairs(org.mlsm, by = "day"), by = NULL)

cld(org.vardiff)

###  Proportional-odds model ####
install.packages("MASS")
library("MASS")
housing.plr <- polr(Sat ~ (Infl + Type + Cont)^2, data = housing, weights = Freq)

lsmip(housing.plr, Infl ~ Cont | Type, ylab = "Latent mean", layout = c(4, 1))

cld(lsmeans(housing.plr, ~ Infl))

housing.lsm <- lsmeans(housing.plr, ~ Infl | cut, mode = "lin")
summary(housing.lsm, type = "response")

summary(pairs(housing.lsm), type = "response") [1:3, ]

lsmeans(housing.plr, ~ Sat | Infl, mode = "prob")

lsmeans(housing.plr, ~ Infl, mode = "mean.class")


### Markov Chain Monte Carlo (MCMC) method samplers ####
#The lsmeans package offers specific capabilities for handling Bayesian models that
#are fitted using Markov chain Monte Carlo (MCMC) methods. Within the fitted object, 
#there is a designated slot that stores the samples obtained from the posterior 
#distribution of the fixed-effects coefficients

#===========================================
### This code does not display results 
set.seed(12345)

install.packages("MCMCglmm")
library(MCMCglmm)

Oats.mc <- MCMCglmm(log(yield) ~ Variety + factor(nitro), 
                    random = ~ Block + Block:Variety, 
                    nitt = 2300, burnin = 300, verbose = FALSE,
                    data = Oats)

Oats.mclsm <- lsmeans(Oats.mc, "nitro", data = Oats)

#Plot the graph 
plot(as.mcmc(Oats.mclsm))

contrast(Oats.mclsm, "consec")


### Finally
### Example of 'rbind' codes may or may not work 
rbind(pairs(lsmeans(Oats.lmer, "Variety")), pairs(lsmeans(Oats.lmer, "nitro")))



