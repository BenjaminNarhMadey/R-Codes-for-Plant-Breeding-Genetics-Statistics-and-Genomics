## EXPLORATORY DATA ANALYSIS

#### Data Cleaning#####
install.packages("tidyverse")
library(tidyverse)
data(package = .packages(all.available = TRUE))
data()
View(corn)

##Variable types ###

data() #displays all data sets in R
dim(data) # returns no of dimensions ie no of rows or observation, no of variables 
glimpse(data) # diaplay a better output than str(), comes with tidyverse
class(data$levels)
str(data) # return the structure of data
View() # returns a view of the data
head() # returns the first 6 rows of data
tail() # returns last 6 rows of data
data$name # returns the names of the data points in the data
unique(data)
attach(data) # helps to avoid repeated mentioning of main data name, use column names directly
names(data) # gives names of variables, just cutt and paste variable names
length(data) # returns the length of the variable
unique(data$levels) #returns all unique values within variable

starwars %>%   # pipe-in operator literally means "and then" or pass the resuls
  select(hair_color) %>%  # of that function into the next function.  
  count(hair_color) %>% # Function creates a table
  arrange(desc(n)) %>% 
  View()

data$levels <- as.factor(data$levels) # convert to factor
class(data$levels)

levels(data$levels)

##Filter observations##
unique(data$hair_color)  #shows all observations withih selection
data %>% 
  select(name, height, ends_with("color")) %>% # we include either blond or brown
  filter(hair_color %in% c("blond", "brown")) & # we use or, and &.
          height < 180 %>% # only include observations <180
  
##Missing data ####
View(data[is.na(hair-color), ]) # returns a table of data, including NAs
summary(data) #shows sumary stats with NA's
mean(data$levels) ##if NA, then
mean(data$levels, na.rm = TRUE) # removes NA obs

data %>% 
  select(name, gender, hair_color, height) # Call data and select columns with missing data

data %>% 
  select(name, gender, hair_color, height) %>% # removes all missing values
  na.omit() # use funtion only if you really know what youre omitting, not recommended

data %>%  # determines where missing values are present
  select(name, gender, hair_color, height) %>% 
  filter(!complete.cases(.)) # excl mark shows filter for missing values only, ie: incomplete cases
# removes excl mark to show values without missing data

data %>% 
  select(name, gender, hair_color, height) %>%
  filter(!complete.cases(.)) %>% 
  drop_na(height) # removes missing data w.r.t selection
  
data %>%  # replace NA with something else in instances where there are no values
  select(name, gender, hair_color, height) %>%
  filter(!complete.cases(.)) %>% 
  mutate(hair_color = replace_na(hair_color, "none")) # creates a new variable or writes over existing variables
# Can replace none with any other values

#Duplicates 
#Example lets create a dataframe
names <- c("Peter", "John", "Andrew", "Peter")
age<- c(22, 33, 44, 22)
friends <- data.frame(names, age)
duplicated(friends) # returns a logical vector, fourth obs. TRUE

friends[duplicated(friends),] # shows duplicated data points in base R
friends[!duplicated(friends),] #shows obs. that are not duplicated in base R

friends %>%  # shows obs. that are not duplicated in tidyverse
  distinct() %>% #  can continue with next line of code
  View()
#Recoding variables 
data %>% select(name, gender)

data %>% 
  select(name, gender) %>% # recodes gender from masculine and feminine to 1,2.
  mutate(gender = recode(gender, "masculine" = 1,
                                 "feminine" 2)) %>% 
  View()
data %>%  # creates a new variable called gender-coded, leaves the var gender
  select(name, gender) %>% # recodes gender from masculine and feminine to 1,2.
  mutate(gender-coded = recode(gender, "masculine" = 1,
                                        "feminine" 2)) %>% 
 ## Data distribution ####
boxplot(data) # returns the distribution of data
hist(data) # retunr the normality of disttribution

#Download Pacman package
install.packages("pacman")
library(pacman)
install.packages("psych")
p_load(psych)

##Describe your data from Pacman package###
#For quantitative variable only

describe(data)  # Describes entire data frame
describe(irir$Sepal.Lenght) # Describs one quantotative variable


## VARIABILITY AND ASSOCIATION ANALYSIS####
install.packages("variability")
library(variability)
library(readxl)
geno <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/MSV Analysis.xlsx", 
                   sheet = "MeanCom1")
View(geno)
names(geno)

#Estimation of genotypic and phenotypic variances
gen.var(geno[3:8], geno$Genotype, geno$Rep) # select columns for data
#Returns the variance for all parameters
#Remove parameters with negative genotypic variance from the columns say [-6], then
gen.var(geno[3:8], geno$Genotype, geno$Rep)

#Genotypic and Phenotypic Correlation
geno.corr(geno[3:8], geno$Genotype, geno$Rep)
pheno.corr(geno[3:8], geno$Genotype, geno$Rep)


#Genotypic and Phenotypic Path Analysis
geno.path(geno$GRY,geno[3:8], geno$Genotype, geno$Rep)
pheno.path(geno$GRY, geno[3:8], geno$Genotype, geno$Rep)

##PATH ANALYSIS####
geno <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Ivy's data/M4 DATA EXCEL new cleaned.xlsx", 
                   sheet = "new")
View(mod)
names(geno)

#Convert as.factor where possible
geno$Genotypes <- as.factor(geno$Genotypes)
geno$Replication <- as.factor(geno$Replication)

#Step 1: Model specification ####

mod.ide = " 

NPL ~   SW + PL + S/P + PH4 + PH2


"
#Step 2: Model estimation and identification####
library(lavaan)
mod.est = sem(
          model = mod.ide,
          data = geno
)
summary(mod.est)
summary(mod.est,
        fit.measures =TRUE, rsquare= TRUE)

#Step 3: Path diagram ####
library(semPlot)
semPaths(
          object = mod.est,
          what = "path",
          whatLabels = "par",
          style = "ram",
          layout = "spring",
          rotation = 1,
          sizeMan = 7,
          sizeLat = 5,
          color = "#78BDE5",
          edge.label.cex = 1.5,
)


### Path Analysis  (Sequential Equation Modelling, SEM ) ####
##Showing equivalence between ultiple linear regression & SEM(path analysis)

install.packages("readr")
library(readr)
install.packages("lavaan")
library(lavaan)

#Specify model (additive model) with SEM ##
specmod <- "
Yield ~ PH + ED + EH + RL
 
"

#Estimation of model ##
fitmod <- sem(specmod, data=df)

##Summarise the results ##
summary(fitmod, fit.measures = TRUE, rsquare = TRUE)

#Specify model (additive model) with linear model ##
#Specify and estimate model
fitmod <- lm(Yield ~ PH + ED + EH + RL, data=df)
#Summarise the results
summary(fitmod)

##Note: Model fit indices conditions
#Chi square test:  supports that model fits data when (p>.05)
#Comparative Fit Index(CFI): supports that model fits data when (CFI >.90)
#Tucker-Lewis Index: supports that model fits data when (TLI >.95)
#Root Mean Square of Approximation (RMSEA): supports that model fits data when (RMSEA <.08)
#Standardized Root Mean Square Residual (SRMR): supports that model fits data when (SRMR <.08)

## PUBLICATION READY ANOVA TABLE ####
library(readxl)
library(dplyr)
library(tibble)
library(flextable)

head(data)
str(data)

#Convert categorical variables to factor variables 
data$Rep = as.factor(data$Rep)
data$Rep = as.factor(data$Water)
data$Priming = as.factor(data$Priming)
str(data)
attach(data)

#Analysis of variance ####
#Build column names, first response varible in 4th column


#Write for loop to repeat it for all response varibles in dataset

for (i in 1:ncol(data[-c(1:3)])) {
  cols <- names(data)[4:ncol(data)]
  
  aov.model <- lapply(x=cols, FUN =function(x)
    aov(reformulate(termlabels = "Rep + Water * Priming", 
                    response = x), 
        data = data))
  anova(aov.model[[2]])
  
  ##Print df, MS and Pvalue
  final = anova(aov.model[[2]])[, c(1,3,5)] # choose columns for df, MS and pvalue
  final
  
  #Getting row names 
  rnames = rownames(final)
  
  #Setting column names 
  colnames(final) = c("DF", "MS", "P-value")
  colnames(final)[2] = cols[i]
  final
  
  ##Rounding values to 2 decimal places 
  final = as.data.frame(round(final, digits =2))
  final
  
  #Assigning aterisks according to p values
  final$sign[final$"P-value" < 0.05] <- "*"
  final$sign[final$"P-value" < 0.01] <- "**"
  final$sign[final$"P-value" > 0.05] <- "ns"
  final
  
  ##Merge MS and significance column together ##
  final[[2]] = paste(final[[2]],
                     ifelse(is.na(final[[4]]), "", final[[4]]))
  final = final[-c(3,4)] # reove extra columns from ANOVA table
  
  #Writing anova tables in excel sheets
  anova = writexl::write_xlsx(final,
                              path = paste(cols[i], "-ANOVA.xlsx"))
  
  
  ## Reading excel files containing ANOVA tables
  file.list <- list.files(pattern = "*-ANOVA.xlsx")
  df.list <- lapply(x= file.list, FUN = read_excel)
  
  #Combined ANOVA table for all variables
  aov.table = rlist::list.cbind(df.list)
  View(aov.table)
  
  ##Remove duplicated columns for DF
  dup.cols = which(duplicated(names(aov.table)))
  aov.table = aov.table[,-dup.cols]
  View(aov.table)
  
  ##Write names for sources of variation
  
  rownames(aov.table) = rnames 
  View(aov.table)
  
}

##Printing the ANOVA table in MSword####
table = flextable(data = aov.table %>% 
                    rownames_to_column("SOV"))
bold(table, bold = TRUE, part = "header")


###VISUALIZING SCATTER PLOTS ####
#Clear R environment 
rm(list = ls(all = TRUE))  # Removes all list from global environment
graphics.off() #Clears all graphics
shell("cls")

#Import data set

data = read_excel(..............)
      header =TRUE
      
fix(data) # allows to view and edit the dataset
attach(data) # Masks the components of dataset and gives direct access to components of dataframe

#Access variables by typing the name of the variable as written in the data

#Visualizing association

#Draw a scatterplot for two variables 

install.packages("ggpubr")
library(ggpubr)

colnames(data) #Prints the names of each column to easily see them

ggscatter(
  data = data,
  x = "name of column", # show relationship between two data columns
  y = "name of column",
  xlab ="label for x-axis",
  ylab = "label for y-axis",
  add = "reg.line", # add regression line
  conf.int = TRUE, # set confidence interval
  conf.int.level = 0.95, #set value for confidence interval
  add.params = list(
                color = "black",
                size = 1,
                linetype = 1, # linetype of regression fitting
                fill = "lightgray",
                ),
  cor.coef = TRUE, # add correlation coefficient 
  cor.method = "pearson", #specify the correlation method, prints values in single line
  cor.coeff.args = list(label.sep = "\n") 
  
)


#Visualizing a matrix of scatterplots

pairs(
  
  x = data[7:10], #specify range of column numbers to use in scatterplot
  main = "A matrix of scatterplots", #specify name of scatterplot
  bg = c("red","green3","blue"),
  upper.panel = NULL
)

##ggplot2 based generalized pairs of plots
library(GGally)
library(ggplot2)

ggpairs(
  data = data,
  columns = 7:10,
  mapping = ggplot2::aes(color = priming), #Priming is an example column name
  upper = list(continuous = "cor"),
  lower = list(continuous = "points"),
  diag = list(continuous = "densityDiag"),
)

## PLOTTING BARGRAPHS WITH SE, ERROR BARS AND ALPHABETS####

rm(list = ls(all = TRUE))  # Removes all list from global environment
graphics.off() #Clears all graphics
shell("cls")

#Import data set

data = read_excel(..............)
header =TRUE

#Applying ANOVA Model###
install.packages("stats")
library(stats)

##Model specification

aov.res <- aov(activity ~ genotype + gender + genotype:gender) #OR
aov.res<- aov(activity ~ genotype*gender)
anova(aov.res)

##Multiple comparasion test by using LSD test
library(agricolae)

##First Factor Variable (Factor A)
LSD_A <- LSD.test(y = activity,
                  trt = genotype, #specify the first factor variable
                  DFerror = aov.res$df.residual,
                  MSerror = deviance(aov.res)/aov.res$df.residual,
                  alpha = 0.05,
                  p.adj = "bonferroni",
                  group = TRUE,
                  console = TRUE)

##Second Factor Variable (Factor B)
LSD_A <- LSD.test(y = activity,
                  trt = gender, #specify the second factor variable
                  DFerror = aov.res$df.residual,
                  MSerror = deviance(aov.res)/aov.res$df.residual,
                  alpha = 0.05,
                  p.adj = "bonferroni",
                  group = TRUE,
                  console = TRUE)

##Interaction Effects
##LSD Test
##First Factor Variable (Factor A)
LSD_AB <- LSD.test(y = activity,
                  trt = gentotype:gender, #specify the interaction variables
                  DFerror = aov.res$df.residual,
                  MSerror = deviance(aov.res)/aov.res$df.residual,
                  alpha = 0.05,
                  p.adj = "bonferroni",
                  group = TRUE,
                  console = TRUE)

##Arrange treatment groups in ascending order of groups for lettering in bargraph
##Ascending order of LSD$group for factor A.
ascend_A = LSD_A$groups %>% 
          group_by(rownames(LSD_A$groups))
          arrange(rownames(LSD_A$groups))
          
print(ascend_A)

##Ascending order of LSD$group for factor B.
ascend_B = LSD_B$groups %>% 
  group_by(rownames(LSD_B$groups))
arrange(rownames(LSD_B$groups))

print(ascend_B)


##Ascending order of LSD$group for interaction 
ascend_AB = LSD_AB$groups %>% 
  group_by(rownames(LSD_AB$groups))
arrange(rownames(LSD_AB$groups))

print(ascend_AB)

##Viisualizing effects
##Getting mean and SE as data frame
install.packages("dplyr")
library(dplyr)

##Main effects
##Mean and SE for factor A
MeanSE_A = data %>% 
          group_by(genotype) %>% 
          summarize(avg_A = mean(activity),
                    se = sd(activity)/sqrt(length(activity)))
print(MeanSE_A)
attach(MeanSE_A)

##Mean and SE for factor B
MeanSE_B = data %>% 
  group_by(genotype) %>% 
  summarize(avg_B = mean(activity),
            se = sd(activity)/sqrt(length(activity)))
print(MeanSE_B)
attach(MeanSE_B)

##Mean and SE for factor A*B
MeanSE_AB = data %>% 
  group_by(genotype) %>% 
  summarize(avg_AB = mean(activity),
            se = sd(activity)/sqrt(length(activity)))
print(MeanSE_AB)
attach(MeanSE_AB)

##Plotting Main effects
library(ggplot2)

##For first factor, A.

p1 = ggplot(MeanSE_A, aes(x = genotype,
                          y = avg_A))
print(p1)

##Adding layers to P1 object
pA = p1 + 

##Plotting bars
geom_bars(stat = "identity",
          color = "black",
          position = position_dodge(width = 0.9),
          width = 0.8) +  #specifies the width of the bars
##Adding error bars
geom_errorbar(aes(ymax = avg_A + se,
                  ymin = avg_A - se),
              position = position_dodge(width = 0.9),
              width = 0.25) + 
  
##Changing main titles, x and y labels
  labs(title = "Genotype effect on MPI activity",
       x = "genotype",
       y = "MPI activity") +
  
#Adding lettering for the test applied (LSD$group)
geom_text(aes( x = genotype,
               y = avg_A + se,
               llabel = as.matrix(ascend_A$groups)),
          position = position_dodge(width = 0.9),
          vjust = -(0.5))
print(pA)

##For second factor, B.

p2 = ggplot(MeanSE_B, aes(x = genotype,
                          y = avg_B))
print(p2)

##Adding layers to P1 object
pB = p2 + 
  
  ##Plotting bars
  geom_bars(stat = "identity",
            color = "black",
            position = position_dodge(width = 0.9),
            width = 0.8) +  #specifies the width of the bars
  ##Adding error bars
  geom_errorbar(aes(ymax = avg_B + se,
                    ymin = avg_B - se),
                position = position_dodge(width = 0.9),
                width = 0.25) + 
  
  ##Changing main titles, x and y labels
  labs(title = "Genotype effect on MPI activity",
       x = "genotype",
       y = "MPI activity") +
  
  #Adding lettering for the test applied (LSD$group)
  geom_text(aes( x = genotype,
                 y = avg_B + se,
                 llabel = as.matrix(ascend_A$groups)),
            position = position_dodge(width = 0.9),
            vjust = -(0.5))
print(pB)

##Plotting interactions
library(ggplot2)
p = ggplot(MeanSE_AB, aes(x = gender,
                          y = avg_AB,
                          fill = factor(genotype)))
print(p)

##Adding layers
##Plotting bars
pAB = p +
          geom_bar(stat = "identity",
                   color = "black",
                   position = position_dodge(width = 0.9)) +
  
##Adding layers for fill and changing legend text
          scale_fill_manual(values = gray(1:3/3),
                            labels = c("FF", "FO", "OO")) + 
#Adding error bars
          geom_errorbar(aes(ymax = avg_AB +se,
                            ymin = avg_AB - se),
                        position = position_dodge(width = 0.9),
                        width = 0.25) +
  ##Changing main titles, x and y labels
  labs(title = "Interation effect on MPI activity",
       x = "",
       y = "MPI activity",
       fill = "genotype") +
  
  #Adding lettering from test applied 
  geom_text(aes(x = gender,
                y = avg_AB + se,
                label = as.matrix(ascend_AB$groups)),
            position = position_dodge(width = 0.9),
            vjust = -(0.5))
print(pAB)

#### MEAN COMPARASIONS#### https://www.youtube.com/watch?v=m-PyIuyEnQE####


library(readxl)
Msvish <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/MSV Analysis.xlsx", 
                     sheet = "Infincib")
View(Msvish)

###Levene's test for homogeneity of variances # https://www.youtube.com/watch?v=Zo8Z9osPFTE####
str(Msvish)
summary(Msvish)
names(Msvish)
Msvish$Genotype <- as.factor(Msvish$Genotype)
Msvish$Rep <- as.factor(Msvish$Rep)

#Apply linear model and look at the results
Msvish.mod1 <- lm(Inc ~ Genotype + Rep, data = Msvish)
Msvish.mod1
anova(Msvish.mod1)
str(Msvish)
plot(Msvish.mod1)
hist(Msvish.mod1)
boxplot(Msvish.mod1)

##Testing for homogeneity using Levene's test
##Extract residual values
Msvish.res <- residuals(Msvish.mod1)
Msvish.res

#add these residuals to original dataframe
Msvish$Res <- Msvish.res
#For Levenes's original test, use absolute values
Msvish$Absres <- abs(Msvish.res)
#For a more robust test, use squared residuals
Msvish$Absres2 <- Msvish.res^2
head(Msvish)

#Test for homogeniety of variances 
#Levene's absolute values
leveneABS.mod <- lm(Absres ~ Genotype, data = Msvish )
anova(leveneABS.mod)

##Levene's abs quard values for more robustness
leveneRES2.mod <- lm(Absres2 ~ Genotype, data = Msvish )
anova(leveneRES2.mod)

##Another way to for Levenes Test####
library(car)
leveneTest(Msvish.mod1, center = mean)# same as abs(res)
leveneTest(Msvish.mod1, center = median) # the default

##Alternative homogeneity of variance tests 
#Bartletts test
#For parametric tests, very sensitive todeprtures from homogeneity

bartlett.test(Inc ~ Genotype, data =Msvish )
#Fligner test
#Non parametric test
fligner.test(Inc ~ Genotype, data =Msvish)
fligner

##Arc Sine transformation of dataset ####
#perform arcsine transformation on values in 'var1' column
asin(sqrt(Msvish))

##### Mean seperation continued....####
library(readxl)
Msvish <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/MSV Analysis.xlsx", 
                     sheet = "MeanNat")
View(Msvish)

Msvish$Genotypes <- as.factor(Msvish$Genotypes)
Msvish$Replication <- as.factor(Msvish$Replication)

str(Msvish)
names(Msvish)
Msvish.mod<- lm(GRY ~ Genotype + Rep, data = Msvish)
anova(Msvish.mod)
View(Msvish)
plot(Msvish)
boxplot(Msvish)

##Fixed Range Tests ##
library(agricolae)
##LSD
LSD <- LSD.test(Msvish.mod, "Genotype")
LSD
summary(LSD)

#### Plotting the multiple comparison of means #####
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus,data=sweetpotato)
comparison<- LSD.test(model,"virus",alpha=0.01,group=TRUE)

#startgraph
op<-par(cex=1.5)
plot(comparison,horiz=TRUE,xlim=c(0,50),las=1)
title(cex.main=0.8,main="Comparison between\ntreatment means",xlab="Yield",ylab="Virus")


####DATA STANDARDIZATION OR SACLING####
##min_max normalization #
library(readxl)
datrans <- read_excel("C:/Users/8th gen L480/Downloads/Compressed/ANALYSED DATA - ALL/ANALYSED DATA - ALL/LANDRACES EVALUTION DROUGHT AND WELL-WATERED/WELL-WATERED/USE FOR ANALYSIS ANALYSIS/LANDRACES WELL-WATERED COMBINED.xlsx", 
                      sheet = "Cleaned ")

min_max <- function(x){
  res <- (x - min(x))/(max(x) - min(x))
  return(res)
}
newd <- as.data.frame(sapply(datrans[,5:17], min_max))
newd   
newd(summary)
names(datrans)
### z score standardization
z_score <- function(x){
  res <- (x - mean(x))/sd(x)
  return(res)
}
newd1 <- as.data.frame(sapply(datrans[,5:17], z_score))
newd1
summary(newd1)

##########Export to excel ###
install.packages("rio")
library(rio)
export(newd1, "WellwaterdtDesktop.xlsx")  # check Files in r packages window to find saved file
##or
write.csv(newd1, file ="stand.csv")

###RANDOMIZED COMPLETE BLOCK DESIGN, FOR INBREDS ####
library(agricolae)
library(readxl)
Msvish <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/Heterosis for graphs.xlsx", 
                     sheet = "Hetinfinbred")
View(Msvish)
Msvish.mod<- lm(GRY ~ Genotype + Rep, data = Msvish)
anova(Msvish.mod)
library(agricolae)
##LSD
LSD <- LSD.test(Msvish.mod, "Genotype", alpha = 0.05, console = TRUE)
LSD
summary(LSD)

####ALPHA LATTICE DESIGN ANALYSIS, FOR HYBRIDS ####
library(readxl)
Msvish1 <- read_excel("C:/Users/8th gen L480/Downloads/Compressed/ANALYSED DATA - ALL/ANALYSED DATA - ALL/LANDRACES EVALUTION DROUGHT AND WELL-WATERED/ACROSS ENVIRONMENT/USE FOR ANALYSIS/RESULTS.xls", 
                      sheet = "Cleaned")
View(Msvish1)
names(Msvish1)
str(Msvish1)
Msvish1$REP = factor(Msvish1$REP)
Msvish1$BLK = factor(Msvish1$BLK)
Msvish1$ENVT = factor(Msvish1$ENVT)
Msvish1$GENOTYPE = factor(Msvish1$GENOTYPE)
names(Msvish1)
names(Msvish1)

##model
lats <- aov(POLLEN ~ REP + BLK +  GENOTYPE + REP:BLK + GENOTYPE:ENVT , data = Msvish1)
summary(lats)
##mean seperation 
##LSD
LSD <- LSD.test(lats, "GENOTYPE", alpha = 0.05, console = TRUE)
LSD
summary(LSD)


###Line graph of Incidence and severity scores##https://www.youtube.com/watch?v=UhQyzOwQmOI####


###DISEASE DEVELOPMENT MODELLING ####
install.packages("deSolve") ###https://kinglab.eeb.lsa.umich.edu/480/nls/de.html
library(deSolve)


### GOMPERTZ FUNCTION ####

install.packages("data.table")
install.packages("nlme")
library(data.table)
library(nlme)
library(readxl)
gomfun <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/Data sheet.xlsx", 
                         sheet = "Gompertz")
View(gomfun)


#Plot figure
plot(D1, D2, D3, D4, D5, INC, pch=4)

#Non-linear regression. METHOD 2 : SSgompertz
names(gomfun)

output <- nls(Genotype~ SSgompertz(Rep, Day, Inc), data=gomfun)

summary(output)

gmod1 <- nlme(gomfun$Genotype ~ SSasymp(Rep, Asym, Day, Inc),
              data = gomfun,
              fixed = Asym + Day + Inc ~ 1,
              random = Asym ~ 1,
              start = c(Asym = 1.83, Rep = 1.5, Inc = 0.04))
gommd2 <- del.frame.default(formula = ~gomfun + Genotype + Rep, data = gomfun
summary(gommd2)
fm2 <- update(fm1, random = pdDiag(Asym + lrc ~ 1))
summary(fm2)

mod3  <- gompertz(gomfun$Day, 2.5, 0.5, 0.3)
mod3
plot(mod3, gomfun$Inc)
###Nother package####
install.packages('growthmodels')
library(growthmodels)

#############
library(readxl)
library(readxl)
mdinf<- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/Data sheet.xlsx", 
                         sheet = "MAV-INF")
View(mdinf)
attach(mydata22)


## Checking normality od data distribution
install.packages('rcompanion')
library(rcompanion)
library(readxl)
myinf<- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/Data sheet.xlsx", 
                         sheet = "MSVINFF")
View(myinf)

attach(myinf)
myinf1 = c( D1, D2, D3, D4, D5, D6, D7, D8, INC, MORTALITY, SEV)
qqnorm(myinf1)
qqline(myinf1, col="blue")
plotNormalHistogram(myinf1)##test reveals data is positvely skewed, not normally distributed

##square root transformation
myinf1_sqrt = sqrt(myinf1)
plotNormalHistogram(myinf_sqrt)### still not normally distributed
library(psych)
skew(myinf1)  ## skew value = 2.758861, too high

#Visualizing a matrix of scatterplots

pairs(
  
  x = myinf[2:12], #specify range of column numbers to use in scatterplot
  main = "A matrix of scatterplots", #specify name of scatterplot
  bg = c("red","green3","blue"),
  upper.panel = NULL
)
## cube root transformation
myinf_cub = sign(myinf1) * abs(myinf1)^(1/3)
plotNormalHistogram(myinf_cub)
skew(myinf_cub)
hist(myinf_cub)

##log transformation
myinf_log = log(myinf1 +1) ### add 1 before transforming when zeros are present
plotNormalHistogram(myinf_log)
skew(myinf_log)
hist(myinf_log) ### PRODUCED NaNs

##Tukey's Ladder of powers transformation

myinf_tuk = transformTukey(myinf1, plotit = TRUE)
plotNormalHistogram(myinf_tuk) ## data is normally distributed
skew(myinf_tuk)## skewness  = 0.3564548, p-value = 4.887e-28
boxplot(myinf_tuk)
summary(myinf_tuk)


  
####MULTIPLE BOXPLOTS ####
library(ggplot2)
library(agricolae)
library(dplyr)
library(readxl)
bp <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/MSV Analysis.xlsx", 
sheet = "Boxplots")
View(bp)
names(bp)
bp$Genotype = as.factor(bp$Genotype)
bp$Rep = as.factor(bp$Rep)
##LSD
value_max = bp %>% group_by(Genotype) %>% summarize(max_value = max(PH))
hsd = HSD.test(aov(PH ~ Genotype, data = bp), trt = "bp", group = T)
sig.letters <- hsd$groups[order(row.names(lsd$groups)),]
waka <- ggplot(data = bp, aes(x = Genotype, y=PH)) + 
  geom_boxplot(aes(fill=Genotype)) + geom_text(data = value_max, aes(x=Genotype))
  y= 0.1 + max_value, label = sig.letters$groups), vjust = 0)+stat_boxplot(
    width = 0.1) +ggtitle("a") + xlab("Incidence(%)") + ylab("Plant height")





##AREA UNDER THE DISEASE PROGRESS CURVE ####
##AUDPS

library(agricolae)
dates <- c(7, 14, 21, 28,35,42, 49) # input number of days
# example 1: evaluation - vector
evaluation <- c(13,22, 28, 45, 56, 65, 72)# input percentages of respective evaluation days
audpc(evaluation, dates)
 

# example 2: evaluation: dataframe nrow=1
evaluation <- data.frame(E1 = 13, E2 = 22, E3 = 28, E4 = 45, E5= 56, E6= 65, E7= 72) # percentages of respective evaluation days, change to match evaluation dates
plot(dates, evaluation, type = "h", ylim = c(0, 100), col = "red", axes = FALSE, cex= 3.5)
title(cex.main = 1.0, main = "Absolute or Relative AUDPC\nTotal area = 100*(49-7)=4200") #Caculate total area by subtracting the first evaluation day from the last
lines(dates, evaluation, col = "red", cex = 3.5)  ##Cex= 0.85, sets the font size
text(dates, evaluation + 5, evaluation)
text(10, 7, "A = (14-7)*(22+13)/2", cex= 0.65) #adjust each x and y value to set into position "A = (14-7)*(23+13)/2" within each trapeziod
text(18, 13,"B = (21-14)*(28+22)/2",cex= 0.65)
text(25, 15, "C = (28-21)*(45+28)/2",cex= 0.65)
text(32, 18, "D = (35-28)*(56+45)/2",cex= 0.65)
text(39, 22, "E = (42-35)*(65+56)/2",cex= 0.65)
text(46, 26, "F = (49-42)*(72+65)/2",cex= 0.65)
text(13, 85, "audpc = A+B+C+D+E+F = 1809",cex= 0.8) # audpc = value returned fron evalution result
text(13, 70, "relative = audpc/area = 0.431",cex= 0.8) # divide evalution result by area result
abline(h = 0)
axis(1, dates)
axis(2, seq(0, 100, 5), las = 2)
lines(rbind(c(7, 13), c(7, 100)), lty = 8, col = "green", ) # adjust to position dotted green lines
lines(rbind(c(7, 100), c(49, 100)), lty = 8, col = "green")
lines(rbind(c(49, 76), c(49, 100)), lty = 8, col = "green")



#################################################
dates<-c(14,21,28) # days
# example 1: evaluation - vector

dates<-c(7,21,49)
# example 1: evaluation - vector

evaluation<-c(16.5, 47.90, 56.49)
audps(evaluation,dates)
audps(evaluation,dates,"relative")


x<-seq(10.5,31.5,7)
y<-c(40,80,90,90)
plot(x,y,"s",ylim=c(0,100),xlim=c(10,32),axes=FALSE,col="red" ,ylab="",xlab="")
title(cex.main=0.8,main="Absolute or Relative AUDPS\nTotal area=(31.5-10.5)*100=2100",
      ylab="evaluation",xlab="WAP" )
plot(x,y,"s",ylim=c(0,100),xlim=c(16,62),axes=FALSE,col="red" ,ylab="",xlab="")
title(cex.main=0.3,main="Absolute or Relative AUDPS\nTotal area=(60.5-15.5)*100=2100",
      ylab="evaluation",xlab="dates" )
points(x,y,type="h")
points(x,y,type="h")
z<-c(14,21,28)
points(z,y[-3],col="blue",lty=2,pch=19)
points(z,y[-3],col="blue",lty=2,pch=19)
axis(1,x,pos=0)
axis(2,c(0,40,80,90,100),las=2)
text(dates,evaluation+5,dates,col="blue")
text(14,20,"A = (17.5-10.5)*40",cex=0.8)
text(21,40,"B = (24.5-17.5)*80",cex=0.8)
text(28,60,"C = (31.5-24.5)*90",cex=0.8)
text(14,95,"audps = A+B+C = 1470")
text(14,90,"relative = audps/area = 0.7")
# It calculates audpc absolute
absolute<-audps(evaluation,dates,type="absolute")
print(absolute)
rm(evaluation, dates, absolute)


#### PRINCIPAL COMPONENT ANALYSIS ####
##https://www.youtube.com/watch?v=3QwZ2GgHSLE

library("ggplot2")
library("gridExtra")
library("factoextra")
library("ggbiplot")
library("corrplot")


library(readxl)
gally <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Gina's data/optimum_top_cross_wacci_nyankpala.xlsx", 
                    sheet = "cleaned")
View(gally)
#conversion of row number 

data=rownames(gally)=c("H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","He11","He12", "H13", "H14", "H15", "H16", "H17", "H18", "H19", "H20", "H21", "H22","H23","H24","H25",
                       "H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","He11","He12", "H13", "H14", "H15", "H16", "H17", "H18", "H19", "H20", "H21", "H22","H23","H24","H25",
                       "H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","He11","He12", "H13", "H14", "H15", "H16", "H17", "H18", "H19", "H20", "H21", "H22","H23","H24","H25",
                       "H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","He11","He12", "H13", "H14", "H15", "H16", "H17", "H18", "H19", "H20", "H21", "H22","H23","H24","H25")
pca =prcomp(gally[,c(3:17)], center = TRUE,scale. = TRUE)
print(pca)

# to get eigen value 
eig.val = get_eigenvalue(pca)
eig.val
#to get scree plot
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))
## PCA results for variables
var=get_pca_var(pca)
# to see the most contributing variables for each dimension
corrplot(var$cos2, is.corr=FALSE,)
#to see the most contributing variables for both dimension
fviz_cos2(pca, choice = "var", axes = 1:2,)
# to draw a bar plot of variable contributions
# Contributions of variables to PC1
a=fviz_contrib(pca, choice = "var", axes = 1)# top= 5 to limit to five var.
# Contributions of variables to PC2
b=fviz_contrib(pca, choice = "var", axes = 2)
grid.arrange(a,b, ncol=2, top='Contribution of the variables to the first two PCs')
# Total contribution on PC1 and PC2
fviz_contrib(pca, choice = "ind", axes = 1:2)
#Graph of variables
fviz_pca_var(pca,
             col.var = "cos2",
             gradient.cols = c("red", "blue", "green"),
             repel = TRUE)
#Biplot of individuals and variables
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "blue",
                col.ind = "red")
##Another way ##
#Plotting PCA
ggbiplot(pca)
#This will name each point with the name of the genotypes 
ggbiplot(pca,labels=rownames(GGE))
# plot using PC1 and PC2
ggbiplot(pca,ellipse=TRUE,choices=c(1,2),labels=rownames(GGE), groups=GGE$ENV)
# scale the samples
ggbiplot(pca,ellipse=TRUE,obs.scale = 2, var.scale = 4.5,
         labels=rownames(GGE), groups=GGE$ENV)
#remove the arrows altogether
ggbiplot(pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE,
         labels=rownames(GGE), groups=GGE$ENV)
# final biplot by Customize ggbiplot
aa=ggbiplot(pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(GGE), groups=GGE$ENV) +
  scale_colour_manual(name="Location", values= c("blue", "red", "green","pink"))+
  ggtitle("PCA of wakjira")+
  theme_minimal()+
  theme(legend.position = "bottom")
aa
ggsave(filename = "Waq.png", plot = aa,width = 22, height = 15, dpi = 2500, units = "cm")

### CLUSTER ANALYSIS ##### 
###https://www.youtube.com/watch?v=Hyqr4m7tle0
###scaling data ##
library(readxl)
clus <- read_excel("C:/Users/8th gen L480/Downloads/STANDADIZED GCA OF MULTIPLE TRAITS.xlsx")
View(clus)
inf.m <- scale(clus[,-1]) # remove columns not needed eg. genotypes
inf.m
d <- dist(inf.m)
options(max.print =10000000 )
d

###### k means ###
k2 <- kmeans(inf.m, 2)
k2
k2$cluster

### factoextra ##
install.packages("factoextra")
library(ggplot2)
library(factoextra)
fviz_nbclust(inf.m, kmeans, method = "wss")#mthods of finding optimum clusters
fviz_nbclust(inf.m, kmeans, method = "silhouette") #mthods of finding optimum clusters

###a. Nb clust for findng the optimum number of clusters ###
install.packages("NbClust")
library(NbClust)

NbClust(data= inf.m, diss=NULL, distance = "euclidean", min.nc=1, max.nc=2, 
             method = "kmeans", index = "all", alphaBeale = 0.1) # this tells the number of clusters

####Set the number of clusters according to number above, say 5 ##
setk <- kmeans(inf.m, 2)

## Seeing cluster membership ##
setk$cluster
#Plotting data set of clusters ##
plot(clus,cols=setk$cluster)

###b. Finding cluster number using Heirachical clustering ###
### Nb Clust using squared eculidean distance D2 ##
NbClust(data= inf.m, diss=NULL, distance = "euclidean", min.nc=2, max.nc=6, 
        method = "ward.D2", index = "all", alphaBeale = 0.1) # this tells the number of clusters
wardse <- hclust(d,"ward.D2")
plot(wardse)
##Plotting dendogram ##
plot(wardse, hang = -1)
plot(wardse, hang = -1, labels = clus$Genotype) # plots with names of genotypes

##Ploting dendrograms with 3 cluster divided by borders ###
rect.hclust(wardse, k=3, border = "blue")
rect.hclust(wardse, k=3, border = 2:5)
## Findinf the clustre membership ##
hcm <- cutree(wardse, k=3) # amend k the valus of k as appropriate
#List of membership of hirachical slustering ##
hcm
### inter and intra cluster distances ###
install.packages("clv")
library(clv)
## k-means ##
kid<- cls.scatt.data(inf.m, setk$cluster)
print(kid)
## h-clust ##
hid<- cls.scatt.data(inf.m, hcm)
print(hid)
## create excel sheet of intra and inter cluster distances#

## finding cluster means of average scores ##
aggregate(clus[, -1:-2], list(setk$cluster), mean)
## remove columns containing genotype, mean values of all variables show

###ANALYSIS OF DIALLEL CROSSES #### https://www.youtube.com/watch?v=cZyuDiaurwM
install.packages("plantbreeding",repos="http://r-forge.r-project.org")
library(plantbreeding)

### Full diallel analysis####
data(fulldial)

##Griffing, 1956, Model 1

out<- diallele1(dataframe = fulldial, male = "MALE", female = "FEMALE",
                progeny = "TRT", replication = "REP", yvar = "YIELD")
#print(out)
out$anvout # ANOVA
out$anova.mod1 #ANOVA for GCA and SCA effecs(Fixed)
out$components.model1 #model 1 GCA, SCA and reciprocal componenents
out$gca.effmat # gca effects 
out$sca.effmat # sca effects matrix
out$reciprocal.effmat # reciprocal effects matrix
out$varcompare # SE for comparasions

### Performing ANOVA using linear model, lm function ##
anout1 <- anova(lm(YIELD ~ as.factor(REP) + as.factor(TRT), fulldial))

### using lmDiallel package###
## Generalizes models to fit linear moseld with different reparameterizations

install.packages("lmDiallel")
library(lmDiallel)
md <- lm(YIELD ~ GCA(MALE,FEMALE) + tSCA(MALE, FEMALE) + 
           REC(MALE, FEMALE), data = fulldial)
summary(md)
anova(md)

###Griffings Diallel Analyses #### https://www.youtube.com/watch?v=uFt3EUGpPbU
##Methods for Griffing's Diallel Analysis:
#Method 1: Parents + F1's + Reciprocals;
#Method 2: Parents + One set of F1's;
#Method 3: One set of F1's and Reciprocals;
#Method 4: One set of F1's only;
-----------------------------------
#Models:
#Fixed Effects model
#Random Effects model

install.packages("DiallelAnalysisR")
library(DiallelAnalysisR)

#Griffings Method 1, Model 1;
DTF<- cbind(all parameters) #or y= name of parameter eg DTF

gm1m1 <- Griffing(y=DTF, Rep = REP, Cross1 = Parent1, Cross2 = Parent2, data = HalfDiallel,
                  Method = 1, Model = 1)
names(gm1m1)
gm1m1
gm1m1means <- gm1m1$Means
gm1m1ANOVA <- gm1m1$ANOVA
gm1m1Genetic.Components <- gm1m1$Effects
gm1m1Standarderror <- as.matrix(gm1m1$StdErr)

###Write results to excel or word##
sink("HalfDiallelResults.doc")
gm1m1 <- Griffing(y=DTF, Rep = REP, Cross1 = Parent1, Cross2 = Parent2, data = HalfDiallel,
                  Method = 1, Model = 1)
names(gm1m1)
gm1m1
gm1m1means <- gm1m1$Means
gm1m1ANOVA <- gm1m1$ANOVA
gm1m1Genetic.Components <- gm1m1$Effects
gm1m1Standarderror <- as.matrix(gm1m1$StdErr)
sink()


###Half diallel, fixed effects, parents + one set of F1's####
#Griffings Method 2, Model 1;
library(readxl)
hdl <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/Half Diallel Analysis.xlsx", 
                  sheet = "Combined")
View(hdl)
DTF<- cbind(hdl) #or y= name of parameter eg DTF
DTF$Cross1 <- as.factor(DTF$Cross1)
DTF$Cross2 <- as.factor(DTF$Cross2)
DTF$Rep <- as.factor(DTF$Rep)
names(DTF)
gm2m1 <- Griffing(y=GRY, Rep = Rep, 
                  Cross1 = Cross1, 
                  Cross2 = Cross2, 
                  data = DTF,
                  Method = 2, Model = 1)
names(gm2m1)
gm2m1
gm2m1Means <- gm2m1$Means
gm2m1ANOVA <- gm2m1$ANOVA
gm2m1Genetic.Components <- gm2m1$Genetic.Components
gm2m1Genetic.Components

gm2m1Effects <- gm2m1$Effects
gm2m1Effects
gm2m1StdErr <- as.matrix(gm2m1$StdErr)


###Write results to excel or word##
sink("HalfDiallelResults_LC.doc")
gm1m1 <- Griffing(y=GRY, Rep = Rep, Cross1 = Cross1, Cross2 = Cross2, data = DTF,
                  Method = 2, Model = 1)
names(gm1m1)
gm1m1
gm1m1means <- gm1m1$Means
gm1m1ANOVA <- gm1m1$ANOVA
gm1m1Genetic.Components <- gm1m1$Effects
gm1m1Standarderror <- as.matrix(gm1m1$StdErr)
sink()
names(DTF)


##Repeat code for all parameters, varying Methods and Models as appropriate ##


#Analysis of North Carolina II mating design ####
install.packages("plantbreeding")
library(plantbreeding)
data(northcaro2)
# for trait yield 
myo <- carolina2(dataframe = northcaro2, set = "set", male = "male", female = "female",
                 replication = "rep", yvar = "yield")
anova(myo$model) # anova
myo$var.m   ### Male variance
myo$var.f  ### Female variance
myo$var.mf  ##Male*Female variance
myo$var.Af  ##Additive male variance
myo$var.AM ##Additive male variance
myo$var.D  ## Dominance variance

### For specific traits
##for trait tuber  
tum <- carolina2(dataframe = northcaro2, set = "set", male = "male", female = "female", 
                 replication = "rep", yvar = "tuber")
anova(tum$model) 
anova(tum$model) # anova 
tum$var.m  ### Male variance
tum$var.f   ### Female variance
tum$var.mf  ####Male*Female variance
tum$var.Af  ##Additive male variance
tum$var.D  ## Dominance variance

###Calculation of selection index ####
library(plantbreeding)
s.index<- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/Gentopic analysis.xlsx", 
                                sheet = "Genetic gainestimates "
s_index <- as.data.frame(s.index)
data(s.index)
p <- seletion.index (phenodf = selindex1$phenodf, pcovmat = selindex1$X, 
                     gcovmat = selindex1$G, ecovmat = selindex1$A)
print(p)

######Multi-trait Genotype-Ideotype Distance Index (MGIDI) In R###https://www.youtube.com/watch?v=8XBNPCXS5hQ
#Create a mixed-effects model (genotype as random effect)
library(readxl)
mgid <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/MSV Analysis.xlsx", 
                   sheet = "MGIDI")
View(mgid)
names(mgid)
library(metan)
model <- gamem(mgid ,
               gen = GEN,
               rep = REP,
               block = BLOCK,
               resp = everything())
#BLUPS for genotypes
a = gmd(model, "blupg")
print(a, n=200)

#Compute the MGIDI index
aku <- mgidi(model)
mgidi_ind <- mgidi(model)
gmd(mgidi_ind, "MGIDI")
#Plot the contribution of each factor on the MGIDI index 
p1 <- plot(aku,type = "contributor")
p2 <- plot(aku)


