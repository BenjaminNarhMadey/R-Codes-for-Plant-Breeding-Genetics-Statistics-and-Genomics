#Download and install the Agricolae and Agrocolaeplotr packages 
###Alpha lattice design ####
library(FielDHub)
alpha <-  alpha_lattice(
  t = 15,
  k = 3,
  r = 4,
  l = 2,
  plotNumber = 101,
  locationNames = c("Jimma", "Kay"),
)
plot(alpha)
options(max.print = 999999)

###Completely randomised design ####

crd1 <- CRD(
  t = 10,
  reps = 5,
  plotNumber = 101,
  seed = 1987,
  locationName = "Fargo"
)

plot(crd1)

####Randomised Complete Block design ####
# Example 2: Generates a RCBD design with 6 blocks and 18 treatments in one location.
# In this case, we show how to use the option data.
treatments <- paste("ND-", 1:18, sep = "")
treatment_list <- data.frame(list(TREATMENT = treatments))
head(treatment_list)
rcbd2 <- RCBD(reps = 6, l = 1, 
              plotNumber = 101, 
              continuous = FALSE, 
              planter = "serpentine", 
              seed = 13, 
              locationNames = "IBAGUE",
              data = treatment_list)
print(rcbd2)
plot(rcbd2)
rcbd2$infoDesign                  
rcbd2$layoutRandom
rcbd2$plotNumber
head(rcbd2$fieldBook)

####Augmented design ####

checks <- 4;
list_checks <- paste("CH", 1:checks, sep = "")
treatments <- paste("G", 5:354, sep = "")
treatment_list <- data.frame(list(ENTRY = 1:354, NAME = c(list_checks, treatments)))
head(treatment_list, 12)
ARCBD2 <- RCBD_augmented(lines = 350, checks = 4, b = 17, l = 3, 
                         planter = "serpentine", 
                         plotNumber = c(101,1001,2001), 
                         seed = 24, 
                         locationNames = LETTERS[1:3],
                         data = treatment_list)

plot(ARCBD2)
ARCBD2$infoDesign
ARCBD2$layoutRandom
ARCBD2$exptNames
ARCBD2$plotNumber
head(ARCBD2$fieldBook, 12)

###Split Plot design ####

# Example 2: Generates a split plot design SPD with 5 whole plots 
# (4 types of fungicide + one control), 10 sub plots per whole plot (10 bean varieties), 
# and 6 reps in an RCBD arrangement. This in 3 locations or sites.
# In this case, we show how to use the option data.
wp <- c("NFung", paste("Fung", 1:4, sep = ""))  # Fungicides (5 Whole plots)
sp <- paste("Beans", 1:10, sep = "")            # Beans varieties (10 sub plots)
split_plot_Data <- data.frame(list(WHOLPLOT = c(wp, rep(NA, 5)), SUBPLOT = sp))
head(split_plot_Data, 12)
SPDExample2 <- split_plot(reps = 6, l = 3, 
                          plotNumber = c(101, 1001, 2001),
                          seed = 23, 
                          type = 2, 
                          locationNames = c("A", "B", "C"),
                          data = split_plot_Data)
plot(SPDExample2)
SPDExample2$infoDesign
SPDExample2$layoutlocations
head(SPDExample2$fieldBook,12)

####Full factorial #####
#Example 1: Generates a full factorial with 3 factors each with 2 levels.
# This in an RCBD arrangement with 3 reps.
fullFact1 <- full_factorial(setfactors = c(2,2,2), reps = 3, l = 1, type = 2,
                            plotNumber = 101,
                            continuous = TRUE,
                            planter = "serpentine",
                            seed = 325,
                            locationNames = "FARGO")
plot(fullFact1)
fullFact1$infoDesign
head(fullFact1$fieldBook,10)
