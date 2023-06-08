library(car)
qqPlot(new_plot$GRY)



library(readxl)
Msvish1 <- read_excel("C:/Users/8th gen L480/OneDrive/Desktop/Research data/GYT Biplot.xlsx", 
                      sheet = "z-transformed data")
View(Msvish1)
names(Msvish1)
new_plot <- Msvish1[,2:11]
#Correlation analysis and correlelograms
library(metan)
corrl <- corr_coef(new_plot)
plot(corrl)

#special packages for correlation analysis
#corrplot
install.packages("corrplot")
library(corrplot)
corrplot( corr = cor(corrl))

#Default function arguments

corrplot(corr = cor(new_plot),
         method = "circle",
         type = "full",
         tl.pos = "tl",
         order = "original")

corrplot(corr = cor(new_plot),
         method = "circle",
         type = "full",
         tl.pos = "tl",
         order = "AOE")

#change the order of the data to alphabetical order using 
# order = "alphabet"

corrplot(corr = cor(new_plot),
         method = "circle",
         type = "full",
         tl.pos = "tl",
         order = "alphabet")

#change the order of the data to angular order
# of eigenvectors using 
# order = "AOE"

corrplot(corr = cor(new_plot),
         method = "circle",
         type = "full",
         tl.pos = "tl",
         order = "AOE")

#change the order of the data 
# to first principal component order using 
# order = "FPC"

corrplot(corr = cor(new_plot),
         method = "shade",
         type = "full",
         tl.pos = "tl",
         order = "FPC")

#change the order of the data
# to heirachichal order order using 
# order = "hclust"

corrplot(corr = cor(new_plot),
         method = "square",
         type = "full",
         tl.pos = "tl",
         order = "hclust")


#change the order of the data
# to adding clustering rectangles  using 
# addrect = 2, rect.col = "green"

corrplot(corr = cor(new_plot),
         method = "ellipse",
         type = "lower",
         tl.pos = "tl",
         addrect = 2, rect.col = 'green')

corrplot(cor(new_plot),
         addCoef.col = "black",
         number.cex = 0.8,
         number.digits = 1,
         diag = TRUE,
         bg = "blue",
         outline = "white",
         addgrid.col = "white",
         mar = c(1,1,1,1))

corrplot(corr = cor(new_plot),
         addCoef.col = 'black',
         number.cex = 0.8,
         number.digits = 1,
         diag = FALSE,
         bg = "grey",
         outline = "black",
         addgrid.col = "black",
         mar = c(1,1,1,1))



col <- colorRampPalette(c("#7F0000", "red", "#FF7F00", 
                          "yellow", "white", "cyan", 
                          "#007FFF", "blue", "#00007F"))

corrplot(corr = cor(new_plot),
         method = "color",
         type = "lower",
         col = COL1(10),
         cl.length = 11,
         cl.pos = "b",
         tl.col = "black",
         addCoef.col = "grey",
         title = "A few more changes - Part 2",
         mar = c(1,1,1,1))

#Mixed corrplots
corrplot(cor(new_plot), order = "AOE", type = "upper", 
         tl.pos = "d", method = "number")

corrplot(cor(new_plot), add = TRUE, type = "lower", 
         method = "pie", order = "AOE", 
         diag = FALSE, tl.pos = "n", cl.pos = "n")

corrplot.mixed(cor(new_plot), order = "AOE", 
               lower = "pie", upper = "number",
               tl.pos = c("d"))

#ggpairs() and ggcorr()
# Very important plots

install.packages("GGally")
library(GGally)

GGally::ggpairs(iris, columns = 1:4)

#Adding species information and color
GGally::ggpairs(iris, columns = 1:4,
                ggplot2::aes(colour = Species))

#Visualization and correlations
GGally::ggcorr(new_plot, 
               method = c("everything", "pearson"),
               label = TRUE, label_alpha = TRUE)
#More variations

GGally::ggcorr(
  new_plot,
  name = expression(rho),
  geom = "tile",
  max_size = 10,
  min_size = 2,
  size = 3,
  hjust = 0.75,
  nbreaks = 6,
  angle = -45,
  palette = "Puor" # colorbind safe, photocopy able
  
)

## corrgram::corrgram()---
install.packages("corrgram")
library(corrgram)

#Basic
corrgram:corrgram(new_plot)

#First
corrgram::corrgram(new_plot, order = TRUE,
                   lower.panel = panel.shade,
                   upper.panel = panel.pie,
                   text.panel = panel.txt,
                   cor.method = "pearson",
                   main = "corrgram(mtcars"
)
#Second 
corrgram::corrgram(new_plot, order = TRUE,
                   lower.panel = panel.ellipse,
                   upper.panel = panel.pts,
                   txt.panel = panel.txt,
                   diag.panel = panel.minmax,
                   main = "corrgram(mtcars, diag.panel
                   = panel.minmax)")

#Third
corrgram::corrgram (new_plot, 
                  
                    order = TRUE,
                    lower.panel = panel.shade,
                    upper.panel = NULL,
                    text.panel = panel.txt)


#Fourth

corrgram (new_plot,
          lower.panel = panel.pts,
          upper.panel = panel.conf,
          diag.panel = panel.density,
          main = "corrgram(iris[1:4],
          panel.pts/.density/.conf)")

#Fifth - panel bar

corrgram(new_plot, order = TRUE, main= "corrgram
         (mtcars)- panel.bar",
         lower.panel = corrgram::panel.ellipse,
         upper.panel = panel.bar, diag.panel = panel.minmax,
         col.regions = colorRampPalette(
           c("darkgoldenrod4","burlywood1", "darkkhaki",
             "darkgreen")))

## pairs.panels() and corPlot() ----
install.packages("psych")
library(psych)

pairs.panels(new_plot)        

pairs.panels(new_plot,
             smooth = TRUE, #If true draws less smooths
             scale = TRUE, #If true scales the correlation text font
             density = FALSE, #If true, adds density plots and histograms
             ellipses = TRUE, #If true draws ellipses
             method = "pearson", #Correlationmethod also spearman or kendall
             pch = 21 + as.numeric(iris$Species), 
             bg = c("red", "yellow", "blue")[iris$Species],
             lm = TRUE, #iF true plots linear fit rather than the less smoothed fit
             cor = TRUE, #If true, reports correlations
             jiggle = FALSE, #If true, data ponts are jittered
             factor = 2, #Jittering factor
             hist.col = 4, #Histograms color
             stars = TRUE, #If true, adds significance level with stars
             ci = FALSE, #If true adds confidence intervals
             
)

#corPlot()

psych::corPlot(new_plot, cex = 0.9)
gr <- colorRampPalette(c("#B52127", "white", "#217185"))
corPlot(new_plot,
        cex = 0.9,
        numbers = TRUE,
        n = 21, #show with n=51)
        zlim = c(-1,1),
        stars = TRUE,
        diag = FALSE,
        gr = gr,
        main = "psych::corPlot(iris[1:4])"
)

## ggplot2 and geom_tile
install.packages("tidyverse")
library(tidyverse)

mtcars_cormat <- cor(new_plot) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(-rowname)

mtcars_cormat %>%
  ggplot(aes(x = rowname, y = name, fill = value))+
  geom_tile()+
  geom_text(aes(label = round(value, 2)), color = "white") +
  scale_fill_gradient2(low = "red",
                       high = "darkgreen",
                       mid = "white",
                       midpoint = 0,
                       limit = c(-1,1),
                       name = "Pearson\ncorrelation"
  )

##Performance Analytics----

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(new_plot,
                  histogram = TRUE,
                  method = "pearson",
                  pch="+")