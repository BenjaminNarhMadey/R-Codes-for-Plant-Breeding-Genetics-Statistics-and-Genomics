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
