#Print name
print("Parth Shah")

#Loading packages through pacman
pacman::p_load(FSA, FSAdata, magrittr, dplyr, tidyr, plyr, tidyverse)

#Import inchBio dataset 
library(readr)
Bio <- read_csv("~/R/ALY6000/Module3/inchBio.csv")
View(Bio)

#Head, Tail, Structure
headtail(Bio)
structure(Bio)

#Create an object, <counts>, that counts and lists all the species records
counts <- table(Bio$species)
counts

#Display just the 8 levels (names) of the species
unique(Bio$species)

#Create a <tmp> object that displays the different species and the number of record of each species in the dataset. Include this information in your report.-
tmp <- table(Bio$species)
tmp

#Create a subset, <tmp2>, of just the species variable and display the first five records
tmp2 <- subset(Bio, select = species)
head(tmp2, 5)

#Create a table, <w>, of the species variable. Display the class of w
w <- table(Bio$species)
w
class(w)

#Convert <w> to a data frame named <t> and display the results
t <- as.data.frame(w)
t

#Extract and display the frequency values from the <t> data frame
t$Freq

#Create a table named <cSpec> from the bio species attribute (variable) and confirm that you created a table which displays the number of species in the dataset <bio>
cSpec <- table(Bio$species)
cSpec

#Create a table named <cSpecPct> that displays the species and percentage of records for each species. Confirm you created a table class.
CSpecPct <- prop.table(cSpec)*100
CSpecPct
class(CSpecPct)

#Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data frame
u <- as.data.frame(CSpecPct)
u
class(u)

#Barplot of <cSpec> with the following: titled Fish Count
barplot(cSpec,
     main = "Fish Count",
     ylab = "COUNTS",
     col = "lightgreen",
     cex.names = 0.60,
     las = 2
     )

#Create a barplot of <cSpecPct>, with the following specifications: 
#Y axis limits of 0 to 4 
#Y axis label color of Light Blue 
#Title of  "Fish Relative Frequency" 
barplot(CSpecPct,
        ylim = c(0,40),
        main = "Fish Relative Frequency",
        ylab = "COUNTS",
        col.lab = "lightblue")

#Rearrange the <u> cSpec Pct data frame in descending order of relative frequency. Save the rearranged data frame as the object <d> 
d <- u[order(-u$Freq),]
d

#Rename the <d> columns Var 1 to Species, and Freq to RelFreq
d
names(d)[names(d)=="Var1"] <- "Species"
d
names(d)[names(d)=="Freq"] <- "RelFreq"
print(d)

#Add new variables to <d> and call them cumfreq, counts, and cumcounts
counts
t$Freq
tdesc <- t[order(-t$Freq),]
tdesc$Freq
d <- d %>% mutate(cumfreq=cumsum(d$RelFreq), counts=tdesc$Freq, cumcounts=cumsum(tdesc$Freq))
d

#Create a parameter variable <def_par> to store parameter variables 
def_par <- par(no.readonly = TRUE)

# barplot <pc>
pc <- barplot(d$counts, width = 1, space = 0.15,border = NA, axes = F, ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, las=2, cex.names = 0.70, main = "Species Pareto", d$counts,na.rm=TRUE)


#Add a cumulative counts line to the <pc> plot with the following: 
#Spec line type is b 
#Scale plotting text at 70% 
#Data values are solid circles with color cyan4 
pc <- barplot(d$counts, 
              width = 1, 
              space = 0.15,
              border = NA, 
              axes = F, 
              main = "Species Pareto",
              ylim = c(0,3.05*228), 
              ylab = "Cummulative Counts", 
              names.arg = d$Species, 
              las=2, 
              cex.names = 0.70
              )
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")

#Place a grey box around the pareto plot
pc <- barplot(d$counts, 
              width = 1, 
              space = 0.15,
              border = NA, 
              axes = F, 
              main = "Species Pareto",
              ylim = c(0,3.05*228), 
              ylab = "Cummulative Counts", 
              names.arg = d$Species, 
              las=2, 
              cex.names = 0.70
)
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")
box(col = "grey62")

#Add a left side axis with the following specifications 
#Horizontal values at tick marks at cumcounts on side 2 
#Tickmark color of grey62 
#Color of axis is grey62 
#Axis scaled to 80% of normal
pc <- barplot(d$counts, 
              width = 1, 
              space = 0.15,
              border = NA, 
              axes = F, 
              main = "Species Pareto",
              ylim = c(0,3.05*228), 
              ylab = "Cummulative Counts", 
              names.arg = d$Species, 
              las=2, 
              cex.names = 0.70
)
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")
box(col = "grey62")
axis(side = 2, at = c(0, d$cumcounts), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)

#Add axis details on right side of box with the specifications: 
#Spec: Side 4 
#Tickmarks at cumcounts with labels from 0 to cumfreq with %, 
#Axis color of cyan5 and label color of cyan4 
#Axis font scaled to 80% of nominal 
pc <- barplot(d$counts, 
              width = 1, 
              space = 0.15,
              border = NA, 
              axes = F, 
              main = "Species Pareto",
              ylim = c(0,3.05*228), 
              ylab = "Cummulative Counts", 
              names.arg = d$Species, 
              las=2, 
              cex.names = 0.70
)
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")
box(col = "grey62")
axis(side = 2, at = c(0, d$cumcounts), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)
axis(side = 4, at = c(0, d$cumcounts), 
     labels = c(0, d$cumfreq),
     las = 1, 
     col.axis = "cyan3", #Error throughs that invalid cyan5 color 
     col = "cyan4", 
     cex.axis = 0.8)


#Display the finished Species Pareto Plot (without the star watermarks). Have your last name on the plot
pc <- barplot(d$counts, 
              width = 1, 
              space = 0.15,
              border = NA, 
              axes = F, 
              main = "Species Pareto\n Parth Shah",
              ylim = c(0,3.05*228), 
              ylab = "Cummulative Counts", 
              names.arg = d$Species, 
              las=2, 
              cex.names = 0.70
)
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")
box(col = "grey62")
axis(side = 2, at = c(0, d$cumcounts), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)
axis(side = 4, at = c(0, d$cumcounts), 
     labels = c(0, d$cumfreq),
     las = 1, 
     col.axis = "cyan3", #Error throughs that invalid cyan5 color 
     col = "cyan4", 
     cex.axis = 0.8)
