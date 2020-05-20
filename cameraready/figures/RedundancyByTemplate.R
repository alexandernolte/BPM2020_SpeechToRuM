library(ggplot2)

serious_theme <- theme_classic() + # less fancy colors
  #increase size of gridlines
  theme(panel.grid.major = element_line(size = .5, color = "lightgrey"),
  #increase size of axis lines
  axis.line = element_line(size=.7, color = "black"),
  #Adjust legend position to maximize space, use a vector of proportion
  #across the plot and up the plot where you want the legend. 
  #You can also use "left", "right", "top", "bottom", for legends on t
  #he side of the plot
#  legend.position = c(.2,.9),
  #box around the legend
  legend.background = element_rect(colour = "black", size=.3),
  #increase the font size
  text = element_text(size=24)) 
## http://www.noamross.net/blog/2013/11/20/formatting-plots-for-pubs.html



data <- read.csv2(file="allresults.csv", sep=",", header=TRUE)

noDRC <- read.csv2(file="/home/claudio/University/Pubs/declarativeprocessmining/model-consistency-paper/test-on-BPIC2012/MINERful-OnlyThresholdAndHierarchyPruning-s075-c0125-i0125_RETURNED_MODEL-templatesonly.txt", quote="'", header=FALSE)
noDRC <- as.data.frame(table(noDRC))
names(noDRC)[names(noDRC)=="noDRC"] <- "Template"
dRC <- read.csv2(file="/home/claudio/University/Pubs/declarativeprocessmining/model-consistency-paper/test-on-BPIC2012/MINERful-s075-c0125-i0125_RETURNED_MODEL-templatesonly.txt", quote="'", header=FALSE)
dRC <- as.data.frame(table(dRC))
names(dRC)[names(dRC)=="dRC"] <- "Template"
noDRC$Freq <- noDRC$Freq - dRC$Freq
noDRC$Type <- "Redundant"
dRC$Type <- "Non-redundant"

data <- rbind(dRC,noDRC)
names(data)[names(data)=="Freq"] <- "Count"
data$Reduperc <- -100.0 * noDRC$Freq / (dRC$Freq + noDRC$Freq)
data$Total <- dRC$Freq + noDRC$Freq

pdf("/home/claudio/University/Pubs/declarativeprocessmining/model-consistency-paper/src/figures/RedundancyByTemplate.pdf")

barplot <- ggplot(data = NULL, aes(x = Template, y = Count)) + 
	geom_bar(data = data, stat="identity", aes(fill = Type)) +
	geom_text(data = data[data$Type == "Redundant",], aes(y = Total + 2, label = paste(round(Reduperc, digits = 1),"%",sep=""))) +
		scale_fill_grey() +
	    serious_theme +
	    theme(legend.position=c(.2,.9), axis.text.x = element_text(angle = 45, hjust = 1))

print(barplot)
dev.off()
