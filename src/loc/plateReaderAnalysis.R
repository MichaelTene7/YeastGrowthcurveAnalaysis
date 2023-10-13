library(ggplot2)
library(gridExtra)
library(growthcurver)

# -- read the data --
inData = mainData = read.csv("Data/PlateReader/second2490-3CompareClean.csv")
inData = mainData = read.csv("Data/PlateReader/third2490-3CompareClean.csv")
inData = mainData = read.csv("Data/PlateReader/lowQualityYeastCompare9-26Clean.csv")



# -- clean the data -- #
timeData = mainData[which(mainData[1] == "Time [s]"),]
timeData = timeData[1,]
rownames(timeData)= "time"
timeData = timeData[,-1]

meanData = mainData[which(mainData[1] == "Mean"),]
rownames(meanData) = mainData[(which(mainData[1] == "Mean")-3),1]
meanData = meanData[,-1]

cleanData = rbind(timeData, meanData)
cleanData = data.frame(t(cleanData))
cleanData = lapply(FUN = as.numeric, cleanData)
cleanData= data.frame(cleanData)

wellNames = names(cleanData)
wellNumbers = names(cleanData)

# -- generate meaningful well names -- 

# - conversion functions - 
convertBoth = function(firstSet, secondSet){
  wellNames = wellNumbers
  for(i in 1:length(firstSet)){
    conversionStep = firstSet[[i]]
    message(paste("replacing", conversionStep[1], "with", conversionStep[2]))
    wellNames[grep(conversionStep[1], wellNumbers)] = conversionStep[2]
  }
  for(i in 1:length(secondSet)){
    conversionStep = secondSet[[i]]
    message(paste("replacing", conversionStep[1], "with", conversionStep[2]))
    wellNames[grep(conversionStep[1], wellNumbers)] = paste(wellNames[grep(conversionStep[1], wellNumbers)], conversionStep[2], sep="_")
  }
  wellNames
}
# - 
rowMeanings = list(
  c("A","Mm_2490"),
  c("B", "Mm_2491"),
  c("C", "Mm_2492"),
  c("D", "Mm_2493"),
  c("E", "Csm_2490"),
  c("F", "Csm_2491"),
  c("G", "Csm_2492"),
  c("H", "Csm_2493")
)
colMeanings = list(
  c("1", "As_1"),
  c("2", "As_5"),
  c("3", "Glu_1"),
  c("4", "Glu_5"),
  c("5", "Ser_1"),
  c("6", "Ser_5"),
  c("7", "Thr_1"),
  c("8", "Thr_5")
)
rowFirst = F

wellNames = wellNumbers

if(rowFirst){
  wellNames = convertBoth(rowMeanings, colMeanings)
}else{
  wellNames = convertBoth(colMeanings, rowMeanings)
}

wellNames

names(cleanData) = wellNames

# -- make plotting function ---

combinedPlot = function(wells){
  plot <- ggplot( aes(x=time), data = cleanData)
  for (i in 1:length(wells)) { 
    loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="")
    plot <- plot + eval(parse(text=loop_input))  
  }
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot
}


# -- plot the data -- 
#allWells = names(cleanData)
#combinedPlot(allWells)

wells2490 = wellNames[grep("2490", wellNames)]
combinedPlot(wells2490)
wells2490mm = wellNames[grepl("2490", wellNames) & grepl("Mm", wellNames)]
wells2491mm = wellNames[grepl("2491", wellNames) & grepl("Mm", wellNames)]
wells2492mm = wellNames[grepl("2492", wellNames) & grepl("Mm", wellNames)]
wells2493mm = wellNames[grepl("2493", wellNames) & grepl("Mm", wellNames)]
mm2490plot = combinedPlot(wells2490mm)
mm2491plot = combinedPlot(wells2491mm)
mm2492plot = combinedPlot(wells2492mm)
mm2493plot = combinedPlot(wells2493mm)

grid.arrange(mm2490plot, mm2492plot, mm2491plot, mm2493plot, ncol = 2)


wells2490csm = wellNames[grepl("2490", wellNames) & grepl("Csm", wellNames)]
wells2491csm = wellNames[grepl("2491", wellNames) & grepl("Csm", wellNames)]
wells2492csm = wellNames[grepl("2492", wellNames) & grepl("Csm", wellNames)]
wells2493csm = wellNames[grepl("2493", wellNames) & grepl("Csm", wellNames)]
csm2490plot = combinedPlot(wells2490csm)
csm2491plot = combinedPlot(wells2491csm)
csm2492plot = combinedPlot(wells2492csm)
csm2493plot = combinedPlot(wells2493csm)
grid.arrange(csm2490plot, csm2492plot, csm2491plot, csm2493plot, ncol = 2)


#

# -- get growthcurver results for the plate -- 
cleanData$time = cleanData$time/60/60

growthcurverOutput = SummarizeGrowthByPlate(cleanData, plot_fit = TRUE, plot_file = "Output/GrowthCurves/YeastStrains.pdf")
write.csv(growthcurverOutput, "Output/GrowthCurves/yeastStrains.csv")




















# --------- old code ----------- #
mainData = read.csv("Data/PlateReader/Intial2490-3CompareClean.csv")
mainData[4,]

mainData$Cycles...Well

rownames(mainData) = mainData$Cycles...Well

mainData[which(mainData[1] == "Mean"),]

meanData = mainData[which(mainData[1] == "Mean"),]
timeData = mainData[which(mainData[1] == "Time [s]"),]
duplicated(timeData) # all time data is the same 
timeData = timeData[1,]
rownames(timeData)= "Time"
timeData = timeData[,-1]

mainData[(which(mainData[1] == "Mean")-3),1]
rownames(meanData) = mainData[(which(mainData[1] == "Mean")-3),1]
meanData = meanData[,-1]

meanData = rbind(timeData, meanData)
meanData2 = data.frame(t(meanData))

plot(meanData2$Time, meanData2$A1)

meanData3 = lapply(FUN = as.numeric, meanData2)
meanData3= data.frame(meanData3)

fields = names(meanData3)

combinedPlot = function(wells){
plot <- ggplot( aes(x=Time), data = meanData3)
for (i in 2:length(wells)) { 
  loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="")
  plot <- plot + eval(parse(text=loop_input))  
}
plot <- plot + guides( color = guide_legend(title = "",) )
plot
}

sevenAndEight = names(meanData3)[c(grep(7, names(meanData3)), grep(8, names(meanData3)))]
combinedPlot(sevenAndEight)

notSevenAndEight = names(meanData3)[-c(grep(7, names(meanData3)), grep(8, names(meanData3)),1)]
combinedPlot(notSevenAndEight)

