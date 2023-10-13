library(ggplot2)
library(gridExtra)
library(growthcurver)

# -- read the data --
inData = mainData = read.csv("Data/09-20-strainPhenotyping.csv")
inData = mainData = read.csv("Data/09-22-strainPhenotyping.csv")
inData = mainData = read.csv("Data/10-12-strainPhenotyping.csv")
inData = mainData = read.csv("Data/10-13-strainPhenotyping.csv")

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
  colorset = c("brown1", "blue", "chocolate1", "cadetblue")
  plot <- ggplot( aes(x=time), data = cleanData)
  for (i in 1:length(wells)) { 
    loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="")
    plot <- plot + eval(parse(text=loop_input))  
  }
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot = plot + scale_color_manual(values = colorset)
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

wellsASMm = wellNames[grepl("As_1", wellNames) & grepl("Mm", wellNames)]
wellsGluMm = wellNames[grepl("Glu_1", wellNames) & grepl("Mm", wellNames)]
wellsSerMm = wellNames[grepl("Ser_1", wellNames) & grepl("Mm", wellNames)]
wellsThrMm = wellNames[grepl("Thr_1", wellNames) & grepl("Mm", wellNames)]
wellsASMmPLot = combinedPlot(wellsASMm)
wellsGluMmPlot = combinedPlot(wellsGluMm)
wellsSerMmPlot = combinedPlot(wellsSerMm)
wellsThrMmPlot = combinedPlot(wellsThrMm)
grid.arrange(wellsASMmPLot, wellsGluMmPlot, wellsSerMmPlot, wellsThrMmPlot, ncol = 2)


combinedPlot(c("Glu_1_Mm_2490","Glu_5_Mm_2490"))


#

# -- get growthcurver results for the plate -- 
cleanData$time = cleanData$time/60/60

growthcurverOutput = SummarizeGrowthByPlate(cleanData, plot_fit = TRUE, plot_file = "Output/GrowthCurves/YeastStrains.pdf")
write.csv(growthcurverOutput, "Output/GrowthCurves/yeastStrains.csv")



# -- Plot the grwoth rates -- 

growthcurverOutput = read.csv("Output/GrowthCurves/yeastStrains.csv")

growthRates = growthcurverOutput$r
names(growthRates) = growthcurverOutput$sample

growthcurverOutputPlot = growthcurverOutput
growthcurverOutputPlot$r[growthcurverOutputPlot$r >1] =NA
growthcurverOutputPlot$strain = growthcurverOutputPlot$sample

growthcurverOutputPlot$strain[grep("2490", growthcurverOutputPlot$sample)] = "2490"
growthcurverOutputPlot$strain[grep("2491", growthcurverOutputPlot$sample)] = "2491"
growthcurverOutputPlot$strain[grep("2492", growthcurverOutputPlot$sample)] = "2492"
growthcurverOutputPlot$strain[grep("2493", growthcurverOutputPlot$sample)] = "2493"
growthcurverOutputPlot$media = growthcurverOutputPlot$sample
growthcurverOutputPlot$media[grep("As", growthcurverOutputPlot$sample)] = "As"
growthcurverOutputPlot$media[grep("Glu", growthcurverOutputPlot$sample)] = "Glu"
growthcurverOutputPlot$media[grep("Ser", growthcurverOutputPlot$sample)] = "Ser"
growthcurverOutputPlot$media[grep("Thr", growthcurverOutputPlot$sample)] = "Thr"

growthcurverOutputPlot$concentration = growthcurverOutputPlot$sample
growthcurverOutputPlot$concentration[grep("_5", growthcurverOutputPlot$sample)] = "5%"
growthcurverOutputPlot$concentration[grep("_1", growthcurverOutputPlot$sample)] = "1%"


grep("Mm", growthcurverOutputPlot$sample)
growthcurverOutputPlotTrimmed = growthcurverOutputPlot[grep("Mm", growthcurverOutputPlot$sample),]

growthcurverOutputPlotTrimmed = growthcurverOutputPlotTrimmed[-grep("Ser_5", growthcurverOutputPlotTrimmed$sample),]
growthcurverOutputPlotTrimmed = growthcurverOutputPlotTrimmed[-grep("Thr_5", growthcurverOutputPlotTrimmed$sample),]
growthcurverOutputPlotTrimmed = growthcurverOutputPlotTrimmed[-grep("Glu_5", growthcurverOutputPlotTrimmed$sample),]
growthcurverOutputPlotTrimmed = growthcurverOutputPlotTrimmed[-grep("AS_1", growthcurverOutputPlotTrimmed$sample),]


ggplot(growthcurverOutputPlotTrimmed, aes(x= strain, y= r, colour=strain, label=concentration))+
  geom_point()+
  geom_text(hjust=0, vjust=0)
# 
ggplot(growthcurverOutputPlotTrimmed, aes(x= strain, y= k, colour=media, label=concentration))+
  geom_point()+
  geom_text(hjust=0, vjust=0)

#

colorset = c("brown1", "blue", "chocolate1", "cadetblue")

mediaK = ggplot(growthcurverOutputPlotTrimmed, aes(x= media, y= k, colour=strain, label=concentration))+
  geom_point()+
  geom_text(hjust=0, vjust=0)+
  scale_color_manual(values = colorset)

mediaR = ggplot(growthcurverOutputPlotTrimmed, aes(x= media, y= r, colour=strain, label=concentration))+
  geom_point()+
  geom_text(hjust=0, vjust=0)+
  scale_color_manual(values = colorset)

grid.arrange(mediaK, mediaR, ncol = 2)










