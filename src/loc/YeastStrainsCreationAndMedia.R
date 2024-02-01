library(ggplot2)
library(gridExtra)
library(growthcurver)
library(dplyr)


source("Src/Loc/plateReaderAnalysisCore.R")
# -- well name conversion arguments -- 

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
numOfPlates = 2

# -- read the data --
#inData = mainData = read.csv("Data/Old/second2490-3CompareClean.csv")

#inData1 = read.csv("Data/09-22-strainPhenotyping.csv")

inData2 = read.csv("Data/10-12-strainPhenotyping.csv")

inData3 = read.csv("Data/10-13-strainPhenotyping.csv")

inData4 = read.csv("Data/10-19-strainPhenotyping-notOvernight.csv")



#means1 = dataCleaner(inData1, "α")
#mean1Extender = data.frame(matrix(nrow=44, ncol=64))
#colnames(mean1Extender) = colnames(means1)
#means1 = rbind(means1, mean1Extender)

means2 = dataCleaner(inData2, "β", addTime = F)
means3 = dataCleaner(inData3,"γ", addTime = T)
means4 = dataCleaner(inData4, "ε", addTime = F)

#cleanData = means3
#cleanData = cbind(means3, means2, means1)
#numOfPlates = 3
#cleanData = cbind(means3, means2)
#numOfPlates = 2

cleanData = cbind(means3, means2, means4)
numOfPlates = 3


wellNames = names(cleanData)



# -- plot the data -- 
#allWells = names(cleanData)
#combinedPlot(allWells)


{ #collapse this bracket set, it is all for use making older plots 
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
wellsASMmPlot = combinedPlotColor(wellsASMm,numOfPlates)
wellsGluMmPlot = combinedPlotColor(wellsGluMm,numOfPlates)
wellsSerMmPlot = combinedPlotColor(wellsSerMm,numOfPlates)
wellsThrMmPlot = combinedPlotColor(wellsThrMm,numOfPlates)
grid.arrange(wellsASMmPlot, wellsGluMmPlot, wellsSerMmPlot, wellsThrMmPlot, ncol = 2)


wellsASMm5 = wellNames[grepl("As_5", wellNames) & grepl("Mm", wellNames)]
wellsASMm5Plot = combinedPlotColor(wellsASMm5,numOfPlates)
grid.arrange(wellsASMm5Plot, wellsGluMmPlot, wellsSerMmPlot, wellsThrMmPlot, ncol = 2)
grid.arrange(wellsASMmPlot, wellsASMm5Plot, ncol = 2)


combinedPlot(c("Glu_1_Mm_2490","Glu_5_Mm_2490"))
}


# New plot remake 
wellsSerMm = wellNames[grepl("Ser_1", wellNames) & grepl("Mm", wellNames)]
wellsSerMmPlot = combinedPlotColor(wellsSerMm, numberofGroups = 4, groupedNumber = 3, colorOrder = c("backgroundRed", "backgroundBlue", "backgroundRed", "backgroundBlue"))

wellsSerMmControl = wellsSerMm[grepl("2491",wellsSerMm)  | grepl("2493", wellsSerMm)]
wellsSerMmDeletion = wellsSerMm[grepl("2490",wellsSerMm)  | grepl("2492", wellsSerMm)]
serMmControlAverage = rowMeans(cleanData[wellsSerMmControl])
serMmDeletionAverage = rowMeans(cleanData[wellsSerMmDeletion])

cleanData = cbind(cleanData, serMmControlAverage)
cleanData = cbind(cleanData, serMmDeletionAverage)

serWellSet = append(wellsSerMm, c("serMmControlAverage", "serMmDeletionAverage"))

wellsSerMmPlotNew = combinedPlotColor(serWellSet, numberofGroups = 5, groupedNumber = 3, groupRepeatNumber = 2, colorOrder = c("backgroundRed", "backgroundBlue", "backgroundRed", "backgroundBlue", "boldAverages"))
wellsSerMmPlotNew


#

# -- get growthcurver results for the plate -- 
cleanData$time = cleanData$time/60/60

growthcurverOutput = SummarizeGrowthByPlate(cleanData, plot_fit = TRUE, plot_file = "Output/YeastStrains.pdf")
#write.csv(growthcurverOutput, "Output/yeastStrains.csv")



# -- Plot the grwoth rates -- 

growthcurverOutput = read.csv("Output/yeastStrains.csv")

growthRates = growthcurverOutput$r
names(growthRates) = growthcurverOutput$sample


#Add metadata to the growthcurver Data 
{
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
  
  growthcurverOutputPlot$plate = growthcurverOutputPlot$sample
  growthcurverOutputPlot$plate = substr(growthcurverOutputPlot$plate, nchar(growthcurverOutputPlot$plate), nchar(growthcurverOutputPlot$plate))
  
  growthcurverOutputPlot$label = growthcurverOutputPlot$strain
  growthcurverOutputPlot$label = paste(growthcurverOutputPlot$label, growthcurverOutputPlot$plate, sep="")
  
  growthcurverOutputPlot$type = growthcurverOutputPlot$strain
  growthcurverOutputPlot$type[growthcurverOutputPlot$type == 2490 | growthcurverOutputPlot$type ==  2492] = "Deletion"
  growthcurverOutputPlot$type[growthcurverOutputPlot$type == 2491 | growthcurverOutputPlot$type ==  2493] = "Control"
  
  growthcurverOutputPlot$sorter = growthcurverOutputPlot$media
  growthcurverOutputPlot$sorter = paste(growthcurverOutputPlot$sorter, growthcurverOutputPlot$type, sep = "-")
  
  grep("Mm", growthcurverOutputPlot$sample)
  growthcurverOutputPlotTrimmed = growthcurverOutputPlot[grep("Mm", growthcurverOutputPlot$sample),]
  
  growthcurverOutputPlotTrimmed = growthcurverOutputPlotTrimmed[-grep("Ser_5", growthcurverOutputPlotTrimmed$sample),]
  growthcurverOutputPlotTrimmed = growthcurverOutputPlotTrimmed[-grep("Thr_5", growthcurverOutputPlotTrimmed$sample),]
  growthcurverOutputPlotTrimmed = growthcurverOutputPlotTrimmed[-grep("Glu_5", growthcurverOutputPlotTrimmed$sample),]
  growthcurverOutputPlotTrimmed = growthcurverOutputPlotTrimmed[-grep("As_1", growthcurverOutputPlotTrimmed$sample),]
  
  
  growthcurverOutputPlotSerine = growthcurverOutputPlotTrimmed
  growthcurverOutputPlotSerine = growthcurverOutputPlotSerine[-grep("As_5", growthcurverOutputPlotSerine$sample),]
  growthcurverOutputPlotSerine = growthcurverOutputPlotSerine[-grep("Thr_1", growthcurverOutputPlotSerine$sample),]
  
  growthcurverOutputPlotThreonine = growthcurverOutputPlotTrimmed
  growthcurverOutputPlotThreonine = growthcurverOutputPlotThreonine[-grep("As_5", growthcurverOutputPlotThreonine$sample),]
  growthcurverOutputPlotThreonine = growthcurverOutputPlotThreonine[-grep("Ser_1", growthcurverOutputPlotThreonine$sample),]
}



mediaK = ggplot(growthcurverOutputPlotTrimmed, aes(x= media, y= k, colour=label, label=concentration))+
  scale_color_manual(values = colorset)+
  ylim(0, 1)+
  ylab("Carrying Capacity [K]")+
  geom_jitter(width = 0.1, size = 3)
mediaK


mediaR = ggplot(growthcurverOutputPlotTrimmed, aes(x= media, y= r, colour=label))+
  scale_color_manual(values = colorset)+
  ylab("Growth Rate [r]")+
  geom_jitter(width = 0.1, size = 3)
mediaR


grid.arrange(mediaK, mediaR, ncol = 2)


# - 

backgroundBlue = c("cornflowerblue", "cornflowerblue", "cornflowerblue")
backgroundRed = c("lightcoral", "lightcoral", "lightcoral")
colorset = c(backgroundRed, backgroundBlue,  backgroundRed, backgroundBlue)

serineK = ggplot(growthcurverOutputPlotSerine, aes(x= sorter, y= k, colour=label, label=concentration))+
  scale_color_manual(values = colorset)+
  ylim(0, 1)+
  ylab("Carrying Capacity [K]")+
  geom_jitter(width = 0.1, size = 3)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(size=12))
serineK


threonineK = ggplot(growthcurverOutputPlotThreonine, aes(x= sorter, y= k, colour=label, label=concentration))+
  scale_color_manual(values = colorset)+
  ylim(0, 1)+
  ylab("Carrying Capacity [K]")+
  geom_jitter(width = 0.1, size = 3)+
  theme_bw()
threonineK


serR = ggplot(growthcurverOutputPlotSerine, aes(x= sorter, y= r, colour=label))+
  scale_color_manual(values = colorset)+
  ylab("Growth Rate [r]")+
  geom_jitter(width = 0.1, size = 3)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(size=12))
serR


thrR = ggplot(growthcurverOutputPlotThreonine, aes(x= sorter, y= r, colour=label))+
  scale_color_manual(values = colorset)+
  ylab("Growth Rate [r]")+
  geom_jitter(width = 0.1, size = 3)+
  theme_bw()
thrR
