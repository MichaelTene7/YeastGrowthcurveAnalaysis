# This script is a demonstration of how to use this code to analyze growth curves. 
# Replace the points in question with the correct file names. Note that the files need to be in the correct folder. 

# You can use files with separate keys, but the keys must have the same columns (thought the assignments can be different)


# --- YOUR DATA FILES ----
#  ### EDIT THIS SECTION ###

# If your plates use the same well key, you can include them together, as shown here
plateFiles = c("Data/Demo/demoFile1.csv", "Data/Demo/demoFile2.csv", "Data/Demo/demoFile3.csv")
keyFile = ("Data/Demo/demoKey.csv")

#Use this is you have plate files with different keys; see lower in the code
plateFiles2 = c("Data/Demo/demoFileType2.csv")
keyFile2 = c("Data/Demo/demoKey2.csv")


#Edit these to give each plate a unique identifier
plateInstances = c("a", "b", "c")

#edit this to change which colorsets the groups use
scriptColorOrder = c("red", "blue", "orange", "cyan", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen")

scriptColorOrder = c("red", "tan", "green", "purple", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen")


#Edit this to change the prefix the files are saved with
filePrefix = "ThesisFigure"





# --- MAIN DATA OUTPUT ---
source("Src/Loc/keyBasedReaderAnalysis.R")
growthDataCleaner(plateFiles, keyFile, instances = plateInstances)


# --- PLOTTING ---

# - pick wells to plot - 
plottedWells = longData$wellNumber[
  longData$DemoPlotColumn == "yes"&   # CHANGE THIS to longData$YourDesiredFilterColumn == "valueMeansYesToPlot"
  longData$Nutrient == "Mm" &  # CHANGE THIS to longData$YourDesiredFilterColumn == "valueMeansYesToPlot"
  longData$Media != "Thr"&
  longData$Mutant != "Control"
  ]                                 
plottedWells = longData$wellNumber   # Run this code if you want all wells to be plotted instead

plottedWells = longData$wellNumber[
    longData$Nutrient == "Csm" &   # CHANGE THIS to longData$YourDesiredFilterColumn == "valueMeansYesToPlot"
    longData$Media == "Thr"&
    longData$DemoPlotColumn == "yes" &
    longData$Mutant != "Control"
]  

#oldLongData = longData
longData = longData[longData$wellNumber %in% plottedWells,]
longData = longData[!longData$wellNumber == "A5a",]

plottedWells = longData$wellNumber[
    longData$Media != "Thr"
]  

longData$Phenotype = longData$Media
longData$Phenotype[longData$Phenotype == "As"] = "HerbivoreOld"
longData$Phenotype[longData$Phenotype == "Glu"] = "Carnivore"
longData$Phenotype[longData$Phenotype == "Ser"] = "Deletion"

#longData2 = longData

longData = oldLongData

data3 = longData[longData$wellNumber %in% plottedWells,]
data3$Phenotype = data3$Media
data3$Phenotype = "Demo"

longData3 = rbind (longData2, data3)

longData = longData3
longData = longData2
plottedWells = longData$wellNumber[

]  

longData = longData[order(longData$time),]

longData$Phenotype[longData$Phenotype == "Herbivore"] = "HerbivoreOld"
longData$Phenotype[longData$Phenotype == "Demo"] = "Herbivore"

plottedWells = longData$wellNumber[
  longData$Phenotype != "HerbivoreOld"
]  

longData = longData[longData$wellNumber %in% plottedWells,]

#write.csv(longData, "Output/Michael/ThesisFigureLongData.csv")

longData$wellName = longData$wellNumber
longData$wellNumber

longData$wellNumber = longData$wellName

dput(unique(longData$wellNumber))

replaceKey = c("A3a" = "  Jaguar", "C3a" = "  Dolphin", "C5a" = "Deletion-1", "A3b" = "  Fisherman Bat", "A5b" = "Deletion-2", "C3b" = "  Polar Bear", "C5b" = "Deletion-3", "A3c" = "  Hyena", "A5c" = "Deletion-4", 
               "C3c" = "  Orca", "C5c" = "Deletion-5", "E7a" = " Horse", "G7a" = " Fruit Bat", "E7b" = " Giraffe", "G7b" = " Deer Mouse", "E7c" = " Cow", "G7c" = " Antelope")
for (i in 1:length(longData$wellNumber)) {
  if (longData$wellNumber[i] %in% names(replaceKey)) {
    longData$wellNumber[i] <- replaceKey[[longData$wellNumber[i]]]
  }
}

# - 

growthCurvePlot = plotGrowthCurve(
          groupingColumn = Phenotype,      # This determines the COLUMN WELLS ARE GROUPED with. Groups share similar colors.
                                     # Choosing "wellNumber" means each well is in a unique group. Up to 12 groups supported.
           wells = plottedWells,      
           autoGroupLabel = F,       # This determines if the key should only have one entry per group (T), or one entry per well (F) 
           displayAverages = F,      # This determines if an average of all of the samples in the group should be plotted
           colorOrder = scriptColorOrder
           )
growthCurvePlot

# -- Growthcurver values --

kPlot = plotGrowthK(xAxisColumn = Media, groupingColumn = Strain, wells = plottedWells, colorOrder = scriptColorOrder)
kPlot
rPlot = plotGrowthR(xAxisColumn = Media, groupingColumn = Strain, wells = plottedWells, colorOrder = scriptColorOrder)
rPlot 




# -- saving to files --

# make the  Output Directory 
if(!dir.exists("Output")){dir.create("Output")}
outputFolderNameNoSlash = paste("Output/",filePrefix, sep = "") 
if(!dir.exists(outputFolderNameNoSlash)){dir.create(outputFolderNameNoSlash)}
outputFolderName = paste("Output/",filePrefix,"/", sep = "")


# - Growthcurver Numbers -
write.csv(growthCurverOutput, paste(outputFolderName, filePrefix, "growthCurverOutput.csv"))

# - Graphs -
png(paste(outputFolderName, filePrefix, "growthCurve.png"))
growthCurvePlot
dev.off()

png(paste(outputFolderName, filePrefix, "Kplot.png"))
kPlot
dev.off()

png(paste(outputFolderName, filePrefix, "Rplot.png"))
rPlot
dev.off()


## ------------ DEMO FOR IF MULITPLE KEYS ARE REQUIRED -------------------- ##

longData1 = growthDataCleaner(plateFiles, keyFile, instances = plateInstances)

longData2 = growthDataCleaner(plateFiles2, keyFile2, 
                              instances = "d",        #CHANGE THIS TO MATCH YOUR DESIRED INSTANCE
                              addTime = T)

# Add as many "longData"s are required for you number of keys, and then make sure to add them to the bind function below. 

longData = rbind(longData1, longData2) %>% arrange(time)
colNamesSet = names(longData[3:(length((names(longData)))-2)])

shortData = longToShortData(longData)
growthCurverOutput = runGrowthCurverWithMetadata(shortData, colNamesSet)



