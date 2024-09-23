# This script is a demonstration of how to use this code to analyze growth curves. 
# Replace the points in question with the correct file names. Note that the files need to be in the correct folder. 

# You can use files with separate keys, but the keys must have the same columns (thought the assignments can be different)


# --- YOUR DATA FILES ----
#  ### EDIT THIS SECTION ###

# If your plates use the same well key, you can include them together, as shown here
plateFiles = c("Data/Michael/PlasmidPhenotyping.csv")
keyFile = ("Data/Michael/plasmidPhenotypingKey.csv")
filePrefix = "plasmidPhenotyping"


#Use this is you have plate files with different keys; see lower in the code
plateFiles2 = c("Data/Demo/demoFileType2.csv")
keyFile2 = c("Data/Demo/demoKey2.csv")


#Edit these to give each plate a unique identifier
plateInstances = c("a")

#edit this to change which colorsets the groups use
scriptColorOrder = c("backgroundBlue", "backgroundRed", "backgroundCyan", "backgroundOrange", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen", "red", "blue")

#Edit this to change the prefix the files are saved with



keydata = read.csv(keyFile)
#write.csv(keydata, keyFile)

#write.csv(keydata, keyFile)


# --- MAIN DATA OUTPUT ---
source("Src/Loc/keyBasedReaderAnalysis.R")
growthDataCleaner(plateFiles, keyFile, instances = plateInstances)


# --- PLOTTING ---

# - pick wells to plot - 
plottedWells = longData$wellNumber[
  longData$Media %in% c( "Ser", "As")   # CHANGE THIS to longData$YourDesiredFilterColumn == "valueMeansYesToPlot"
]   

plottedWells = longData$wellNumber   # Run this code if you want all wells to be plotted instead
# - 

growthCurvePlot = plotGrowthCurve(
  groupingColumn = Media,      # This determines the COLUMN WELLS ARE GROUPED with. Groups share similar colors.
  # Choosing "wellNumber" means each well is in a unique group. Up to 12 groups supported.
  wells = plottedWells,      
  autoGroupLabel = T,       # This determines if the key should only have one entry per group (T), or one entry per well (F) 
  displayAverages = F,      # This determines if an average of all of the samples in the group should be plotted
  useLine = T,
  colorOrder = scriptColorOrder
)
growthCurvePlot

#-- Growthcurver values --

plottedWells = longData$wellNumber[
  longData$Media %in% c( "Ser", "As")   # CHANGE THIS to longData$YourDesiredFilterColumn == "valueMeansYesToPlot"
] 

kPlot = plotGrowthK(xAxisColumn = MediaStrain, groupingColumn = Strain, wells = plottedWells, colorOrder = scriptColorOrder)
kPlot
rPlot = plotGrowthR(xAxisColumn = MediaStrain, groupingColumn = Strain, wells = plottedWells, colorOrder = scriptColorOrder)
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



