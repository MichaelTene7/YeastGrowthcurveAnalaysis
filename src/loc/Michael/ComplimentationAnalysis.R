# This script is a demonstration of how to use this code to analyze growth curves. 
# Replace the points in question with the correct file names. Note that the files need to be in the correct folder. 

# You can use files with separate keys, but the keys must have the same columns (thought the assignments can be different)


# --- YOUR DATA FILES ----
#  ### EDIT THIS SECTION ###

# If your plates use the same well key, you can include them together, as shown here
plateFiles = c("Data/Michael/10-12-strainPhenotyping.csv", "Data/Michael/10-13-strainPhenotyping.csv", "Data/Michael/10-19-strainPhenotyping-notOvernight.csv")
keyFile = ("Data/Michael/strainPhenotypingKey.csv")
filePrefix = "strainsPhenotyping"

plateFiles = c("Data/11-2-completemetation.csv")
keyFile = ("Data/Michael/ComplimentationKey.csv")
filePrefix = "Complimentation"

plateFiles = c("Data/Michael/YeastComplimentation5-23.csv")
keyFile = ("Data/Michael/ComplementationRound2Key3.csv")
filePrefix = "ReComplimentation"

plateFiles = c("Data/Michael/5-30UndergradComplimentation.csv")
keyFile = ("Data/Michael/ComplementationRound2Key3.csv")
filePrefix = "UndergradComplimentation"



#Use this is you have plate files with different keys; see lower in the code
plateFiles2 = c("Data/Demo/demoFileType2.csv")
keyFile2 = c("Data/Demo/demoKey2.csv")


#Edit these to give each plate a unique identifier
plateInstances = c("a", "b", "c")
plateInstances = c("a")

#edit this to change which colorsets the groups use
scriptColorOrder = c("backgroundBlue", "backgroundRed", "backgroundCyan", "backgroundOrange", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen")

scriptColorOrder = c("backgroundCyan", "backgroundBlue", "backgroundRed", "backgroundGreen", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen")


#Edit this to change the prefix the files are saved with



keydata = read.csv(keyFile)
#keydata = keydata %>% unite("MediaMutation", c(Media,Mutant), remove = F, sep="-")
#write.csv(keydata, keyFile)
keydata = keydata %>% unite("MutationPlasmid", c(Mutant,Plasmid), remove = F, sep="-")

#write.csv(keydata, keyFile)


# --- MAIN DATA OUTPUT ---
source("Src/Loc/keyBasedReaderAnalysis.R")
growthDataCleaner(plateFiles, keyFile, instances = plateInstances)


# --- PLOTTING ---

# - pick wells to plot - 
plottedWells = longData$wellNumber[
  longData$Media %in% c( "Glu")   # CHANGE THIS to longData$YourDesiredFilterColumn == "valueMeansYesToPlot"
]   

plottedWells = longData$wellNumber   # Run this code if you want all wells to be plotted instead
# - 

growthCurvePlot = plotGrowthCurve(
  groupingColumn = MutationPlasmid,      # This determines the COLUMN WELLS ARE GROUPED with. Groups share similar colors.
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
  longData$Media %in% c( "Ser", "Glu")   # CHANGE THIS to longData$YourDesiredFilterColumn == "valueMeansYesToPlot"
] 

kPlot = plotGrowthK(xAxisColumn = Media, groupingColumn = MutationPlasmid, wells = plottedWells, colorOrder = scriptColorOrder)
kPlot
rPlot = plotGrowthR(xAxisColumn = Media, groupingColumn = MutationPlasmid, wells = plottedWells, colorOrder = scriptColorOrder)
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



