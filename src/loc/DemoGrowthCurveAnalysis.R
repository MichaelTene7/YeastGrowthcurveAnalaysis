# This script is a demonstration of how to use this code to analyze growth curves. 
# Replace the points in question with the correct file names. Note that the files need to be in the correct folder. 

# You can use files with separate keys, but the keys must have the same columns (thought the assignments can be different)


# --- YOUR DATA FILES ----
# EDIT THIS SECTION

# If your plates use the same well key, you can include them together, as shown here
plateFiles = c("Data/demoFile1.csv", "Data/demoFile2.csv", "Data/demoFile3.csv")
keyFile = ("Data/demoKey.csv")

#Use this is you have plate files with different keys; see lower in the code
plateFiles2 = c("Data/demoFileType2.csv")
keyFile2 = c("Data/demoKey2.csv")


#Edit these to give each plate a unique identifier
plateInstances = c("a", "b", "c")

#edit 


# --- MAIN DATA OUTPUT ---

growthDataCleaner(plateFiles, keyFile, instances = plateInstances)


# --- PLOTTING ---
plottedWells = longData$wellNumber[
  longData$DemoPlotColumn == "yes"   # CHANGE THIS to longData$YourDesiredFilterColumn == "valueMeansYesToPlot"
  ]                                 

plottedWells = longData$wellNumber   # Run this code if you want all wells to be plotted instead

#edit this to change which colorsets the groups use
scriptColorOrder = c("red", "blue", "orange", "cyan", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen")





plotGrowth(groupingColumn = row,     # This determines the COLUMN WELLS ARE GROUPED with. Groups share similar colors.
                                     # Choosing "wellNumber" means each well is in a unique group. Up to 12 groups supported.
           wells = plottedWells,      
           autoGroupLabel = T,       # This determines if the key should only have one entry per group (T), or one entry per well (F) 
           displayAverages = F,      # This determines if an average of all of the samples in the group should be plotted
           colorOrder = scriptColorOrder
           )

# -- Plot growthcurver values --

growthKPlot(instance, Tetrad)

