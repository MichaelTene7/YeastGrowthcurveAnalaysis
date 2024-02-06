# ----- Making a script to decode from a key csv 
inputData = inData3


# weirdness with getting the column names to sync properly -- in particular with the moving instance column
# finish code, set up plot
# make sure to check the payments on wellsfargo 

{
inputKey = "Data/DemoKey.csv"
plateFilesSingle = c("Data/10-12-strainPhenotyping.csv")
plateFilesMulti = c("Data/10-12-strainPhenotyping.csv", "Data/10-13-strainPhenotyping.csv", "Data/10-19-strainPhenotyping-notOvernight.csv")
addTime = T
instances = NULL
longDataOut = "longData"
shortDataOut = "shortData"
growthCurverDataOut = "growthCurverOutput"
keyFile = inputKey
}
plateFiles = plateFilesSingle


growthDataCleaner(plateFilesMulti, inputKey)
growthDataCleaner(plateFilesSingle, inputKey, instances = "a")
{
#Param addTime is most deprecated due to new handling of muliptle plate files
growthDataCleaner = function(plateFiles, keyFile, addTime = T, instances = NULL, longDataOut = "longData", shortDataOut = "shortData", growthCurverDataOut = "growthCurverOutput"){
  # -- Step 0 prepare to process multiple input files
  if(is.null(instances) & length(plateFiles) ==1 ){
    instanced = F
  }else{
    instanced = T
  }
  if(length(plateFiles) > 1){
    if(is.null(instances)){
      instances = letters[1:length(plateFiles)]
      message(paste("Instances set to", paste(instances, collapse = ",")))
    }else{
      if(length(instances) != length(plateFiles)){
        stop("The instance string must have the same length as the number of plates")
      }
      if(!all(nchar(instances) == nchar(instances)[1])){
        stop("The instance value must all be of the same length")
      }
    }
  }
  
  # -- Step 1 get means from data --
  plateData = read.csv(plateFiles[1])
  keyData = read.csv(keyFile)
  
  if(length(plateFiles) > 1){
    inData1 = meanExtractor(plateData, addTime = T, instance = instances[1])
    meanData = inData1
    for(i in 2:length(plateFiles)){
      plateData = read.csv(plateFiles[i])
      inDataNext = meanExtractor(plateData, addTime = F, instance = instances[i])
      meanData = cbind(meanData, inDataNext)
    }
  }else{
    meanData = meanExtractor(plateData, addTime = addTime, instance = instances[1])
  }
  
  # --- Step 2 create long data using the key ---
  longData = meanToLongData(meanData, keyData, instanced = instanced)

  assign(longDataOut, value = longData, envir = .GlobalEnv)

  # - Make a version of the well conversions independent of the tibble to attach to the short data later - 
  colNamesSet = names(longData[3:(length((names(longData)))-2)])
  
  assign("colNamesSet", colNamesSet, envir = .GlobalEnv)
  
  # --- Step 3 make a short version of the data --- 
  shortData = longToShortData(longData)

  assign(shortDataOut, shortData, envir = .GlobalEnv)
  
  # --- Step 4 run growthCurver ---
  
  growthCurverOutput = runGrowthCurverWithMetadata(shortData, colNamesSet)
  assign(growthCurverDataOut, growthCurverOutput, envir = .GlobalEnv)

}

# outputs: Data in short format (Wellnumber, data separated by _); long format 

# Things to do after: 
 # Growthcurver plots
 # K and R plots
 # 

meanExtractor = function(mainData, instance = NULL, addTime = T){
  # -- clean the data -- #
  
  #Isolate the time data elsewhere so that it can be added in later 
  timeData = mainData[which(mainData[1] == "Time [s]"),]                        #Select the time columns
  timeData = timeData[1,]                                                       #They're all identical so we only need the first one
  rownames(timeData)= "time"                                                    #Move the 'time' label to the rowname 
  timeData = timeData[,-1]                                                      #Remove the row with 'time' in text 
  # -- 
  
  # Get the means of all of the observation
  meanData = mainData[which(mainData[1] == "Mean"),]                            #The data in in the row titled mean 
  rownames(meanData) = mainData[(which(mainData[1] == "Mean")-3),1]             #The well name of the data is always stored three rows above the mean data 
  meanData = meanData[,-1]                                                      #Remove the row with the 'mean' label as text 
  rownames(meanData) = sapply(rownames(meanData) , paste, instance, sep="")     #If there is an Instance marker added (to give different plates different row names), add it to the row name
  
  if(addTime){                                                                  #If adding a time row, add the row
    cleanData = rbind(timeData, meanData)
  }else{                                                                        #Else do nothing 
    cleanData = meanData
  }
  cleanData = data.frame(t(cleanData))                                          #Rotate the data 90 degrees 
  cleanData = lapply(FUN = as.numeric, cleanData)                               #Convert all values to numeric
  cleanData= data.frame(cleanData)                                              #Save as dataframe
  
  cleanData                                                                     #Return the cleaned data 
}

meanToLongData = function(meanData, keyData, instanced = F){
  
  # -- convert the data to long format --
  longData = pivot_longer(meanData, 2:ncol(meanData))
  names(longData)[2] = "wellNumber"
  longData = longData %>% relocate(value, .before = wellNumber)
  
  # - use the well number to connect the key and the long data - 
  for(i in 2:ncol(keyData)){
    currentCol = ncol(longData)+1
    longData[currentCol] = rep(NA)
    colnames(longData)[currentCol] = names(keyData[i])
    
    for(j in 1:nrow(longData[currentCol])){
      keyRow = match(substr(longData$wellNumber[j], 1, 2), keyData$Well)
      
      longData[j, currentCol] = keyData[keyRow, i]
    }
  }
  # - add rows and columns to the long data - 
  longData$wellNumber2 = longData$wellNumber
  if(instanced == F){
  longData = separate_wider_position(longData, wellNumber2, c(row = 1, column = 1))
  }else{
    longData = separate_wider_position(longData, wellNumber2, c(row = 1, column = 1, instance = max(nchar(longData$wellNumber2))-2))
    longData = relocate(longData, instance, .after = wellNumber)
  }
  
  longData
}

longToShortData = function(longData){
  shortData = longData
  if("instance" %in% names(shortData)){
    shortData = shortData %>% unite("wellNumber", c(wellNumber, instance))
  }
  if("row" %in% names(shortData)){
    shortData = shortData %>% select(-row)
  }
  if("column" %in% names(shortData)){
    shortData = shortData %>% select(-column)
  }
  shortData = shortData %>% unite("wellName", c(3:ncol(shortData)))
  
  shortData = pivot_wider(shortData, values_from = value, names_from = wellName)
  
}

runGrowthCurverWithMetadata = function(shortData, colNames = colNamesSet){
  
  # -run growthCurver -
  
  growthcurverOutput = SummarizeGrowthByPlate(shortData)
  
  # - Re-add metadata to growthCurver output -
  
  growthcurverOutput$fullWellCode = growthcurverOutput$sample
  growthcurverOutput$fullWellCode2 = growthcurverOutput$sample
  growthcurverOutput = growthcurverOutput %>% separate_wider_delim(fullWellCode2, "_", names = colNames)
  
  if("instance" %in% names(growthcurverOutput)){
    growthcurverOutput$sample = paste(growthcurverOutput$wellNumber, growthcurverOutput$instance)
  }else{
    growthcurverOutput$sample = growthcurverOutput$wellNumber
  }
  
  growthcurverOutput

}

}


growthKPlot = function(sorter, groupColumn, growthCurverData = growthCurverOutput, colorSet = masterColorSet){
  kPlot = ggplot(growthCurverData, aes(x= {{sorter}}, y= k, colour=wellNumber))+
    scale_color_manual(values = colorSet)+
    ylim(0, 1)+
    ylab("Carrying Capacity [K]")+
    geom_jitter(width = 0.1, size = 3)+
    theme_bw()+
    theme(axis.title.x = element_blank())+
    theme(axis.text.x = element_text(size=12))
  kPlot
}



{
  inputKey = "Data/DemoKey.csv"
  plateFilesSingle = c("Data/10-12-strainPhenotyping.csv")
  plateFilesMulti = c("Data/10-12-strainPhenotyping.csv", "Data/10-13-strainPhenotyping.csv", "Data/10-19-strainPhenotyping-notOvernight.csv")
  addTime = T
  instances = NULL
  longDataOut = "longData"
  shortDataOut = "shortData"
  growthCurverDataOut = "growthCurverOutput"
  keyFile = inputKey
}
plateFiles = plateFilesSingle


growthDataCleaner(plateFilesMulti, inputKey)
growthDataCleaner(plateFilesSingle, inputKey, instances = "a")

longDataMulit = longData



# ------------C ode from working on the dynamic ploitting function s

if(!is.null(wells)){                                                          #If wells isn't empty, limit the dataset to the specified wells
  dataSet = dataSet[which(dataSet$wellNumber %in% wells),]
}

# - Add group information columns to the data - 
dataSet = dataSet %>% mutate(groupColumn = {{groupingColumn}})                # Make a column with a static name that has the grouping data
dataSet = dataSet %>% mutate(groupValue = as.numeric(as.factor(groupColumn))) # Make a column with the row's group number                
dataSet = dataSet %>% group_by(groupColumn) %>% mutate(groupInstance = row_number()) # Make a column that show what instance of its group the row is 


useDynamicGroupColumn = function(dataSet, groupingColumn, wells){
  if(!is.null(wells)){                                                          #If wells isn't empty, limit the dataset to the specified wells
    dataSet = dataSet[which(dataSet$wellNumber %in% wells),]
  }
  
  # - Add group information columns to the data - 
  dataSet = dataSet %>% mutate(groupColumn = {{groupingColumn}})                # Make a column with a static name that has the grouping data
  dataSet = dataSet %>% mutate(groupValue = as.numeric(as.factor(groupColumn))) # Make a column with the row's group number                
  dataSet = dataSet %>% group_by(groupColumn) %>% mutate(groupInstance = row_number()) # Make a column that show what instance of its group the row is 
  numberofGroups = nrow(dataSet %>% count(groupColumn))                         # Count how many different values there are in the grouping column
  uniqueSamples = unique(dataSet$wellNumber)                                    # Get a list of the unique samples
  
}

autoLegend = function(plot, data = dataSet, labelSetVal = labelSet, autoGroupLabelVal = autoGroupLabel, legendTitleVal = legendTitle, numGroups = numberofGroups){
  if(!is.null(legendTitleVal)){
    plot = plot +  guides(color = guide_legend(title = legendTitleVal))
  }
  
  if(!is.null(labelSetVal) | autoGroupLabelVal){
    startOfEachGroup = NULL
    for(i in 1:(numGroups)){
      groupstart = which(data$groupValue == i)[1]
      startOfEachGroup = append(startOfEachGroup, groupstart)
    }
    print(startOfEachGroup)
    
    if(autoGroupLabelVal){
      labelSetVal = NULL
      for(i in 1:numGroups){
        labelSetVal[i] = data$groupColumn[startOfEachGroup[i]]
      }
    }
    
    plot = plot + scale_color_manual(values = masterColorSet, 
                                     breaks = data$wellNumber[startOfEachGroup],
                                     labels = labelSetVal
    ) 
  }
  plot
}






#-------------------------------------------------------------------------------------------------------------------
#Code from making the fancy long function 

testFunction = function(wells, groupingColumn = NULL, numberofGroups = NULL, groupedNumber =3, groupsRepeatTimes = 1, colorOrder = c("red", "blue", "orange", "cyan", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen"), labelSet = NULL, legendTitle = NULL, colorListHub = colorListsHub, dataSet = longData){
  #This is a set of colors, eachof which has four versions of it 
  
  numberofGroups = nrow(dataSet %>% count( {{groupingColumn}} ))
  dataSet = dataSet %>% mutate(groupColumn = {{groupingColumn}})                 # Make a column with a static name that has the grouping data
  
  dataSet = dataSet %>% mutate(groupValue = as.numeric(as.factor(groupColumn)))                
  dataSet = dataSet %>% group_by(groupValue) %>% mutate(groupInstance = row_number())
  dataSet
}
testOut = testFunction(wells, groupingColumn = strain, groupedNumber = 8, labelSet = c("A", "B", "C", "D", "E", "F", "G", "H"))



testOut2 = testOut[which(testOut$wellName %in% wellsSerMm),]
testOut2 = testOut2 %>% group_by(time, groupColumn) %>%  mutate(groupAverage = mean(value))
testOut2 = testOut2 %>% left_join(outAverages, by = "groupColumn") %>% select(-value) %>% distinct()



cleanData = dataCleanerLong(inData3)
wells = names(cleanData)


groupColumn = strain
plotGrowth(wells, groupingColumn = column, groupedNumber = 8, labelSet = c("A", "B", "C", "D", "E", "F", "G", "H"))
plotGrowth(wells, groupingColumn = column, autoGroupLabel = T)
plotGrowth(wells, groupingColumn = strain, autoGroupLabel = T)


testWells = wellNames
wellsSerMm = wells[grepl("Ser", wells) & grepl("Mm", wells)]


combinedPlotColorLongFancy(wellsSerMm, groupingColumn = strain, autoGroupLabel = T)
combinedPlotColorLongFancy(wellsSerMm, groupingColumn = strain, autoGroupLabel = T, aver)

plotGrowth(wellsSerMm, data= longData, groupingColumn = strain, autoGroupLabel = T, displayAverages = T, colorOrder = c("backgroundRed", "backgroundBlue", "backgroundOrange", "backgroundCyan"))
plotGrowth(wellsSerMm, data= longData, groupingColumn = strain, autoGroupLabel = T, displayAverages = T, colorOrder = c("backgroundRed", "backgroundBlue", "backgroundOrange", "backgroundCyan"))


colorstart = which(testOut$groupValue == i)[1]

currentGroupValue = testOut$groupValue[i]
currentGroupInstance = testOut$groupInstance[i]

currentColorSet = colorListHub[[currentGroupValue]]
currentColor = currentColorSet[[currentGroupInstance]]
masterColorSet = append(masterColorSet, currentColor)


wells = unique(longData$wellName)

combinedPlotColorLong(wells, numberofGroups = 8, groupedNumber = 8, labelSet = c("A", "B", "C", "D", "E", "F", "G", "H"))
combinedPlotColorLong(wells, numberofGroups = 8, groupedNumber = 8)



#-------------------------------------------------------------------------------------------------------------------
# Old colorset Code
# ---- Old Code ---- 
colorset1 = c("brown1", "blue", "chocolate1", "deepskyblue")
colorset2 = c("brown3", "blue3", "chocolate3", "deepskyblue3")
colorset3 = c("brown4", "blue4", "chocolate4", "deepskyblue4")
colorset4 = c("brown2", "blue2", "chocolate2", "deepskyblue2")

colorset1 = c("brown1", "brown3", "brown4", "brown2")
colorset2 = c("blue", "blue3", "blue4", "blue2")
colorset3 = c("chocolate1", "chocolate3", "chocolate4", "chocolate2")
colorset4 = c("deepskyblue", "deepskyblue3", "deepskyblue4", "deepskyblue2")
colorset5 = c("hotpink", "hotpink1", "hotpink2", "hotpink3")

colorsets = data.frame(colorset1, colorset2, colorset3, colorset4)
colorset = unlist(colorsets[1:numOfPlates,])
names(colorset) =NULL

# Old colorset code 
#colorsetNames = c("colorset1", "colorset2", "colorset3", "colorset4","colorset5","colorset6","colorset7")     #These are placeholder colorsetnames, they are used to reference the colorsets regardless of what colors they actually hold

#colorsets = NULL
#for(i in 1:groupRepeatNumber){
#  rowSet = ((groupedNumber *(i-1))+1):(groupedNumber * i)
#  colorsets = cbind(colorsets, colorset1[rowSet], colorset2[rowSet], colorset3[rowSet], colorset4[rowSet], colorset5[rowSet]) #This is not dynamic even thoguh is should be. This is an open flaw in this funciton. 
#}
#colorsets = data.frame(colorsets)
#colorset = unlist(colorsets) 
#for(i in 1:colorsetNumber){                                                   #For each of the number of colors requested 
#  assign(colorsetNames[i], eval(parse( text = colorOrder[i])))                #Set the colorsset to the color found in colorOrder
#}








str(wellsSerMmPlot)

# -----------------------------------------------------------------------------------------------------------------
# --- Generate expected Results Graph ----

wellsSerMm = wellNames[grepl("Ser_1", wellNames) & grepl("Mm", wellNames)]
wellsSerMm = append(wellsSerMm, "time")
dummyData = cleanData[names(cleanData) %in% wellsSerMm]
wellsSerMm = wellNames[grepl("Ser_1", wellNames) & grepl("Mm", wellNames)]


combinedPlotColor2 = function(wells, colorsetNumber =3){
  colorset1 = c("brown1", "blue", "chocolate1", "deepskyblue")
  colorset2 = c("brown3", "blue3", "chocolate3", "deepskyblue3")
  colorset3 = c("brown4", "blue4", "chocolate4", "deepskyblue4")
  colorset4 = c("brown2", "blue2", "chocolate2", "deepskyblue2")
  
  colorset1 = c("brown1", "brown3", "brown4", "brown2")
  colorset2 = c("blue", "blue3", "blue4", "blue2")
  colorset3 = c("chocolate1", "chocolate3", "chocolate4", "chocolate2")
  colorset4 = c("deepskyblue", "deepskyblue3", "deepskyblue4", "deepskyblue2")
  
  colorsets = data.frame(colorset1, colorset2, colorset3, colorset4)
  colorset = unlist(colorsets[1:colorsetNumber,])
  colorset = c( "chocolate1", "chocolate3", "purple","darkgreen", "green4")
  names(colorset) =NULL
  plot <- ggplot( aes(x=time), data = dummyData)
  for (i in 1:length(wells)) { 
    loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="")
    plot <- plot + eval(parse(text=loop_input))  
  }
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot = plot + scale_color_manual(values = colorset)
  plot = plot + ylim(0,1)
  plot
}

dummyData = dummyData[-6]
dummyData = dummyData[-2]
dummyData = dummyData[-6]
names(dummyData) = c("time", "Herbivore2", "CHA1_Delete", "Carnivore2", "Herbivore1", "Carnivore1")
plotWells = names(dummyData)

plotWells = plotWells[-1]

wellsSerMmPlot = combinedPlotColor2(plotWells,numOfPlates)
wellsSerMmPlot

#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
