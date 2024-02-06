library(ggplot2)
library(gridExtra)
library(growthcurver)
library(dplyr)
library(tidyr)

# -------- Data cleaning Functions -------

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
    
    
    longData
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
    
    #longData = longData %>% unite(wellName, c(3:ncol(longData)), remove = F)
    
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
      growthcurverOutput$sample = paste(growthcurverOutput$wellNumber)
    }else{
      growthcurverOutput$sample = growthcurverOutput$wellNumber
    }
    
    growthcurverOutput
    
  }
  
}



# ------ Plotting functions -------

source("Src/Reu/colorLists.R")
defaultColorOrder = c("red", "blue", "orange", "cyan", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen")


plotGrowthCurve = function(groupingColumn, wells = NULL, dataSet = longData, autoGroupLabel = F, useLine = F, displayAverages = F, labelSet = NULL, legendTitle = NULL, colorListHub = colorListsHub,
                      colorOrder = defaultColorOrder){
  
  
  if(!is.null(wells)){                                                          #If wells isn't empty, limit the dataset to the specified wells
    dataSet = dataSet[which(dataSet$wellNumber %in% wells),]
  }
  # - Add group information columns to the data - 
  dataSet = dataSet %>% mutate(groupColumn = {{groupingColumn}})                # Make a column with a static name that has the grouping data
  dataSet = dataSet %>% mutate(groupValue = as.numeric(as.factor(groupColumn))) # Make a column with the row's group number                
  dataSet = dataSet %>% group_by(groupColumn) %>% mutate(groupInstance = row_number()) # Make a column that show what instance of its group the row is 
  
  
  numberofGroups = nrow(dataSet %>% count(groupColumn))                         # Count how many different values there are in the grouping column
  uniqueSamples = unique(dataSet$wellNumber)                                    # Get a list of the unique samples
  
  masterColorSet = buildColorList({{groupingColumn}}, wells, dataSet, autoGroupLabel, displayAverages, labelSet, colorListHub, colorOrder)
  
  # - plot the graph - 
  plot <- ggplot( aes(x=time, y=value, color=wellNumber), data = dataSet) 
  if(useLine){ plot = plot +geom_line()}else{plot = plot +geom_point()}
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot = plot + scale_color_manual(values = masterColorSet)
  plot = plot + ylim(0,1) + ylab("Absorbance") +xlab("Time [s]") +theme_bw()
  
  if(displayAverages){
    if(useLine){plot = plot + geom_line(aes(y = groupAverage, color = groupValue))
    }else{ plot = plot + geom_point(aes(y = groupAverage, color = groupColumn))}
  }
  
  plot = autoLegend(plot, data = dataSet, autoGroupLabelVal = autoGroupLabel, labelSetVal = labelSet, legendTitleVal = legendTitle, numGroups = numberofGroups)
  
  plot
}




buildColorList = function (groupingColumn, wells = NULL, dataSet = longData, autoGroupLabel = F, displayAverages = F, labelSet = NULL, colorListHub = colorListsHub, 
                           colorOrder = defaultColorOrder){
  
  #dataSet = dataSet %>% mutate(groupColumn = {{groupingColumn}})                # Make a column with a static name that has the grouping data
  #dataSet = dataSet %>% mutate(groupValue = as.numeric(as.factor(groupColumn))) # Make a column with the row's group number                
  #dataSet = dataSet %>% group_by(groupColumn) %>% mutate(groupInstance = row_number()) # Make a column that show what instance of its group the row is 
  
  # - Determine the number of groups and samples -
  numberofGroups = nrow(dataSet %>% count(groupColumn))                         # Count how many different values there are in the grouping column
  uniqueSamples = unique(dataSet$wellNumber)                                    # Get a list of the unique samples

  
  # - Build a colorlist -
  colorsetNest = data.frame(colorListHub$red)                                   #This just makes a dataframe with the correct number of rows
  for(i in 1:numberofGroups){                                                   #For each of the number of colors requested 
    colorsetNest[i] =  eval(parse( text = paste("colorListHub$", colorOrder[i], sep="")))                       #Set the color in that column of colorsetNest to be the color, found in ColorListHub, at that position in color order
    names(colorsetNest)[i] = colorOrder[i]                                      #match the column name to the colorset name
  }
  
  masterColorSet = NULL                                                         #Make an empty colorSets object
  for(i in 1:length(uniqueSamples)){                                            #Do this for each sample   
    currentGroupValue = dataSet$groupValue[i]                                   #Get the row's group value
    
    currentColorSet = colorsetNest[[currentGroupValue]]                         #Set the colorlist to the entry in colorOrder that is the group's value
    
    currentGroupInstance = dataSet$groupInstance[i]
    while(currentGroupInstance > length(currentColorSet)){                      #Make it so that if there are more values than the group size
      currentGroupInstance = currentGroupInstance - length(currentColorSet)     #remove one multiple of the group size until it is smaller
    }                                                                           #This causes the colors to loop around if they go over the group size 
    
    currentColor = currentColorSet[[currentGroupInstance]]                      
    masterColorSet = append(masterColorSet, currentColor)
    
  }
  names(masterColorSet) = uniqueSamples                                         #Connect the colors to the sample names 
  
  assign("masterColorSet", masterColorSet, envir = .GlobalEnv)
  
  
  # - calculate averages - 
  if(displayAverages){
    
    dataSet = dataSet %>% group_by(time, groupColumn) %>%  mutate(groupAverage = mean(value))
    groupNames = unique(dataSet$groupColumn)
    
    for(i in 1:numberofGroups){
      colorPosistion = grep(colorOrder[i], names(colorListHub))[1]              #Use the index to figure out which color we are working on
      if(colorPosistion > length(colorListHub$boldColors)){                     #If we are using one of the background colors 
        colorPosistion = colorPosistion - length(colorListHub$boldColors)       #get the correct position
        
        averageColor = colorListHub$boldColors[colorPosistion]
        
        masterColorSet = append(masterColorSet, averageColor)
        names(masterColorSet)[length(masterColorSet)] = groupNames[i]
      }
    }
  }
  #print(masterColorSet)
  masterColorSet
  
}



plotGrowthK = function(xAxisColumn, groupingColumn,  wells = NULL,  dataSet = growthCurverOutput, autoGroupLabel = T, useLine = F, displayAverages = F, labelSet = NULL, legendTitle = NULL, colorListHub = colorListsHub,
                      colorOrder = defaultColorOrder){
  if(!is.null(wells)){                                                          #If wells isn't empty, limit the dataset to the specified wells
    dataSet = dataSet[which(dataSet$wellNumber %in% wells),]
  }
  # - Add group information columns to the data - 
  dataSet = dataSet %>% mutate(groupColumn = {{groupingColumn}})                # Make a column with a static name that has the grouping data
  dataSet = dataSet %>% mutate(groupValue = as.numeric(as.factor(groupColumn))) # Make a column with the row's group number                
  dataSet = dataSet %>% group_by(groupColumn) %>% mutate(groupInstance = row_number()) # Make a column that show what instance of its group the row is 
  
  
  numberofGroups = nrow(dataSet %>% count(groupColumn))                         # Count how many different values there are in the grouping column
  uniqueSamples = unique(dataSet$wellNumber)                                    # Get a list of the unique samples
  
  masterColorSet = buildColorList({{groupingColumn}}, wells, dataSet, autoGroupLabel, displayAverages, labelSet, colorListHub, colorOrder)
  
  # - plot the graph - 
  plot = ggplot(dataSet, aes(x= {{xAxisColumn}}, y= k, colour=wellNumber))+
    scale_color_manual(values = masterColorSet)+
    ylim(0, 1)+
    ylab("Carrying Capacity [K]")+
    geom_jitter(width = 0.1, size = 3)+
    theme_bw()+
    theme(axis.title.x = element_blank())+
    theme(axis.text.x = element_text(size=12))
  
  plot = autoLegend(plot, data = dataSet, autoGroupLabelVal = autoGroupLabel, labelSetVal = labelSet, legendTitleVal = legendTitle, numGroups = numberofGroups)
  
  plot
}

plotGrowthR = function(xAxisColumn, groupingColumn,  wells = NULL,  dataSet = growthCurverOutput, autoGroupLabel = T, useLine = F, displayAverages = F, labelSet = NULL, legendTitle = NULL, colorListHub = colorListsHub,
                       colorOrder = defaultColorOrder){
  if(!is.null(wells)){                                                          #If wells isn't empty, limit the dataset to the specified wells
    dataSet = dataSet[which(dataSet$wellNumber %in% wells),]
  }
  # - Add group information columns to the data - 
  dataSet = dataSet %>% mutate(groupColumn = {{groupingColumn}})                # Make a column with a static name that has the grouping data
  dataSet = dataSet %>% mutate(groupValue = as.numeric(as.factor(groupColumn))) # Make a column with the row's group number                
  dataSet = dataSet %>% group_by(groupColumn) %>% mutate(groupInstance = row_number()) # Make a column that show what instance of its group the row is 
  
  
  numberofGroups = nrow(dataSet %>% count(groupColumn))                         # Count how many different values there are in the grouping column
  uniqueSamples = unique(dataSet$wellNumber)                                    # Get a list of the unique samples
  
  masterColorSet = buildColorList({{groupingColumn}}, wells, dataSet, autoGroupLabel, displayAverages, labelSet, colorListHub, colorOrder)
  
  # - plot the graph - 
  plot = ggplot(dataSet, aes(x= {{xAxisColumn}}, y= r, colour=wellNumber))+
    scale_color_manual(values = masterColorSet)+
    ylab("Growth Rate [r]")+
    geom_jitter(width = 0.1, size = 3)+
    theme_bw()+
    theme(axis.title.x = element_blank())+
    theme(axis.text.x = element_text(size=12))
  
  plot = autoLegend(plot, data = dataSet, autoGroupLabelVal = autoGroupLabel, labelSetVal = labelSet, legendTitleVal = legendTitle, numGroups = numberofGroups)
  
  plot
}


