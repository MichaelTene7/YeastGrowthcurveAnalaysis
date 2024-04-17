library(ggplot2)
library(gridExtra)
library(growthcurver)
library(dplyr)
library(tidyr)

source("Src/Reu/colorLists.R")

# -- well name conversion arguments -- 
rowsFirst = F
numOfSamples = 2
numOfPlates = 1

# ------ Clean the data  ------

# --- generate meaningful well names ---  
convertBoth = function(wellNumbers, rowFirst = F, rowMeaning = rowMeanings, colMeaning = colMeanings){
  wellNames = wellNumbers
  if(rowFirst){
    firstSet = rowMeanings
    secondSet = colMeanings
  }else{
    firstSet = colMeanings
    secondSet = rowMeanings
  }
  
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


# --- get the means out of the data file ---





dataCleaner = function(mainData, instance = NULL, addTime = T, rowFirst = rowsFirst ){
  # -- clean the data -- #
  
  #Isolate the time data elsewhere so that it can be added in later 
  timeData = mainData[which(mainData[1] == "Time [s]"),]                        #Select the time columns
  timeData = timeData[1,]                                                       #They're all identical so we only need the first one
  rownames(timeData)= "time"                                                    #Move the 'time' label to the rowname 
  timeData = timeData[,-1]                                                      #Remove the row with 'time' in text 
  # -- 
  
  # Get the means of all of the observation
  meanData = mainData[which(mainData[1] == "Mean"),]                            # The data in in the row titled mean 
  rownames(meanData) = mainData[(which(mainData[1] == "Mean")-3),1]             # The well name of the data is always stored three rows above the mean data 
  rownames(meanData) = convertBoth(rownames(meanData), rowFirst)                # Convert the well name using the specified key
  rownames(meanData) = sapply(rownames(meanData) , paste, instance, sep="")     # If there is an Instance marker added (to give different plates different row names), add it to the row name

  meanData = meanData[,-1]                                                      #Remove the row with the 'mean' label as text 
  #meanData = mutate_all(meanData, as.numeric)
  
  if(addTime){                                                                  #If adding a time row, add the row
    cleanData = rbind(timeData, meanData)
  }else{                                                                        #Else do nothing 
    cleanData = meanData
  }
  cleanData = data.frame(t(cleanData))                                          #Rotate the data 90 degrees 
  cleanData = lapply(FUN = as.numeric, cleanData)                               #Convert all values to numeric
  cleanData= data.frame(cleanData)                                              #Save as dataframe
  
  assign("wellNames", names(cleanData), envir = .GlobalEnv)                     #Set the global environment (main script) wellNames object to the names of these wells 
  
  cleanData                                                                     #Return the cleaned data 
}

# -- Pivot the data -- 

dataCleanerLong = function(mainData, instance = NULL, addTime = T, rowFirst = rowsFirst, splitColumnNames = NULL ){
  # -- clean the data -- #
  
  #Isolate the time data elsewhere so that it can be added in later 
  timeData = mainData[which(mainData[1] == "Time [s]"),]                        #Select the time columns
  timeData = timeData[1,]                                                       #They're all identical so we only need the first one
  rownames(timeData)= "time"                                                    #Move the 'time' label to the rowname 
  timeData = timeData[,-1]                                                      #Remove the row with 'time' in text 
  # -- 
  
  # Get the means of all of the observation
  meanData = mainData[which(mainData[1] == "Mean"),]                            # The data in in the row titled mean 
  rownames(meanData) = mainData[(which(mainData[1] == "Mean")-3),1]             # The the rowname to the well number, well number of the data is always stored three rows above the mean data 
  meanData = meanData[,-1]                                                      #Remove the row with the 'mean' label as text 
  #meanData = mutate_all(meanData, as.numeric)
  
  if(addTime){                                                                  #If adding a time row, add the row
    cleanData = rbind(timeData, meanData)
  }else{                                                                        #Else do nothing 
    cleanData = meanData
  }
  cleanData = data.frame(t(cleanData))                                          #Rotate the data 90 degrees 
  cleanData = lapply(FUN = as.numeric, cleanData)                               #Convert all values to numeric
  cleanData= data.frame(cleanData)                                              #Save as dataframe

  
  # -- organize the data in Longer format -- 
  
  longData = pivot_longer(cleanData, 2:ncol(cleanData))                         # pivot the data so that each timepoint is its own row
  names(longData)[2] = "wellNumber"    
  
  
  # - separate data from wellNumber into columns -
  longData$wellNumber2 = longData$wellNumber
  longData = separate_wider_position(longData, wellNumber2, c(row = 1, column = 1))
  
  # - make a wellName based on the wellNumber -
  longData$wellName = longData$wellNumber
  longData$wellName = convertBoth(longData$wellName, rowFirst)                # Convert the well name using the specified key
  longData$wellName = sapply(longData$wellName , paste, instance, sep="")     # If there is an Instance marker added (to give different plates different row names), add it to the row name
  
  # - separate data from wellName into columns -

  longData$wellName2 = longData$wellName
  if(is.null(splitColumnNames)){
    longData = separate_wider_delim(longData, wellName2, "_", names_sep = "-")
  }else{
    longData = separate_wider_delim(longData, wellName2, "_", names = splitColumnNames)
  }
    
  assign("wellNames", names(longData$wellName), envir = .GlobalEnv)                     #Set the global environment (main script) wellNames object to the names of these wells 
  
  longData                                                                     #Return the cleaned data 
}


# ------ make plotting functions -------






# ---- Long-Data plotting functions ----

combinedPlotLong = function(wells = NULL, dataSet = .GlobalEnv$cleanData){
  if(!is.null(wells)){                                                          #If wells isn't empty, limit the dataset to the specified wells
    dataSet = dataSet[which(dataSet$wellName %in% wells),]
  }
  plot <- ggplot(data = dataSet,  aes(x=time, y=value, color=wellName))
  plot <- plot + guides( color = guide_legend(title = "",) )                    #Add a legend to the plot
  plot                                                                          #Return the plot 
}

plotGrowth = function(groupingColumn, wells = NULL, dataSet = cleanData, colorOrder = c("red", "blue", "orange", "cyan", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen"), autoGroupLabel = F, useLine = F, displayAverages = F, labelSet = NULL, legendTitle = NULL, colorListHub = colorListsHub){
  
  if(!is.null(wells)){                                                          #If wells isn't empty, limit the dataset to the specified wells
    dataSet = dataSet[which(dataSet$wellNumber %in% wells),]
  }
  
  numberofGroups = nrow(dataSet %>% count( {{groupingColumn}} ))                # Count how many different values there are in the grouping column
  uniqueSamples = unique(dataSet$wellNumber)                                      # Get a list of the unique samples
  
  
  # - Add group information columns to the data - 
  dataSet = dataSet %>% mutate(groupColumn = {{groupingColumn}})                # Make a column with a static name that has the grouping data
  dataSet = dataSet %>% mutate(groupValue = as.numeric(as.factor(groupColumn))) # Make a column with the row's group number                
  dataSet = dataSet %>% group_by({{groupingColumn}}) %>% mutate(groupInstance = row_number()) # Make a column that show what instance of its group the row is 
  
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
  
  #print(colorOrder)
  #print(masterColorSet)
  
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
  
  
  # - plot the graph - 
  plot <- ggplot( aes(x=time, y=value, color=wellName), data = dataSet) 
  if(useLine){ plot = plot +geom_line()}else{plot = plot +geom_point()}
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot = plot + scale_color_manual(values = masterColorSet)
  plot = plot + ylim(0,1) + ylab("Absorbance") +xlab("Time [s]") +theme_bw()
  
  if(displayAverages){
    if(useLine){plot = plot + geom_line(aes(y = groupAverage, color = groupValue))
    }else{ plot = plot + geom_point(aes(y = groupAverage, color = groupColumn))}
  }
  
  if(!is.null(legendTitle)){
    plot = plot +  guides(color = guide_legend(title = legendTitle))
  }
  
  if(!is.null(labelSet) | autoGroupLabel){
    startOfEachGroup = NULL
    for(i in 1:(numberofGroups)){
      groupstart = which(dataSet$groupValue == i)[1]
      startOfEachGroup = append(startOfEachGroup, groupstart)
    }
    print(startOfEachGroup)
    
    if(autoGroupLabel){
      labelSet = NULL
      for(i in 1:numberofGroups){
        labelSet[i] = dataSet$groupColumn[startOfEachGroup[i]]
      }
    }
    
    plot = plot + scale_color_manual(values = masterColorSet, 
                                     breaks = dataSet$wellName[startOfEachGroup],
                                     labels = labelSet
    ) 
  }
  plot
}


combinedPlotColorLong = function(wells, numberofGroups = .GlobalEnv$numberofGroups, groupedNumber =3, groupsRepeatTimes = 1, colorOrder = c("red", "blue", "orange", "cyan", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen"), labelSet = NULL, legendTitle = NULL, colorListHub = colorListsHub, dataSet = longData){
  #This is a set of colors, eachof which has four versions of it 
  
  colorsetNest = data.frame(colorListHub$red)                                                #This just makes a dataframe with the correct number of rows
  
  for(i in 1:numberofGroups){                                                   #For each of the number of colors requested 
    colorsetNest[i] =  eval(parse( text = paste("colorListHub$", colorOrder[i], sep="")))                       #Set the color in that column of colorsetNest to be the color, found in ColorListHub, at that position in color order
    names(colorsetNest)[i] = colorOrder[i]                                      #match the column name to the colorset name
  }
  
  masterColorSet = NULL                                                         #Make an empty colorSets object
  for(i in 1:groupsRepeatTimes){                                                #Do this for each pass of the groups                                           
    rowSet = ((groupedNumber *(i-1))+1):(groupedNumber * i)                     #Determine which indexes are being used this run. Ie. (groupedNumber =2) if this is repeat 1, get indexes 1 and 2; if this is repeat 2, get indexes 3 and 4 
    for(j in 1:numberofGroups){                                                 #For each of the colorsets                                             
      masterColorSet = append(masterColorSet, colorsetNest[rowSet,j])                     #Add the correct indexes of the colorset to the master colorset 
    }
  }

  names(masterColorSet) = sort(wells)
  plot <- ggplot( aes(x=time, y=value, color=wellName), data = dataSet) +geom_line()
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot = plot + scale_color_manual(values = masterColorSet)
  plot = plot + ylim(0,1) + ylab("Absorbance") +xlab("Time [s]") +theme_bw()
  
  if(!is.null(legendTitle)){
    plot = plot +  guides(color = guide_legend(title = legendTitle))
  }
  
  if(!is.null(labelSet)){
    startOfEachColor = NULL
    for(i in 0:(numberofGroups-1)){
      colorstart = (i*groupedNumber)+1
      startOfEachColor = append(startOfEachColor, colorstart)
    }
    
    plot = plot + scale_color_manual(values = masterColorSet, 
                                     breaks = sort(wells)[startOfEachColor],
                                     labels = labelSet
    ) 
  }
  plot
}


# -- Non-long plotting functions -- 


#This function simply draws each well sequentially onto a single graph as a separate series
combinedPlot = function(wells){
  plot <- ggplot( aes(x=time), data = .GlobalEnv$cleanData)                     # Set the X axis as time
  for (i in 1:length(wells)) {                                                  # For each well in the vector of wells provided 
    loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="") #Write out the ggplot command to graph that well, and give it a unique color
    plot <- plot + eval(parse(text=loop_input))                                 # Draw that line on top of the existing plot 
  }
  plot <- plot + guides( color = guide_legend(title = "",) )                    #Add a legend to the plot
  plot                                                                          #Return the plot 
}



#Param groupedNumber is the number of wells in the same group in a row
#Param groupsRepeatTimes is if when the list has been completed, how many times it should go back and start with the first group
combinedPlotColor = function(wells, numberofGroups = .GlobalEnv$numberofGroups, groupedNumber =3, groupsRepeatTimes = 1, colorOrder = c("red", "blue", "orange", "cyan", "pink", "lightgreen", "purple"), labelSet = NULL, legendTitle = NULL, colorListHub = colorListsHub){
  colorsetNest = data.frame(colorListHub$red)                                                #This just makes a dataframe with the correct number of rows
  
  for(i in 1:numberofGroups){                                                   #For each of the number of colors requested 
    colorsetNest[i] =  eval(parse( text = paste("colorListHub$", colorOrder[i], sep="")))                      #Set the color in that column of colorsetNest to be the color at that position in color order
    names(colorsetNest)[i] = colorOrder[i]                                      #match the column name to the colorset name
  }
  
  masterColorSet = NULL                                                         #Make an empty colorSets object
  for(i in 1:groupsRepeatTimes){                                                #Do this for each pass of the groups                                           
    rowSet = ((groupedNumber *(i-1))+1):(groupedNumber * i)                     #Determine which indexes are being used this run. Ie. (groupedNumber =2) if this is repeat 1, get indexes 1 and 2; if this is repeat 2, get indexes 3 and 4 
    for(j in 1:numberofGroups){                                                 #For each of the colorsets                                             
      masterColorSet = append(masterColorSet, colorsetNest[rowSet,j])                     #Add the correct indexes of the colorset to the master colorset 
    }
  }
  
  
  names(masterColorSet) = sort(wells)
  plot <- ggplot( aes(x=time), data = cleanData)
  for (i in 1:length(wells)) { 
    loop_input = paste("geom_smooth(aes(y=",wells[i],",color='",wells[i],"'))", sep="")
    plot <- plot + eval(parse(text=loop_input))  
  }
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot = plot + scale_color_manual(values = masterColorSet)
  plot = plot + ylim(0,1) + ylab("Absorbance") +xlab("Time [s]") +theme_bw()
  
  if(!is.null(legendTitle)){
    plot = plot +  guides(color = guide_legend(title = legendTitle))
  }

  if(!is.null(labelSet)){
    startOfEachColor = NULL
    for(i in 0:(numberofGroups-1)){
      colorstart = (i*groupedNumber)+1
      startOfEachColor = append(startOfEachColor, colorstart)
    }
    
    plot = plot + scale_color_manual(values = masterColorSet, 
                                     breaks = sort(wells)[startOfEachColor],
                                     labels = labelSet
                                     ) 
  }
  plot
}




combinedPlotColorCustom = function(wells, colorset = colorOrder){
  plot <- ggplot( aes(x=time), data = cleanData)
  for (i in 1:length(wells)) { 
    loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="")
    plot <- plot + eval(parse(text=loop_input))  
  }
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot = plot + scale_color_manual(values = colorset)
  plot = plot + ylim(0,1)
  plot
}



