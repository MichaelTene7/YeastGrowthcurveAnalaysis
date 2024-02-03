library(ggplot2)
library(gridExtra)
library(growthcurver)
library(dplyr)
library(tidyr)

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



# ---- color lists ----
{
  colorListsHub = NULL
  colorListsHub$red = c("red", "red3", "firebrick3", "red2", "red4", "red1", "firebrick", "firebrick2")
  colorListsHub$blue = c("blue", "blue4", "royalblue4", "blue3", "blue2", "navy", "blue1", "mediumblue")
  colorListsHub$orange = c("chocolate1", "chocolate3", "chocolate4", "chocolate2", "chocolate", "darkorange", "darkorange2", "darkorange3")
  colorListsHub$cyan = c("deepskyblue", "deepskyblue3", "deepskyblue4", "deepskyblue2", "darkturquoise", "turquoise3", "cyan3", "skyblue3")
  colorListsHub$pink = c("hotpink", "hotpink1", "hotpink2", "hotpink3", "orchid", "orchid1", "orchid3", "orchid2")
  colorListsHub$green = c("springgreen1", "springgreen2", "springgreen3", "springgreen4", "seagreen3", "seagreen", "seagreen1", "seagreen2")
  colorListsHub$purple = c("purple", "purple1", "purple2", "purple3", "darkorchid", "mediumpurple", "darkorchid4", "darkorchid2")
  colorListsHub$yellow = c("gold1", "gold3", "gold4", "gold2", "goldenrod1", "goldenrod2", "goldenrod", "goldenrod3")
  colorListsHub$tan = c("papayawhip", "peachpuff", "peachpuff2", "peachpuff3", "wheat1", "wheat2", "wheat3", "wheat")
  colorListsHub$lightblue = c("lightblue", "lightblue1", "lightblue2", "lightblue3", "skyblue", "skyblue1", "skyblue2", "lightsteelblue1")
  colorListsHub$lightred = c("indianred", "indianred1", "indianred2", "indianred3", "brown3", "coral2", "coral3", "coral1")
  colorListsHub$brightgreen = c("chartreuse", "chartreuse2", "chartreuse3", "olivedrab2", "olivedrab1", "olivedrab3", "darkolivegreen1", "darkolivegreen2")
  
  colorListsHub$backgroundRed = rep("indianred2", 8)
  colorListsHub$backgroundBlue = rep("cornflowerblue", 8)
  colorListsHub$backgroundOrange = rep("orange1", 8)
  colorListsHub$backgroundCyan = rep("turquoise", 8)
  colorListsHub$backgroundPink = rep("rosybrown1", 8)
  colorListsHub$backgroundGreen = rep("lightgreen", 8)
  colorListsHub$backgroundPurple = rep("mediumorchid2", 8)
  colorListsHub$backgroundYellow = rep("lightgoldenrod", 8)
  colorListsHub$backgroundTan = rep("papayawhip", 8)
  colorListsHub$backgroundLightBlue = rep("lightblue1", 8)
  colorListsHub$backgroundLightRed = rep("lightcoral", 8)
  colorListsHub$backgroundBrightGreen = rep("darkolivegreen1", 8)
  
  colorListsHub$boldColors = c("darkred", "darkblue", "darkorange", "cyan4", "hotpink4", "darkgreen", "magenta4", "yellow3", "tan", "skyblue3", "tomato", "olivedrab4")
}


# ---- Long-Data plotting functions ----

combinedPlotLong = function(wells = NULL, dataSet = .GlobalEnv$cleanData){
  if(!is.null(wells)){                                                          #If wells isn't empty, limit the dataset to the specified wells
    dataSet = dataSet[which(dataSet$wellName %in% wells),]
  }
  plot <- ggplot(data = dataSet,  aes(x=time, y=value, color=wellName))
  plot <- plot + guides( color = guide_legend(title = "",) )                    #Add a legend to the plot
  plot                                                                          #Return the plot 
}

plotGrowth = function(wells = NULL, groupingColumn = NULL, colorOrder = c("red", "blue", "orange", "cyan", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen"), autoGroupLabel = F, useLine = F, labelSet = NULL, legendTitle = NULL, colorListHub = colorListsHub, dataSet = longData){
  
  if(!is.null(wells)){                                                          #If wells isn't empty, limit the dataset to the specified wells
    dataSet = dataSet[which(dataSet$wellName %in% wells),]
  }
  
  
  numberofGroups = nrow(dataSet %>% count( {{groupingColumn}} ))                # Count how many different values there are in the grouping column
  uniqueSamples = unique(dataSet$wellName)                                      # Get a list of the unique samples
  
  
  dataSet = dataSet %>% mutate(groupColumn = {{groupingColumn}})                 # Make a column with a static name that has the grouping data
  
  dataSet = dataSet %>% mutate(groupValue = as.numeric(as.factor(groupColumn))) # Make a column with the row's group number                
  dataSet = dataSet %>% group_by({{groupingColumn}}) %>% mutate(groupInstance = row_number()) # Make a column that show what instance of its group the row is 
  
  
  colorsetNest = data.frame(colorListHub$red)                                   #This just makes a dataframe with the correct number of rows
  
  for(i in 1:numberofGroups){                                                   #For each of the number of colors requested 
    colorsetNest[i] =  eval(parse( text = paste("colorListHub$", colorOrder[i], sep="")))                       #Set the color in that column of colorsetNest to be the color, found in ColorListHub, at that position in color order
    names(colorsetNest)[i] = colorOrder[i]                                      #match the column name to the colorset name
  }
  
  masterColorSet = NULL                                                         #Make an empty colorSets object
  for(i in 1:length(uniqueSamples)){                                         #Do this for each sample   
    currentGroupValue = dataSet$groupValue[i]                                   #Get the row's group value
    currentColorSet = colorListHub[[currentGroupValue]]                         #Set the colorlist to the entry in colorOrder that is the group's value
    
    currentGroupInstance = dataSet$groupInstance[i]
    while(currentGroupInstance > length(currentColorSet)){                              #Make it so that if there are more values than the group size
      currentGroupInstance = currentGroupInstance - length(currentColorSet)                     #remove one multiple of the group size until it is smaller
    }                                                                           #This causes the colors to loop around if they go over the group size 
    
    currentColor = currentColorSet[[currentGroupInstance]]                      
    
   
    masterColorSet = append(masterColorSet, currentColor)
      
  }
  #print(masterColorSet)
  
  names(masterColorSet) = sort(uniqueSamples)                                   #Connect the colors to the sample names 
  plot <- ggplot( aes(x=time, y=value, color=wellName), data = dataSet) 
  if(useLine){ plot = plot +geom_line()}else{plot = plot +geom_point()}
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot = plot + scale_color_manual(values = masterColorSet)
  plot = plot + ylim(0,1) + ylab("Absorbance") +xlab("Time [s]") +theme_bw()
  
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
                                     breaks = sort(wells)[startOfEachGroup],
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



