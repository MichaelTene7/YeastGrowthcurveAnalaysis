library(ggplot2)
library(gridExtra)
library(growthcurver)
library(dplyr)
library(tidyr)

# -- well name conversion arguments -- 
rowsFirst = F
numOfSamples = 2
numOfPlates = 1

# ---- Clean the data  ---- 

# - generate meaningful well names -  
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


# - get the means out - 
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

dataCleanerLong = function(mainData, instance = NULL, addTime = T, rowFirst = rowsFirst ){
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



longData = pivot_longer(cleanData, 2:ncol(cleanData))





# -- make plotting function ---

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

# -- Long-Data plotting functions

combinedPlot = function(wells){
  plot <- ggplot( aes(x=time), data = .GlobalEnv$cleanData)                     # Set the X axis as time
  for (i in 1:length(wells)) {                                                  # For each well in the vector of wells provided 
    loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="") #Write out the ggplot command to graph that well, and give it a unique color
    plot <- plot + eval(parse(text=loop_input))                                 # Draw that line on top of the existing plot 
  }
  plot <- plot + guides( color = guide_legend(title = "",) )                    #Add a legend to the plot
  plot                                                                          #Return the plot 
}

combinedPlotColorLong = function(wells, numberofGroups = .GlobalEnv$numberofGroups, groupedNumber =3, groupsRepeatTimes = 1, colorOrder = c("red", "blue", "orange", "cyan", "pink", "green", "purple", "yellow", "tan", "lightblue", "lightred", "brightgreen"), labelSet = NULL, legendTitle = NULL){
  #This is a set of colors, eachof which has four versions of it 
  red = c("red", "red3", "firebrick3", "red2", "red4", "red1", "firebrick", "firebrick2")
  blue = c("blue", "blue3", "blue4", "blue2", "navy", "blue1", "royalblue4", "mediumblue")
  orange = c("chocolate1", "chocolate3", "chocolate4", "chocolate2", "chocolate", "darkorange", "darkorange2", "darkorange3")
  cyan = c("deepskyblue", "deepskyblue3", "deepskyblue4", "deepskyblue2", "darkturquoise", "turquoise3", "cyan3", "skyblue3")
  pink = c("hotpink", "hotpink1", "hotpink2", "hotpink3", "orchid", "orchid1", "orchid3", "orchid2")
  green = c("springgreen1", "springgreen2", "springgreen3", "springgreen4", "seagreen3", "seagreen", "seagreen1", "seagreen2")
  purple = c("purple", "purple1", "purple2", "purple3", "darkorchid", "mediumpurple", "darkorchid4", "darkorchid2")
  yellow = c("gold1", "gold3", "gold4", "gold2", "goldenrod1", "goldenrod2", "goldenrod", "goldenrod3")
  tan = c("papayawhip", "peachpuff", "peachpuff2", "peachpuff3", "wheat1", "wheat2", "wheat3", "wheat")
  lightblue = c("lightblue", "lightblue1", "lightblue2", "lightblue3", "skyblue", "skyblue1", "skyblue2", "lightsteelblue1")
  lightred = c("indianred", "indianred1", "indianred2", "indianred3", "brown3", "coral2", "coral3", "coral1")
  brightgreen = c("chartreuse", "chartreuse2", "chartreuse3", "olivedrab2", "olivedrab1", "olivedrab3", "darkolivegreen1", "darkolivegreen2")
  
  backgroundBlue = c("cornflowerblue", "cornflowerblue", "cornflowerblue", "cornflowerblue", "cornflowerblue", "cornflowerblue", "cornflowerblue", "cornflowerblue")
  backgroundRed = c("lightcoral", "lightcoral", "lightcoral", "lightcoral", "lightcoral", "lightcoral", "lightcoral", "lightcoral")
  boldAverages = c("darkred", "darkblue", "darkorange", "cyan4", "hotpink4", "darkgreen", "magenta4", "yellow3", "tan", "skyblue3", "tomato", "olivedrab4")
  
  colorsetNest = data.frame(red)                                                #This just makes a dataframe with the correct number of rows
  
  for(i in 1:numberofGroups){                                                   #For each of the number of colors requested 
    colorsetNest[i] =  eval(parse( text = colorOrder[i]))                       #Set the color in that column of colorsetNest to be the color at that position in color order
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
  plot <- ggplot( aes(x=time, y=value, color=name), data = longData) +geom_line()
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

wells = unique(longData$name)

combinedPlotColorLong(wells, numberofGroups = 8, groupedNumber = 8, labelSet = c("A", "B", "C", "D", "E", "F", "G", "H"))














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
combinedPlotColor = function(wells, numberofGroups = .GlobalEnv$numberofGroups, groupedNumber =3, groupsRepeatTimes = 1, colorOrder = c("red", "blue", "orange", "cyan", "pink", "lightgreen", "purple"), labelSet = NULL, legendTitle = NULL){
  #This is a set of colors, eachof which has four versions of it 
  red = c("red", "red3", "firebrick3", "red2", "red4", "red1", "firebrick", "firebrick2")
  blue = c("blue", "blue3", "blue4", "blue2", "navy", "blue1", "royalblue4", "mediumblue")
  orange = c("chocolate1", "chocolate3", "chocolate4", "chocolate2", "chocolate", "darkorange", "darkorange2", "darkorange3")
  cyan = c("deepskyblue", "deepskyblue3", "deepskyblue4", "deepskyblue2", "darkturquoise", "turquoise3", "cyan3", "skyblue3")
  pink = c("hotpink", "hotpink1", "hotpink2", "hotpink3", "orchid", "orchid1", "orchid3", "orchid2")
  green = c("springgreen1", "springgreen2", "springgreen3", "springgreen4", "seagreen3", "seagreen", "seagreen1", "seagreen2")
  purple = c("purple", "purple1", "purple2", "purple3", "darkorchid", "mediumpurple", "darkorchid4", "darkorchid2")
  yellow = c("gold1", "gold3", "gold4", "gold2", "goldenrod1", "goldenrod2", "goldenrod", "goldenrod3")
  tan = c("papayawhip", "peachpuff", "peachpuff2", "peachpuff3", "wheat1", "wheat2", "wheat3", "wheat")
  lightblue = c("lightblue", "lightblue1", "lightblue2", "lightblue3", "skyblue", "skyblue1", "skyblue2", "lightsteelblue1")
  lightred = c("indianred", "indianred1", "indianred2", "indianred3", "brown3", "coral2", "coral3", "coral1")
  brightgreen = c("chartreuse", "chartreuse2", "chartreuse3", "olivedrab2", "olivedrab1", "olivedrab3", "darkolivegreen1", "darkolivegreen2")
  
  backgroundBlue = c("cornflowerblue", "cornflowerblue", "cornflowerblue", "cornflowerblue", "cornflowerblue", "cornflowerblue", "cornflowerblue", "cornflowerblue")
  backgroundRed = c("lightcoral", "lightcoral", "lightcoral", "lightcoral", "lightcoral", "lightcoral", "lightcoral", "lightcoral")
  boldAverages = c("darkred", "darkblue", "darkorange", "cyan4", "hotpink4", "darkgreen", "magenta4", "yellow3", "tan", "skyblue3", "tomato", "olivedrab4")
  
  colorsetNest = data.frame(red)                                                #This just makes a dataframe with the correct number of rows
  
  for(i in 1:numberofGroups){                                                   #For each of the number of colors requested 
    colorsetNest[i] =  eval(parse( text = colorOrder[i]))                       #Set the color in that column of colorsetNest to be the color at that position in color order
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
