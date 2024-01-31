library(ggplot2)
library(gridExtra)
library(growthcurver)
library(dplyr)

# -- well name conversion arguments -- 
rowsFirst = F
numOfSamples = 2

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
  timeData = mainData[which(mainData[1] == "Time [s]"),]
  timeData = timeData[1,]
  rownames(timeData)= "time"
  timeData = timeData[,-1]
  
  meanData = mainData[which(mainData[1] == "Mean"),]
  rownames(meanData) = mainData[(which(mainData[1] == "Mean")-3),1]
  rownames(meanData) = convertBoth(rownames(meanData), rowFirst)
  rownames(meanData) = sapply(rownames(meanData) , paste, instance, sep="")

  meanData = meanData[,-1]
  #meanData = mutate_all(meanData, as.numeric)
  
  if(addTime){
    cleanData = rbind(timeData, meanData)
  }else{
    cleanData = meanData
  }
  cleanData = data.frame(t(cleanData))
  cleanData = lapply(FUN = as.numeric, cleanData)
  cleanData= data.frame(cleanData)
  
  assign("wellNames", names(cleanData), envir = .GlobalEnv)
  
  cleanData
}

# -- make plotting function ---

colorset1 = c("brown1", "blue", "chocolate1", "deepskyblue")
colorset2 = c("brown3", "blue3", "chocolate3", "deepskyblue3")
colorset3 = c("brown4", "blue4", "chocolate4", "deepskyblue4")
colorset4 = c("brown2", "blue2", "chocolate2", "deepskyblue2")

colorset1 = c("brown1", "brown3", "brown4", "brown2")
colorset2 = c("blue", "blue3", "blue4", "blue2")
colorset3 = c("chocolate1", "chocolate3", "chocolate4", "chocolate2")
colorset4 = c("deepskyblue", "deepskyblue3", "deepskyblue4", "deepskyblue2")

colorsets = data.frame(colorset1, colorset2, colorset3, colorset4)
colorset = unlist(colorsets[1:numOfPlates,])
names(colorset) =NULL



combinedPlot = function(wells){
  plot <- ggplot( aes(x=time), data = .GlobalEnv$cleanData)
  for (i in 1:length(wells)) { 
    loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="")
    plot <- plot + eval(parse(text=loop_input))  
  }
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot
}

combinedPlotColor = function(wells, colorsetNumber = numberofGroups, groupedNumber =2, groupRepeatNumber = 2, colorOrder = c("red", "orange", "blue", "cyan"), labelSet = NULL, legendTitle = NULL){
  red = c("red", "red3", "firebrick3", "red2")
  blue = c("blue", "blue3", "blue4", "blue2")
  orange = c("chocolate1", "chocolate3", "chocolate4", "chocolate2")
  cyan = c("deepskyblue", "deepskyblue3", "deepskyblue4", "deepskyblue2")
  pink = c("hotpink", "hotpink1", "hotpink2", "hotpink3")
  lightgreen = c("springgreen1", "springgreen2", "springgreen3", "springgreen4")
  purple = c("purple", "purple1", "purple2", "purple3")
  
  
  colorsetNames = c("colorset1", "colorset2", "colorset3", "colorset4","colorset5","colorset6","colorset7")
  
  for(i in 1:colorsetNumber){
    assign(colorsetNames[i], eval(parse( text = colorOrder[i])))
  }
  
  
  colorsets = NULL
  for(i in 1:groupRepeatNumber){
    rowSet = ((groupedNumber *(i-1))+1):(groupedNumber * i)
    colorsets = cbind(colorsets, colorset1[rowSet], colorset2[rowSet], colorset3[rowSet], colorset4[rowSet])
  }
  colorsets = data.frame(colorsets)
  colorset = unlist(colorsets) 
  names(colorset) = sort(wells)
  plot <- ggplot( aes(x=time), data = cleanData)
  for (i in 1:length(wells)) { 
    loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="")
    plot <- plot + eval(parse(text=loop_input))  
  }
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot = plot + scale_color_manual(values = colorset)
  plot = plot + ylim(0,1) + ylab("Absorbance")
  
  if(!is.null(legendTitle)){
    plot = plot +  guides(color = guide_legend(title = legendTitle))
  }

  if(!is.null(labelSet)){
    startOfEachColor = NULL
    for(i in 0:(colorsetNumber-1)){
      colorstart = (i*groupedNumber)+1
      startOfEachColor = append(startOfEachColor, colorstart)
    }
    
    plot = plot + scale_color_manual(values = colorset, 
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
