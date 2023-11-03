library(ggplot2)
library(gridExtra)
library(growthcurver)
library(dplyr)

# -- well name conversion arguments -- 
rowFirst = F
numOfPlates = 2

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
dataCleaner = function(mainData, instance = NULL, addTime = F, rowFirst = F ){
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
  plot <- ggplot( aes(x=time), data = cleanData)
  for (i in 1:length(wells)) { 
    loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="")
    plot <- plot + eval(parse(text=loop_input))  
  }
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot
}

combinedPlotColor = function(wells, colorsetNumber =3){
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
  names(colorset) =NULL
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