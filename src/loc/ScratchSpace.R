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


groupColumn = strain
combinedPlotColorLongFancy(wells, groupingColumn = column, groupedNumber = 8, labelSet = c("A", "B", "C", "D", "E", "F", "G", "H"))
combinedPlotColorLongFancy(wells, groupingColumn = column, autoGroupLabel = T)
combinedPlotColorLongFancy(wells, groupingColumn = strain, autoGroupLabel = T)


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
