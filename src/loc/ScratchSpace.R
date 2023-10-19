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
  colorset = c( "red", "red3", "chocolate1","darkgreen", "green4")
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
