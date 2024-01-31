# -- get the functions ---
source("Src/Loc/plateReaderAnalysisCore.R")


# -- set plate input settings --- 
rowMeanings = list(
  c("A", "2490A1_β"),
  c("B", "2490A2_β"),
  c("C", "2491A3_β"),
  c("D", "2491A4_β"),
  c("E", "2490A1_γ"),
  c("F", "2490A2_γ"),
  c("G", "2491A3_γ"),
  c("H", "2491A4_γ")
)
colMeanings = list(
  c("1", "As_ε"),
  c("2", "Glu_ε"),
  c("3", "Ser_ε"),
  c("4", "Thr_ε"),
  c("5", "As_ζ"),
  c("6", "Glu_ζ"),
  c("7", "Ser_ζ"),
  c("8", "Thr_ζ")
)
rowsFirst = T
numberofGroups = 4


# ---- Read the data ---- 

mainData = read.csv("Data/11_10_ComplementationReorder.csv")

cleanData = dataCleaner(mainData)


thrWells = wellNames[grep("Thr", wellNames)]
combinedPlot(thrWells)
combinedPlotColor(thrWells)

gluWells = wellNames[grep("Glu", wellNames)]
combinedPlotColor(gluWells)

asWells = wellNames[grep("As", wellNames)]
combinedPlotColor(asWells)

serWells = wellNames[grep("Ser", wellNames)]
combinedPlotColor(serWells)


thr2490wells = wellNames[grepl("2490", wellNames) & grepl("Thr", wellNames)]
colorOrder = c("brown1", "brown3", "blue", "blue3", "brown4", "brown2", "blue4", "blue2")
combinedPlotColorCustom(thr2490wells)

ser2490wells = wellNames[grepl("2490", wellNames) & grepl("Ser", wellNames)]
colorOrder = c("brown1", "brown3", "brown4", "brown2","blue", "blue3","blue4", "blue2")
combinedPlotColorCustom(ser2490wells)



colorOrder = c("brown1", "brown3", "blue", "blue3", "brown4", "brown2", "blue4", "blue2")

A12490Wells = wellNames[grep("2490A1", wellNames)]
A1_2490Plot = combinedPlotColor(A12490Wells, colorOrder = c("red", "pink", "blue", "cyan"), labelSet = c("As", "Glu", "Ser", "Thr"), legendTitle = "2490A1")

A22490Wells = wellNames[grep("2490A2", wellNames)]
A2_2490Plot =combinedPlotColor(A22490Wells, colorOrder = c("red", "pink", "blue", "cyan"), labelSet = c("As", "Glu", "Ser", "Thr"), legendTitle = "2490A2")


A12491Wells = wellNames[grep("2491A1", wellNames)]
A1_2491Plot =combinedPlotColor(A12491Wells, colorOrder = c("red", "pink", "blue", "cyan"), labelSet = c("As", "Glu", "Ser", "Thr"), legendTitle = "2491A1")

A22491Wells = wellNames[grep("2491A2", wellNames)]
A2_2491Plot =combinedPlotColor(A22491Wells, colorOrder = c("red", "pink", "blue", "cyan"), labelSet = c("As", "Glu", "Ser", "Thr"), legendTitle = "2491A2")

grid.arrange(A1_2490Plot, A2_2490Plot, A1_2491Plot, A2_2491Plot, ncol = 2)







