Venn4Plot.st <-
function(list1, list2, list3, list4,listNames, filename, img.fmt = "pdf", Table1, 
                      Table2, Table3, Table4, ColName= "GeneSymbol", resDir=NULL ){
  
  #list1: Vector amb els Gene symbols de una de les llistes
  #list2: Vector amb els Gene symbols de una de les llistes
  #list3: Vector amb els Gene symbols de una de les llistes
  #list4: Vector amb els Gene symbols de una de les llistes
  #listNames: Nom dels mÃ¨todes que corresponen a cada una de les llistes
  #filename: Nom del output file
  #Table1: Dataset del primer mÃ¨tode
  #Table2: Dataset del segon mÃ¨tode
  #Table3: Dataset del tercer mÃ¨tode
  #Table4: Dataset del quart mÃ¨tode
  
  require(Vennerable) 
  require(colorfulVennPlot) #Per generar plots amb colors diferents
  require(RColorBrewer)  
  #establim els colors per als plots
  cols <- c(brewer.pal(8,"Pastel1"), brewer.pal(8,"Pastel2"))  #Fixar els colors per als venn diagrams amb 4 condicions
  cols <- cols[c(8,2,3,15,5,6,7,1,9,10,11,12,13,14,4,16)]      ##Reorganitzar els colors per a que no coincideixin tonalitats semblants
  
  if (!is.null(resDir)){
    resultsDir=resDir
  }
  
  #Ens assegurem de que no hi ha NA
  list1 <- list1[!is.na(list1)]
  list2 <- list2[!is.na(list2)]
  list3 <- list3[!is.na(list3)]
  list4 <- list4[!is.na(list4)]
  
  #Creem l'objecte del Venn
  list.venn<-list(list1,list2,list3, list4)
  names(list.venn)<-c(listNames[1],listNames[2],listNames[3], listNames[4])
  vtest<-Venn(list.venn)
  vennData <- sapply(vtest@IntersectionSets,function(x) length(unlist(x)))
  
  #PLOT VENN
  if(img.fmt == "png") {
    png(file.path(resultsDir,paste("VennDiagram",filename,"png",sep=".")))
  } else if (img.fmt == "pdf"){
    pdf(file.path(resultsDir,paste("VennDiagram",filename,"pdf",sep=".")))
  }
  plotVenn4d(vennData[-1], labels=c(listNames[1],listNames[2],listNames[3], listNames[4]), 
             Colors=cols, Title="", shrink=0.8)
  dev.off()
  
  
  #EXCEL
  options( java.parameters = "-Xmx4g" ) #super important abans de cridar a XLConnect
  require(XLConnect)
  xlcFreeMemory()
  
  wb <- loadWorkbook(file.path(resultsDir,paste("GeneLists",filename,"xlsx",sep=".")),create=TRUE) #no li agraden els espais al nom o noms llargs!
  createSheet(wb, name = listNames[1]) 
  writeWorksheet(wb,Table1[Table1[,ColName] %in% unlist(vtest@IntersectionSets$`1111`),], 
                 sheet = listNames[1], startRow = 1, startCol = 1, header=TRUE)
  createSheet(wb, name = listNames[2])
  writeWorksheet(wb,Table2[Table2[,ColName] %in% unlist(vtest@IntersectionSets$`1111`),], 
                 sheet = listNames[2], startRow = 1, startCol = 1, header=TRUE)
  createSheet(wb, name = listNames[3])
  writeWorksheet(wb,Table3[Table3[,ColName] %in% unlist(vtest@IntersectionSets$`1111`),], 
                 sheet = listNames[3], startRow = 1, startCol = 1, header=TRUE)
  createSheet(wb, name = listNames[4])
  writeWorksheet(wb,Table4[Table4[,ColName] %in% unlist(vtest@IntersectionSets$`1111`),], 
                 sheet = listNames[4], startRow = 1, startCol = 1, header=TRUE)
  saveWorkbook(wb)
  
}
