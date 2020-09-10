Venn2Plot.csv <-
function(listG1, listG2,listNames,filename,data4T= NULL, symbols=TRUE, img.fmt = "pdf",
                      mkExcel = TRUE, mkCSV=FALSE, colnmes= c("AffyID", "Symbol")){
  #Funció per fer un venn diagram 2D
  #listG1: Data frame amb els resultats del data4Tyers del primer grup que volem comparar
  #listG2: Data frame amb els resultats del data4Tyers del segon grup que volem comparar
  #listNames: vector with the names of the lists of genes (normally contrasts)
  #data4T: Data4Tyers
  #symbols: if TRUE gene symbols are used to make the venn 
  #mkExcel: if TRUE an excel is made with the results of the venn diagram
  #colnmes: Nom de les columnes AffyID i Symbol
  
  require(Vennerable) 
  require(colorfulVennPlot) #Per generar plots amb colors diferents
  require(RColorBrewer)  
  #establim els colors per als plots
  cols <- brewer.pal(8,"Pastel2") 
  
  #Creem l'objecte del Venn
  if (symbols){
    listG1<-listG1[!is.na(listG1[,colnmes[2]]),]
    listG2<-listG2[!is.na(listG2[,colnmes[2]]),]
    list1<-listG1[,colnmes[2]]
    list2<-listG2[,colnmes[2]]
  }else{
    list1<-listG1[,colnmes[1]]
    list2<-listG2[,colnmes[1]]
  }
  
  list.venn<-list(list1,list2)
  names(list.venn)<-c(listNames[1],listNames[2])
  vtest<-Venn(list.venn)
  vennData <- sapply(vtest@IntersectionSets,function(x) length(unlist(x)))
  
  #PLOT VENN
  #els noms estan al reves al plot labels
  if(img.fmt == "png") {
    png(file.path(resultsDir,paste("VennDiagram",filename,"png",sep=".")))
  } else if (img.fmt == "pdf"){
    pdf(file.path(resultsDir,paste("VennDiagram",filename,"pdf",sep=".")))
  }
  plotVenn2d(vennData, labels=c(listNames[2],listNames[1]), Colors=cols, Title="", shrink=1)
  dev.off()
  
  if(mkExcel) {
    #EXCEL
    options( java.parameters = "-Xmx4g" ) #super important abans de cridar a XLConnect
    require(XLConnect)
    xlcFreeMemory()
    cNames <- colnames(data4T)[!colnames(data4T) %in% grep("scaled$", colnames(data4T), value=T)]
    
    if (symbols) {
      wb <- loadWorkbook(file.path(resultsDir,paste("VennGenes",filename,"xlsx",sep=".")),create=TRUE)
      createSheet(wb, name = listNames[1])
      writeWorksheet(wb,data4T[(!is.na(data4T[,colnmes[2]])) & 
                                 (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`10`)) & 
                                 (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]), cNames], 
                     sheet = listNames[1], startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = listNames[2])
      writeWorksheet(wb,data4T[(!is.na(data4T[,colnmes[2]])) & 
                                 (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`01`)) & 
                                 (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]), cNames], 
                     sheet = listNames[2], startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "Common")
      writeWorksheet(wb,data4T[(!is.na(data4T[,colnmes[2]])) & 
                                 (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`11`)) & 
                                 (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]) & (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]), 
                               cNames],
                     sheet = "Common", startRow = 1, startCol = 1, header=TRUE)
      saveWorkbook(wb)
    }else {
      
      wb <- loadWorkbook(file.path(resultsDir,paste("VennGeneLists",filename,"xlsx",sep=".")),create=TRUE)
      createSheet(wb, name = listNames[1])
      writeWorksheet(wb,data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`10`),
                               cNames], 
                     sheet = listNames[1], startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = listNames[2])
      writeWorksheet(wb,data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`01`),
                               cNames], 
                     sheet = listNames[2], startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "Common")
      writeWorksheet(wb,data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`11`),
                               cNames],
                     sheet = "Common", startRow = 1, startCol = 1, header=TRUE)
      saveWorkbook(wb)
    }
  }
  if(mkCSV){
    cNames <- colnames(data4T)[!colnames(data4T) %in% grep("scaled$", colnames(data4T), value=T)]
    
    if (symbols) {
      write.csv2(data4T[(!is.na(data4T[,colnmes[2]])) & 
                          (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`10`)) & 
                          (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]), cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,listNames[1],"csv",sep=".")),
                 row.names = FALSE)
      
      write.csv2(data4T[(!is.na(data4T[,colnmes[2]])) & 
                          (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`01`)) & 
                          (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]), cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,listNames[2],"csv",sep=".")),
                 row.names = FALSE)
      
      write.csv2(data4T[(!is.na(data4T[,colnmes[2]])) & 
                          (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`11`)) & 
                          (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]) & (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]), 
                        cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"Common","csv",sep=".")),
                 row.names = FALSE)
      
    }else {
      
      write.csv2(data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`10`),
                        cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,listNames[1],"csv",sep=".")),
                 row.names = FALSE)
      
      write.csv2(data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`01`),
                        cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,listNames[2],"csv",sep=".")),
                 row.names = FALSE)
      
      write.csv2(data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`11`),
                        cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"Common","csv",sep=".")),
                 row.names = FALSE)
    }
  }
}
