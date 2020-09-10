Venn3Plot.csv <-
function(listG1, listG2, listG3, listNames,filename ,data4T= NULL, symbols=TRUE, img.fmt = "pdf",
                      mkExcel = TRUE, mkCSV=FALSE, colnmes= c("AffyID", "Symbol")){
  #Funció per fer un venn diagram 3D
  #listG1: Data frame amb els resultats del data4Tyers del primer grup que volem comparar
  #listG2: Data frame amb els resultats del data4Tyers del segon grup que volem comparar
  #listG3: Data frame amb els resultats del data4Tyers del tercer grup que volem comparar
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
    listG3<-listG3[!is.na(listG3[,colnmes[2]]),]
    list1<-listG1[,colnmes[2]]
    list2<-listG2[,colnmes[2]]
    list3<-listG3[,colnmes[2]]
  }else{
    list1<-listG1[,colnmes[1]]
    list2<-listG2[,colnmes[1]]
    list3<-listG3[,colnmes[1]]
  }
  
  list.venn<-list(list1,list2,list3)
  names(list.venn)<-c(listNames[1],listNames[2],listNames[3])
  vtest<-Venn(list.venn)
  vennData <- sapply(vtest@IntersectionSets,function(x) length(unlist(x)))
  
  #PLOT VENN
   if(img.fmt == "png") {
    png(file.path(resultsDir,paste("VennDiagram",filename,"png",sep=".")))
  } else if (img.fmt == "pdf"){
    pdf(file.path(resultsDir,paste("VennDiagram",filename,"pdf",sep=".")))
  }
  plotVenn3d(vennData, labels=c(listNames[1],listNames[2],listNames[3]), 
             Colors=cols, Title="", shrink=1)
  dev.off()
  
  if(mkExcel){
    #EXCEL
    options( java.parameters = "-Xmx4g" ) #super important abans de cridar a XLConnect
    require(XLConnect)
    xlcFreeMemory()
    colnames(data4T)[!colnames(data4T) %in% grep("scaled$", colnames(data4T), value=T)]
    
    if(symbols){
      wb <- loadWorkbook(file.path(resultsDir,paste("GeneLists",filename,".xlsx",sep=".")),create=TRUE) #no li agraden els espais al nom o noms llargs!
      createSheet(wb, name = "orange") 
      writeWorksheet(wb,data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`100`)) & 
                                     (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]),
                                     cNames], 
                     sheet = "orange", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "blue")
      writeWorksheet(wb,data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`010`)) & 
                                     (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]),
                                     cNames], 
                     sheet = "blue", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "green")
      writeWorksheet(wb,data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`001`)) & 
                                     (data4T[,colnmes[1]] %in% listG3[,colnmes[1]]),
                                     cNames], 
                     sheet = "green", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "pink")
      writeWorksheet(wb,data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`110`)) & 
                                     (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]) &
                                     (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]),
                                     cNames], 
                     sheet = "pink", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "brown")
      writeWorksheet(wb,data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`011`)) & 
                                     (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]) &
                                     (data4T[,colnmes[1]] %in% listG3[,colnmes[1]]),
                                     cNames], 
                     sheet = "brown", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "yellow")
      writeWorksheet(wb,data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`101`)) & 
                                     (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]) &
                                     (data4T[,colnmes[1]] %in% listG3[,colnmes[1]]),
                                     cNames], 
                     sheet = "yellow", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "Common grey")
      writeWorksheet(wb,data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`111`)) &
                                     (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]) &
                                     (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]) &
                                     (data4T[,colnmes[1]] %in% listG3[,colnmes[1]]),cNames], 
                     sheet = "Common grey", startRow = 1, startCol = 1, header=TRUE)
      saveWorkbook(wb)
    
    }else{
      wb <- loadWorkbook(file.path(resultsDir,
                                   paste("GeneLists",filename,".xlsx",sep=".")),
                         create=TRUE) #no li agraden els espais al nom o noms llargs!
      
      createSheet(wb, name = "orange") 
      writeWorksheet(wb,data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`100`),
                               cNames], 
                     sheet = "orange", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "blue")
      writeWorksheet(wb,data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`010`),
                               cNames], 
                     sheet = "blue", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "green")
      writeWorksheet(wb,data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`001`),
                               cNames], 
                     sheet = "green", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "pink")
      writeWorksheet(wb,data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`110`),
                               cNames], 
                     sheet = "pink", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "brown")
      writeWorksheet(wb,data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`011`),
                               cNames], 
                     sheet = "brown", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "yellow")
      writeWorksheet(wb,data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`101`),
                               cNames], 
                     sheet = "yellow", startRow = 1, startCol = 1, header=TRUE)
      createSheet(wb, name = "Common grey")
      writeWorksheet(wb,data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`111`),
                               cNames], 
                     sheet = "Common grey", startRow = 1, startCol = 1, header=TRUE)
      saveWorkbook(wb)
      
    }
  }
  if(mkCSV){
    colnames(data4T)[!colnames(data4T) %in% grep("scaled$", colnames(data4T), value=T)]
    
    if(symbols){
      
      write.csv2(data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`100`)) & 
                          (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]), cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"orange","csv",sep=".")),
                 row.names = FALSE)
      write.csv2(data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`010`)) & 
                          (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]),cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"blue","csv",sep=".")),
                 row.names = FALSE)
      write.csv2(data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`001`)) & 
                          (data4T[,colnmes[1]] %in% listG3[,colnmes[1]]), cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"green","csv",sep=".")),
                 row.names = FALSE)
      write.csv2(data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`110`)) & 
                          (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]) &
                          (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]), cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"pink","csv",sep=".")),
                 row.names = FALSE)
      write.csv2(data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`011`)) & 
                          (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]) &
                          (data4T[,colnmes[1]] %in% listG3[,colnmes[1]]), cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"brown","csv",sep=".")),
                 row.names = FALSE)
      
      write.csv2(data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`101`)) & 
                          (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]) &
                          (data4T[,colnmes[1]] %in% listG3[,colnmes[1]]), cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"yellow","csv",sep=".")),
                 row.names = FALSE)
      write.csv2(data4T[!is.na(data4T[,colnmes[2]]) & (data4T[,colnmes[2]] %in% unlist(vtest@IntersectionSets$`111`)) &
                          (data4T[,colnmes[1]] %in% listG1[,colnmes[1]]) &
                          (data4T[,colnmes[1]] %in% listG2[,colnmes[1]]) &
                          (data4T[,colnmes[1]] %in% listG3[,colnmes[1]]),cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"CommonGrey","csv",sep=".")),
                 row.names = FALSE)
      
    }else{
      
      write.csv2(data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`100`),
                        cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"orange","csv",sep=".")),
                 row.names = FALSE)
      write.csv2(data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`010`),
                        cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"blue","csv",sep=".")),
                 row.names = FALSE)
      write.csv2(data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`001`),
                        cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"green","csv",sep=".")),
                 row.names = FALSE)
      write.csv2(data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`110`),
                        cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"pink","csv",sep=".")),
                 row.names = FALSE)
      write.csv2(data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`011`),
                        cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"brown","csv",sep=".")),
                 row.names = FALSE)
      write.csv2(data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`101`),
                        cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"yellow","csv",sep=".")),
                 row.names = FALSE)
      write.csv2(data4T[data4T[,colnmes[1]] %in% unlist(vtest@IntersectionSets$`111`),
                        cNames],
                 file= file.path(resultsDir,paste("VennGenes",filename,"CommonGrey","csv",sep=".")),
                 row.names = FALSE)
    }
  }
}
