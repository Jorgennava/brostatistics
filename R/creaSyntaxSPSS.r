#'Función para crear un syntax de spss desde un datamap generado por soda
#'
#'Esta función crea un syntax de spss a partir de un DataMap de Soda
#'@param sPath El archivo descargado de soda, un "dataMap"
#'@param sFile El nombre del archivo a generar, recordar usar la terminación ".sps"
#'@export
#'@keywords spss

creaSyntaxSPSS<- function(sPath, SFile){
  # sPath <- "~/Descargas/U&A Seguros V2_DataMap_22-02-2017 15_59_05.xlsx"
  # sFile <- "~/Descargas/magic.sps"

  syntax <- NULL
  # Primera parte: El etiquetado de values
  sDatos <- readxl::read_excel(sPath,col_names = T)
  sDatos <- sDatos[!is.na(sDatos$Label),]
  cadena <- sDatos$Label
  cadena <- gsub("<.*?>"," ",cadena)
  cadena <- gsub("\\[.*?\\]"," ",cadena)
  cadena <- strtrim(cadena,256)
  sDatos$syntax <- paste0("VARIABLE LABELS ",sDatos$Variable," '",cadena,"'.")
  syntax <- sDatos$syntax
  
  # Segunda parte: El value labels 
  sDatos <- readxl::read_excel(sPath,sheet = "Value Labels",col_names = T)
  variables <- unique(sDatos$Variable)
  
  pb <- txtProgressBar(min = 0, max = length(variables), style = 3)
  for(i in 1:length(variables)){
    # i <- 1
    setTxtProgressBar(pb, i)
    variablemini <- variables[i]
    sDatosmini <- sDatos[sDatos$Variable %in% variablemini,]
    sDatosmini$syntax <- paste0(sDatosmini$Value," '",sDatosmini$`Value Label`,"' ")
    syntax <- c(syntax,"VALUE LABELS",variablemini,sDatosmini$syntax,".")
  }
  close(pb)
  syntax <- c(syntax,"EXECUTE.")
  write.table(syntax,file = sFile,quote = F,row.names = F,col.names = F,fileEncoding = "latin1")
  cat(paste("\nSyntax creado en :",sFile))
}
 

