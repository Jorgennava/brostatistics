#'Genera una tabla (base de datosgs de encuesta) una vez que se le pasa un .csv y un "catálogo" (en excel) que se hayan generado en "soda
#'@param xfile El .csv descargado de soda
#'@param yfile El excel de catálogo descargado desde soda
#'@export
#'@keywords soda
#'@examples 
#'datosgs <- gaseosa(xfile = "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS 13-09-2016 18-23-09.csv",
#'yfile1 = "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS_DataMap_13-09-2016 18_24_04.xlsx")

gaseosa <- function(xfile, yfile){
  # Donde
  # xfile Es el .csv
  # yfile Es el excel que baja de soda
  # xfile <- "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS 13-09-2016 18-23-09.csv"
  # yfile <- "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS_DataMap_13-09-2016 18_24_04.xlsx"
  # 
  #########################
  # Previos...
  
  # La base no se puede descargar en spss, voy a etiquetar a mano...
  
  datosgs <- read.csv(
    xfile,stringsAsFactors = F
  )
  etiquetasPregunta <- readxl::read_excel(path = yfile,sheet = 1)
  etiquetasVariable <- readxl::read_excel(path = yfile,sheet = 2)
  
  for(i in 1:length(datosgs)){
    # i <- 37
    miDato <- names(datosgs)[i]
    # Uso subset porque quiero respetar la estructura de mis datosgs i.e. un data frame
    subdatosgs <- subset(datosgs, select = miDato)
    misEtiquetas <- etiquetasPregunta[etiquetasPregunta$Variable==miDato,]
    misEtiquetasVariable <- etiquetasVariable[etiquetasVariable$Variable==miDato,]
    
    if(nrow(misEtiquetas)>0){
      subdatosgs[,1] <- set_label(subdatosgs[,1],misEtiquetas$Label)
    }
    if(nrow(misEtiquetasVariable)>0){
      miVector <- as.character(unlist(misEtiquetasVariable[,3]))
      names(miVector) <- as.numeric(unlist(misEtiquetasVariable[,2]))
      
      subdatosgs[,1] <- factor(x = subdatosgs[,1],levels = as.numeric(unlist(misEtiquetasVariable[,2])),labels = as.character(unlist(misEtiquetasVariable[,3])))
    }
    datosgs[,i] <- subdatosgs
  }
  return(datosgs)
  ##########################

}

