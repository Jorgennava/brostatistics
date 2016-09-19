#'Realiza prueba de proporciones sobre un data frame (una tabla) que sea un recuento o porcentaje de frecuencia
#'
#'Realiza un prop.test con hipótesis alternativa "mayor qué" sobre un data frame tal que sea un resultado de frecuencias.
#'La primera columna de la tabla (tabla[,1]) debe ser de las respuestas de la frecuencia
#'La última fila del data frame debe ser el "Total" (de ahí obtiene n el prop.test)
#'@param tablaProp La tabla principal
#'@param simboloPct Los datos tienen simbolo de porcentaje?  A veces al leer un .csv viene el símbolo de porcentaje
#'@export
#'@keywords prop
#'@examples pruebaProp(data.frame(nombres=c("Uno","Dos"),Variable1=c(48,100),Variable2=c(16,100)))

pruebaProp <- function(
  # La tabla de datos....
  # La primera columna es la respuesta
  # La última fila debe ser la del total
  tablaProp,
  simboloPct=F
){

  # consigueProp(tablaProp = listado[[i]],simboloPct = T)
  #
  # tablaProp <- testin
  # simboloPct <- F

  ################################################# Supuestos...

  # Mi primer columna, es texto
  if(simboloPct){
    final <- as.data.frame(sapply(tablaProp[2:length(tablaProp)], function(x){strsplit(x,split = "%")}),stringsAsFactors = F)
    final <- as.data.frame(sapply(final[1:length(final)], function(x){as.numeric(as.character(x))}),stringsAsFactors = F)
  }else{
    final <- as.data.frame(sapply(tablaProp[2:length(tablaProp)], function(x){as.numeric(as.character(x))}))
  }

  tablaSPMirror <- final

  for(spi in 1:(nrow(final)-1)){
    # Voy por el primer row
    # spi<-2
    tablaSPMirror[spi,] <- ""
    for(spt in 1:length(final)){
      # Voy por cada columna...
      # spt<-3
      # for(spw in (1:length(final))[!1:length(final) %in% spt]){
      for(spw in 1:length(final)){
        # spw<-1
        #Sólo cuando estoy evaluando diferentes columnas
        if(spt!=spw){
          objetivo<-final[spi,spt]
          objetivoTotal<-final[nrow(final),spt]
          competidor<-final[spi,spw]
          competidorTotal<-final[nrow(final),spw]
          if(objetivo>0 & competidor>0 & objetivo!=objetivoTotal & competidor != competidorTotal){
            if(prop.test(
              # Exitos
              x = c(
                objetivo, competidor
              ),
              n = c(
                objetivoTotal,competidorTotal
              ),
              alternative = "greater",
              correct = T
            )$p.value<0.05){
              tablaSPMirror[spi, spt]<-paste(tablaSPMirror[spi, spt]," ",LETTERS[spw]," ",sep="")
            }else{
              tablaSPMirror[spi, spt]<-paste(tablaSPMirror[spi, spt],"",sep="")
            }
          }
        }
      }
    }
  }
  names(tablaSPMirror) <- paste(names(tablaSPMirror),"(",LETTERS[1:length(tablaSPMirror)],")",sep = "")

  rowNombres <- tablaProp[,1]
  nombreN <- names(tablaProp)[1]

  tablaProp <- cbind(tablaProp[,1],tablaSPMirror)
  tablaProp[,1] <- rowNombres
  names(tablaProp)[1] <- nombreN
  return(tablaProp)
}
