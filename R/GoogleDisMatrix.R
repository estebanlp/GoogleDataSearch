GoogleDisMatrix<- function(orilat,orilon,deslat,deslon,mode='driving',apikey=NULL,departure_time=NULL,arrival_time=NULL,transit_mode=NULL,transit_routing_preference=NULL){
  requireNamespace("XML",quietly = TRUE)
  requireNamespace("RCurl",quietly = TRUE)  
  
  origin<-paste0(orilat,',',orilon)
  destination<-paste0(deslat,',',deslon)
  mode<-paste0(mode)
  
  xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=',mode,'&sensor=false')
  
  xmlfile <- XML::xmlTreeParse(xml.url)
  xmltop <- XML::xmlRoot(xmlfile)
  distance <- xmltop[['row']][[1]][['distance']][['value']][[1]]
  distance <- as.numeric(unclass(distance)[['value']])
  mts <- distance 
  duration <- xmltop[['row']][[1]][['duration']][['text']][[1]]
  duration <- unclass(duration)[['value']]
  min<-duration
  return(list(mts,min))
}
