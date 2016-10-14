GoogleRoadDist<- function(orilat,orilon,deslat,deslon,mode='driving',apikey=NULL,departure_time=NULL,arrival_time=NULL,transit_mode=NULL,transit_routing_preference=NULL){
  requireNamespace("XML",quietly = TRUE)
  requireNamespace("RCurl",quietly = TRUE)  
  requireNamespace("stringr",quietly = TRUE)  
  
  origin<-paste0(orilat,',',orilon)
  destination<-paste0(deslat,',',deslon)
  mode<-paste0(mode)
  apikey<-paste0("&key=",as.character(apikey))
  core<-"https://maps.googleapis.com/maps/api/distancematrix/xml?origins="
  
  xml.url <- paste0(core,origin,'&destinations=',destination,'&mode=',mode)
  xmlfile <- XML::xmlTreeParse(RCurl::getURL(xml.url))
  xmltop <- XML::xmlRoot(xmlfile)
  distance <- xmltop[['row']][[1]][['distance']][['value']][[1]]
  distance <- as.numeric(unclass(distance)[['value']])
  miles <- distance*0.000621371 
  duration <- xmltop[['row']][[1]][['duration']][['text']][[1]]
  duration <- unclass(duration)[['value']]
  duration <- as.numeric(stringr::str_extract(duration,"\\d"))
  minutes<-duration
  D<-list(miles=miles,minutes=minutes)
  return(D)
}