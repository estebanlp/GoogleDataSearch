googleDis<- function(orilat,orilot,deslat,deslot,mode){
  require(XML)
  
  origin<-paste0(orilat,',',orilot)
  destination<-paste0(deslat,',',deslot)
  mode<-paste0(mode)
  
  xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=',mode,'&sensor=false')
  
  xmlfile <- xmlTreeParse(xml.url)
  xmltop = xmlRoot(xmlfile)
  distance <- xmltop[['row']][[1]][['distance']][['value']][[1]]
  distance <- as.numeric(unclass(distance)[['value']])
  mts <- distance 
  duration <- xmltop[['row']][[1]][['duration']][['text']][[1]]
  duration <- unclass(duration)[['value']]
  min<-duration
  return(list(mts,min))
}
