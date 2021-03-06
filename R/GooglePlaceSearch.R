GooglePlaceSearch<-function(query, apikey=NULL)
{
  requireNamespace("XML",quietly = TRUE)
  requireNamespace("RCurl",quietly = TRUE)
  query<-as.character(query)
  query<-gsub(" ","+",query)
  core<-"https://maps.googleapis.com/maps/api/place/textsearch/xml?query="
  corenext<-"https://maps.googleapis.com/maps/api/place/textsearch/xml?pagetoken="
  apikey<-paste0("&key=",as.character(apikey))
  DATA<-as.data.frame(matrix(0,1,6))
  names(DATA)<-cbind("store_name","store_type","store_addr","store_lat","store_lon","store_rating")
  p<-1
  while(p <=8){
    if(p==1){
      xml.url <-paste0(core,query,apikey)
    } else {
      xml.url <-paste0(corenext,next_page,apikey)
    }
    Sys.sleep(5)
    xmlfile<-XML::xmlTreeParse(RCurl::getURL(xml.url))
    xmltop <- XML::xmlRoot(xmlfile)
    stores<-as.numeric(table(attributes(xmltop$children)$names=="result")["TRUE"])
    n<-stores+1
    
    if(unclass(xmltop$children$status[[1]])[['value']]=="ZERO_RESULTS") stop("There are ZERO results for your query.")
    if(unclass(xmltop$children$status[[1]])[['value']]=="REQUEST_DENIED") stop("Your API Key has been denied. Check your API key validity.")
    if(unclass(xmltop$children$status[[1]])[['value']]=="INVALID_REQUEST") stop("A required parameter is missing.")
    
    
    eval(parse(text=paste0("DATA",p,"<-as.data.frame(matrix(,stores,6))")))
    eval(parse(text=paste0("names(DATA",p,")<-c('store_name', 'store_type', 'store_addr', 'store_lat', 'store_lon', 'store_rating')")))
    
    for(i in 1:stores){
      store_name<-xmltop$children[2:n][[i]][['name']][[1]]
      store_type<-xmltop$children[2:n][[i]][['type']][[1]]
      store_addr<-xmltop$children[2:n][[i]][['formatted_address']][[1]]
      store_lat<-xmltop$children[2:n][[i]][['geometry']][[1]][[1]][[1]]
      store_lon<-xmltop$children[2:n][[i]][['geometry']][[1]][[2]][[1]]
      store_rating<-xmltop$children[2:n][[i]][['rating']][[1]]
      
      store_name<-as.character(unclass(store_name)[['value']])
      store_type<-as.character(unclass(store_type)[['value']])
      store_addr <-as.character(unclass(store_addr)[['value']])
      store_lat <-as.numeric(unclass(store_lat)[['value']])
      store_lon <-as.numeric(unclass(store_lon)[['value']])
      store_rating <-as.numeric(unclass(store_rating)[['value']])
      store_rating<-ifelse(length(store_rating)==0,0,store_rating)
      
      eval(parse(text=paste0("DATA",p,"[(",i,"),]<-cbind(store_name,store_type,store_addr,store_lat,store_lon,store_rating)")))
    }
    eval(parse(text=paste0("DATA<-rbind(DATA,DATA",p,")")))
    next_page<-unclass(xmltop$children$next_page_token[[1]])[['value']]
    if(p>=1 & is.null(next_page)==TRUE){
      p<-9
    }else {
      p<-p+1  
    }
  }
  DATA<-DATA[which(DATA[,1]!=0),]
  return(DATA)
  Todelete<-as.character(ls()[grepl("DATA",ls())])
  rm(Todelete)
}