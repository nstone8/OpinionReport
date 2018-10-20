library(jsonlite)
nullToNANestedList <- function(l){
    for(i in 1:length(l)){
        if(is.list(l[[i]])){
            l[[i]] <- nullToNANestedList(l[[i]])
        }else if(is.null(l[[i]])){
            l[[i]] <- NA
        }
    }
    return(l)
}

getOneIP <- function(address){
    print("address:")
    print(address)
    api <- "http://api.ipstack.com/"
    key <- "78317398918010144a693d85d28e980d"
    return(data.frame(as.list(unlist(nullToNANestedList(fromJSON(readLines(paste0(api,address,"?access_key=",key),warn=FALSE)))))))
}
ipLookup <- function(ip){
    ipFrame <- getOneIP(ip[1])
    if(length(ip)>1){
        for(address in ip[-1]){
            jsonData <- getOneIP(address)
            ipFrame <- rbind(ipFrame,jsonData)
                }
    }
    return(ipFrame)
}
