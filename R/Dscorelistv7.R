#v7, print of Dscore is disabled; 10/3/2015

getDscorelist <- function(compactoutput, version="1996", format="wide", ref=ref, itembank=itembank)
  {
	##input is compactoutput object from getItemlist
	listD <- vector("list", length=length(compactoutput))
	for(i in 1:length(compactoutput) ){
	print(i)
  #print(compactoutput[[i]])
 	listD[[i]] <- Dscore(VWOdata=compactoutput[[i]], year=version,
                        itembank=itembank, ref)

  }
	###
	if (format=="list")
  {out<-listD}
	else{
	out <- listD[[1]]
	if(length(listD)>1){
	for(i in 2:length(listD)){
	out <- rbind(out,listD[[i]])}}
  }
  
	rownames(out) <- NULL
	names(out)<-c("ID","Visit","Age", "Dscore")

	
	if(format=="wide"){ 
	occ <- sort(unique(out[,"Visit"]))
	
	
	out <- reshape(out,direction="wide",idvar="ID", timevar="Visit")
	
	AGE <- paste("Age",occ,sep=".")
	DSCORE <- paste("Dscore",occ,sep=".")
	out2 <- out[,"ID"]
	
	for(i in 1:length(occ)){
	out2 <- cbind(out2,out[match(DSCORE[i],names(out))],out[match(AGE[i],names(out))])}
	names(out2)[names(out2)=="out2"]<- "ID"
	
	AGE2 <- paste("Age",occ,sep="")
	DSCORE2 <- paste("Dscore",occ,sep="")
	
	newnames <- "ID"
	for(i in 1:length(occ)){
	newnames <- c(newnames,DSCORE2[i],AGE2[i])}
	
	names(out2) <- newnames
	rownames(out2) <- NULL
	out <- out2 }
  return(out)
  }  


























































