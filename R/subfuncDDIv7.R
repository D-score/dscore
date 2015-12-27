##process input from the Dutch Developmental Instrument (Van Wiechen) 
##v7 is equal to v6; March 10, 2015

readDDI <-function(path="choose"){ 
# path can be specified, else files can be chosen
# files are .xls files of the Dutch Development Instrument  (DDI)
	if(path=="choose"){
  path <- choose.files(caption = "Select one or more *.xls files with the DDI")
  }
 	Sheetlist <- vector("list")
  for (i in 1:length(path)){
  cat("Reading workbook ", i, "\n")
	wb <- loadWorkbook(path[i])  
  sheets <- getSheets(wb)
	n <- length(sheets)         # n sheets per workbook
	cat ("Workbook ", i, "has ", n-1, "sheets", "\n")
	workbook <- vector("list")
	for (j in 2:n){ #omit the first sheet with the dropdown menu
	cat("Reading worksheet ",j-1,"\n") 
	workbook[[j-1]] <- read.xlsx(path[i],j,header=FALSE)
  }
	Sheetlist <- c(Sheetlist,workbook)
  }
	# A list with has been created from the XLS files. 
	class(Sheetlist) <- "sheetlist"
return(Sheetlist)}

#cleanDDI nog checken voor 2005
cleanDDI <-
function(Sheetlist, version="1996",finalvisit=13,maxitems=75){
	stopifnot(class(Sheetlist)=="sheetlist")
	# imputefromweeks: impute age from weeks if no date of measurement is given but instead weeks are filled in.
	sheets2 <- Sheetlist
	ncolumn<-finalvisit+2 #first two columns are variable number and name
	rownum<-maxitems+8 #maximum number of rows of excel input are 75 items+first 7 rows + 1 between row (row 46)
	#= 83 rows.
  itemvec<-8:rownum
	nmis<-sum(is.na(sheets2[[1]][8:rownum,1]))
		itemvec<- 8:rownum
	if (nmis!=0){
	itemmis<-which(is.na(sheets2[[1]][8:rownum,1] ))
  itemvec<-itemvec[-itemmis]}
	itemnames <- paste("v",sheets2[[1]][itemvec,1],sep="")  #bij versie 2005 geeft dit 2 lege rijen met vNA!!
	numitems <- length(itemnames)
	sheets3 <- vector("list", length=length(sheets2))
	for (i in 1:length(sheets3)){sheets3[[i]] <- matrix(ncol=ncolumn,  nrow=numitems+5,   
    dimnames=list(c(itemnames,"meetdag", "gebdag", "Leeftijd", "pnr", "version"),c()))}
	for (i in 1:length(sheets3)){
	sheets3[[i]][(numitems+2),1] <- as.character(sheets2[[i]][2,3])  #gebdag
	sheets3[[i]][numitems+4,1] <-   as.character(sheets2[[i]][1,3]) #pnr
	sheets3[[i]][numitems+5,1] <- version #version of DDI
  }
	for (i in 1:length(sheets3)){
	 for (j in 1:finalvisit){
	# j+2 third column is first occassion
	sheets3[[i]][c(1:numitems),j] <- as.character(sheets2[[i]][itemvec,j+2])
	sheets3[[i]][numitems+1,j] <- as.character(sheets2[[i]][4,j+2])
	if(!is.na( sheets2[[i]][4,j+2])){
	sheets3[[i]][numitems+3,j] <- as.numeric(as.character(sheets2[[i]][4,j+2])) - as.numeric(as.character(sheets2[[i]][2,3])) }
	else {sheets3[[i]][numitems+3,j] <- round(as.numeric(as.character(sheets2[[i]][5,j+2]))*7)}
    }
  }
	cleanoutput <- list(sheets3, version=version, itemnames=itemnames, numitems=numitems)
	class(cleanoutput) <- "cleanoutput"
return(cleanoutput)}

# sheets are transformed from +,-,M to 0(fail) and 1(pass)
#Scenario 1 (used for Dscore)
string2valueLR <- function(string){ #transformation of LR items
  value <- switch(string,
                  "+|+" = 1,
                  "M|M" = 1,
                  "-|-" = 0,
                  "+|-" = 0,
                  "-|+" = 0,
                  "M|+" = 1,
                  "+|M" = 1,
                  "M|-" = 0,
                  "-|M" = 0,
                  "+| " = 0,
                  "+|"  = 0,
                  " |+" = 0,
                  "|+"  = 0,
                  "-| " = 0,
                  "-|"  = 0,
                  " |-" = 0,
                  "|-"  = 0,
                  "M| " = 0,
                  "M|"  = 0,
                  " |M" = 0,
                  "|M"  = 0,
                  "++"  = 1,
                  "--"  = 0,
                  "MM"  = 1,
                  "+"   = 0,
                  "-"   = 0,
                  "M"   = 0,
                  "m|m" = 1,
                  "m|+" = 1,
                  "+|m" = 1,
                  "m|-" = 0,
                  "-|m" = 0,
                  "m| " = 0,
                  "m|"  = 0,
                  " |m" = 0,
                  "|m"  = 0,
                  "mm"  = 1,
                  "m"   = 0,                  
                  "+-"  = 0,
                  "-+"  = 0,
                  "m "  = 0,
                  "M+" = 1,
                  "M-" = 0,
                  "+-" = 0,
                  "M " = 0,
                  "++ " = 1);
  return(value);}

string2value <- function(string,M=1){
  if(M==1){
    value <- switch(string,
                    "+"  = 1,
                    "M"  = 1,
                    "-"  = 0,
                    "++" = 1,
                    "--" = 0,
                    " -"=0,
                    "MM" = 1,
                    "m"  = 1,
                    "mm" = 1,
                    "m " = 1,
                    "M " = 1,
                    "M+" = 1    )}
    if(M==2){
    value <- switch(string,
                    "+"  = 1,
                    "M"  = 2,
                    "-"  = 0,
                    "++" = 1,
                    "--" = 0,
                    " -"=0,
                    "MM" = 2,
                    "m"  = 2,
                    "mm" = 2,
                    "m " = 2,
                    "M " = 2,
                    "M+" = 2    )}
    return(value)}



transformDDI <-
function(cleanoutput, itembank=itembank,M=1) {
	stopifnot(class(cleanoutput)=="cleanoutput")
	csheetlist <- cleanoutput[[1]]
	version <- cleanoutput[[2]]
	itemnames <- cleanoutput[[3]]
	numitems <- cleanoutput[[4]]
	# sheets are transformed from +,-,M to 0(fail) and 1(pass)
	LRitems <- itembank[,paste("ID.VWO",version,sep="")][itembank[,"LR"]==1]
	LRitems <- itemnames[match(LRitems,itemnames)]
	LRitems <- as.character(LRitems[!is.na(LRitems)])
	Sitems <- itembank[,paste("ID.VWO",version,sep="")][itembank[,"LR"]==0]
	Sitems <- itemnames[match(Sitems,itemnames)]
	Sitems <- as.character(Sitems[!is.na(Sitems)])

	for(i in 1:length(csheetlist)){ 
	  cat("Transformation of sheet",i,"\n")
		for (k in 1:dim(csheetlist[[i]])[2]){
		  cat("Transformation of column",k,"\n")
		  for(j in 1:length(Sitems)){
		    #eerst omzetten van Single items
        cat("item",Sitems[j],"\n")
		  if(csheetlist[[i]][Sitems[j],k]!="" & !is.na(csheetlist[[i]][Sitems[j],k]) & is.null(string2value(csheetlist[[i]][Sitems[j],k],M=M))==TRUE)
        {
		  warnstring <- NULL
		  warnstring <- paste(c("The Value ",csheetlist[[i]][Sitems[j],k], " (item ", substr(Sitems[j],2,100),", occassion ", k, ", sheet ", i, ", pnr ",  csheetlist[[i]][numitems+4,1]
		  ,") is not valid and has been dropped."), sep="")
		  warning(warnstring,immediate. = TRUE)
		  csheetlist[[i]][Sitems[j],k]<-""
		    }
     else if(is.null(string2value(csheetlist[[i]][Sitems[j],k],M=M))==TRUE){
       csheetlist[[i]][Sitems[j],k]<-""
     }
		 else{
       csheetlist[[i]][Sitems[j],k]<-string2value(csheetlist[[i]][Sitems[j],k],M=M)
		    }
		  }
		for(j in 1:length(LRitems)){
        #omzetten van LR items
		  cat("item",LRitems[j],"\n")
		if(csheetlist[[i]][LRitems[j],k]!="" &!is.na(csheetlist[[i]][LRitems[j],k]) & is.null(string2valueLR(csheetlist[[i]][LRitems[j],k]))==T){
		warnstring <- NULL
		warnstring <- paste(c("The Value ",csheetlist[[i]][LRitems[j],k], " (item ",  substr(LRitems[j],2,100),", occassion ", k, ", sheet ", i, ", pnr ",  csheetlist[[i]][numitems+4,1]
		,") is not valid and has been dropped."), sep="")
		warning(warnstring,immediate. = TRUE)
		csheetlist[[i]][LRitems[j],k]<-""}
		else if(is.null(string2valueLR(csheetlist[[i]][LRitems[j],k]))==TRUE)
      {
		  csheetlist[[i]][LRitems[j],k]<-""
		  }
		else{
      csheetlist[[i]][LRitems[j],k]<-string2valueLR(csheetlist[[i]][LRitems[j],k])
		  }
		}
	}	#end of k loop
} #end of i loop
	transformoutput <- list(csheetlist, version,itemnames, numitems)
	class(transformoutput) <- "transformoutput"
return(transformoutput)}


compactDDI <-
function(transformoutput, impute=FALSE, finalvisit=9, itembank=itembank){
	#stopifnot(class(transformoutput)=="transformoutput")
	#if impute=true: typical items are imputed: a plus on previous occasion is imputed at typical occasion
	#and a minus at next occasion is imputed at typical occasion
	csheetlist <- transformoutput[[1]]
	version <- transformoutput[[2]]
	itemnames <-  transformoutput[[3]]
	numitems <- length(itemnames)
	AUTlist <- csheetlist
# for the imputation one occassion more than specified in "finalvisit" is needed. therefore the function must know wether 
# one more occassion is observed.
observed <- dim(AUTlist[[1]])[1] #first dimension is number of visits
{if(observed==finalvisit){maxocc<- finalvisit} else {maxocc<- finalvisit+1}}
	 #format veranderen van excelpagina naar input format voor Dscore
	  for (i in 1:length(AUTlist)){
	 #maak eerst een matrix met rijen=itemnamen en kolommen is aantal visits+1 (ivm imputatie en mogelijkheid tot doorscoren)
    AUTlist[[i]] <- AUTlist[[i]][1:numitems,1:maxocc]
    }
    for (i in 1:length(AUTlist)){
    #transponeren van de matrix:
    AUTlist[[i]]<-matrix(as.double(t(AUTlist[[i]])),nrow=maxocc,ncol=length(itemnames))
    colnames(AUTlist[[i]]) <- itemnames
    }
    occ<-c(1:maxocc)
    list1 <- vector("list",length=length(csheetlist))
    for (i in 1:length(AUTlist)) {
    #Voeg pnr occ en dag toe
    pnr<-ifelse(!is.na(csheetlist[[i]][numitems+4,1]),csheetlist[[i]][numitems+4,1],0)
    pnr<-as.double(rep(pnr, maxocc))
    list1[[i]] <- as.matrix(cbind(pnr, occ=occ, dag=csheetlist[[i]][numitems+3,occ],AUTlist[[i]] ))
    }
    for (i in 1:length(list1)){
    list1[[i]] <- data.frame(matrix(as.numeric(list1[[i]]), nrow=maxocc))}
    for (i in 1:length(list1)){ colnames(list1[[i]]) <-  c("pnr", "occ", "dag", itemnames)}	
    selectie <- vector(length=length(list1))
    for (i in 1:length(list1)){
	selectie[i] <-any(!is.na(list1[[i]][,-c(1:3)]))
	}
    list1 <- list1[selectie]
	# Impute could be done as a separate function.#
	# but if no extra occassions are used to estimate the dscore, it is still possible to impute a missing dscore from another occassion.
	# therefore the imputation has to take place before extra occassions are deleted. Therefore the deletion of the extra occassion would have to be a separate funtion too.
	# or the dscore funtion would have to take into account extra occassions/no extraoccassion.
	# Only items that are typically measured up to the visit given in the "finalvisit" argument are returned. one extra row is taken for imputation.
	itemoccured <- c(TRUE,TRUE,TRUE)  #de eerste 3 kolommen komen voor
    for (i in 4: dim(list1[[1]])[2]){
       itemoccured[i] <-  itembank[which(itembank[,paste("ID.VWO",version,sep="")]==colnames(list1[[1]])[i]),"occ"]<=maxocc
     }
	for (i in 1:length(list1)){list1[[i]] <- list1[[i]][,itemoccured]  #selecteer alleen de kolommen die corresponderen met itemnamen van de DDI versie 
  }
    if(impute==TRUE){ 
			# If an occassion is entirely skipped, (18 months is uncommon), only the Fails are imputed from the next visit(24 months).
		#  singleocc is a dataframe that has the value "FALSE" on the typical moment an item is measured at. Which is provided by the itembank in the column "occ"
				singleocc<- list1[[1]]
		singleocc[,1:3] <- FALSE
		singleocc[,4:dim(list1[[1]])[2]] <- TRUE
		for (i in 4:dim(list1[[1]])[2]){singleocc[itembank[which(itembank[,paste("ID.VWO",version,sep="")]==names(list1[[1]])[i]),"occ"],names(list1[[1]])[i]] <-FALSE
    }
		## voor 1996 geldt: items 51-54 may not be imputed because their meaning differs at each occasion 
				if(version=="1996"){
		singleocc[, as.character(itembank[,paste("ID.VWO",version,sep="")][51:54]) ] <- TRUE
				}
				#also 52-55 en item 68 for DDI version 2005
    if(version=="2005"){
      singleocc[ ,as.character(itembank[,paste("ID.VWO",version,sep="")][c(51:54,67)]) ] <- TRUE
      
    }
		# seperate dataframes are made for imputing 0 and 1. 
		# for the last and first occassions that are recorded, it is not possible to impute from the following(last) or the preceding(first)
		# occassion. therefore those occassions are set to "TRUE"
		singleoccmin <- singleocc
		singleoccmin[maxocc,-c(1:3)] <-TRUE
		singleoccplus  <- singleocc
		singleoccplus[1,-c(1:3)] <- TRUE
		###imputation of fails###
		for (i in 1:length(list1)){
		for (j in 4:ncol(singleocc)){
		#is there a typical occassion that should be imputed?
		if (any(singleoccmin[,j]==FALSE)){
		#is the value missing?
		if (is.na(list1[[i]][as.numeric(which(singleoccmin[,j]==FALSE)),j])==TRUE){
		# following missing?
		if(!is.na(list1[[i]][as.numeric(which(singleoccmin[,j]==FALSE))+1,j])){
		# is the preceding either NA or 0 ?
		if(any((is.na(list1[[i]][as.numeric(which(singleoccmin[,j]==FALSE))-1,j])),(list1[[i]][as.numeric(which(singleoccmin[,j]==FALSE))-1,j]==0))){# 
		# is the following occassion 0 ?
		if (list1[[i]][as.numeric(which(singleoccmin[,j]==FALSE))+1,j]==0){list1[[i]][as.numeric(which(singleoccmin[,j]==FALSE)),j]<- 0}}}
		}}}}
		###imputation of passes###
		for (i in 1:length(list1)){
		for (j in 4:ncol(singleocc)){
		#is there a typical occassion that should be imputed?
		if (any(singleoccplus[,j]==FALSE)){
		#is the value missing?
		if (is.na(list1[[i]][as.numeric(which(singleoccplus[,j]==FALSE)),j])==TRUE){
		# previous missing?
		if(!is.na(list1[[i]][as.numeric(which(singleoccplus[,j]==FALSE))-1,j])){
		# is the following either NA or 1 ?
		if(any((is.na(list1[[i]][as.numeric(which(singleoccplus[,j]==FALSE))+1,j])),(list1[[i]][as.numeric(which(singleoccplus[,j]==FALSE))+1,j]==1))){# 
		# is the preceding occassion 1 ?
		if (list1[[i]][as.numeric(which(singleoccplus[,j]==FALSE))-1,j]==1){list1[[i]][as.numeric(which(singleoccplus[,j]==FALSE)),j]<- 1}
		# is the preceding occassion 2 ?
		if (list1[[i]][as.numeric(which(singleoccplus[,j]==FALSE))-1,j]==2){list1[[i]][as.numeric(which(singleoccplus[,j]==FALSE)),j]<- 2}
		
		}}}}}}
	}
	    
	###### add age
	pnrcsheetlist <- vector() 
	for(i in 1:length(csheetlist)){pnrcsheetlist[i] <- csheetlist[[i]]["pnr",1]}
	for (i in 1:length(list1)){
	for (j in 1:finalvisit){
	n <- match(list1[[i]][1,1],pnrcsheetlist) 
	list1[[i]][j,"dag"] <- as.numeric(csheetlist[[n]]["Leeftijd",j])
	if(!is.na( csheetlist[[n]]["Leeftijd",j])){
	if( as.numeric(csheetlist[[n]]["Leeftijd",j])<=0){
	list1[[i]][j,"dag"]<-NA
  }  }
    }
    list1[[i]]<-list1[[i]][1:finalvisit,]#omit rows that are not applicable
  }
	class(list1) <-"VWitems"
  return(list1)
}







