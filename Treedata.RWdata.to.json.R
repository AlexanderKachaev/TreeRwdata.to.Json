#
# Combine two csv files and convert to json format
# Form a title for each tree: tree age, diameter, etc.
# and rows of tree rings
# Alex Kachaev 2020.09.18
#
library("rjson")
# setwd("directory containing source data")
rwl<-read.table(file ="RWdata.txt", sep=";", dec=",", header = TRUE)
Tr<-read.table(file ="Treedata.txt", sep=";", dec=",", header = TRUE)	
yrwl<-rwl[,1]
df.rwl<-data.frame(rwl[,-1])
rownames(df.rwl)<-yrwl
nyear<-rownames(df.rwl)
ntree<-colnames(df.rwl)
ddf<-dim(df.rwl)
ltree<-ddf[2]
#str(df.rwl)
str.json<-c("{\"ar\":[")
end<-","
for(i in 1:ltree){
	sr<-na.omit( df.rwl[,i])
	# returns a vector containing indices where not NA
	srf<-which(is.na(df.rwl[,i])==FALSE) 
	lsr<-length(sr)		   
	DBH.t<-sum(sr)/5
	Status_<-0; if(Tr[i,6]=="LIVING") Status_<-1
    if(i==ltree) end<-"]}"
    str.json<-c(str.json,paste0(toJSON(
	list(Site=Tr[i,2],Species=Tr[i,3],Name=ntree[i], Article=Tr[i,4],
		 Contact=Tr[i,5],Old=lsr,be.sep=list(Status=Status_),
	     Years=c(as.numeric(nyear[srf[1]]), as.numeric(nyear[srf[length(srf)]])),
	     DBH=DBH.t, Series=round(sr,3))
	  ),end))	
}
file.Conn<-file("TreeRWdata.json")
writeLines(str.json, file.Conn)
close(file.Conn)