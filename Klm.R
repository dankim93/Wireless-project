setwd("~/Desktop/Klm/")
Offline<-readLines("offline.final.trace.txt")


processLine<-function(Line){
  L<-strsplit(Line,";")
  
  TimeData<-matrix(NA,nrow=1,ncol=6)
  
  TimeData[,1]<-as.numeric(unlist(strsplit(L[[1]][1],"="))[2])
  TimeData[,2]<-unlist(strsplit(L[[1]][2],"="))[2]
  TimeData[,c(3:5)]<-as.numeric(unlist(strsplit(unlist(strsplit(L[[1]][3],
                                                                "="))[2],",")))
  TimeData[,6]<-as.numeric(unlist(strsplit(L[[1]][4],"="))[2])
  colnames(TimeData)<-c("time","scanMAC","posX","posY",
                        "posZ","Orientation")
  
  n<-length(L[[1]])
  nmac<-length(L[[1]])-4
  if(nmac<0){return (0)}
  MACData<-matrix(NA,nrow=nmac,ncol=4)
  colnames(MACData)<-c("MAC","Signal",
                       "Channel","Type")
  for(i in 5:n){
    MACData[,"MAC"][(i-4)]<-unlist(strsplit(L[[1]][i],"="))[1]
    MACData[,"Signal"][(i-4)]<-as.numeric(unlist(strsplit(unlist(strsplit(L[[1]][i],"="))[2],
                                                          ","))[1])
    MACData[,"Channel"][(i-4)]<-as.numeric(unlist(strsplit(unlist(strsplit(L[[1]][i],"="))[2],
                                                           ","))[2])
    MACData[,"Type"][(i-4)]<-as.numeric(unlist(strsplit(unlist(strsplit(L[[1]][i],"="))[2],
                                                        ","))[3])
  }
  
  Mat<-matrix(NA,nrow=nmac,ncol=10)
  colnames(Mat)<-append(colnames(TimeData),colnames(MACData))
  Mat[,c(1:6)]<-matrix(rep(TimeData,nmac),nrow=nmac,ncol=6,byrow=TRUE)
  Mat[,c(7:10)]<-MACData
  return(Mat)
}


processLine(Offline[4])

# Offline file, Processed Data
options(warn=-1)
tmp <- lapply(Offline,processLine)
offline<-do.call(rbind,tmp)

id.adhoc<-which(offline[,"Type"]==1)
idnull<-which(offline[,"Type"]==0)
idtnull<-which(offline[,"time"]=="0")

offlineap<-offline[-append(id.adhoc,idnull,idtnull),]

# File processing
write.csv(offlineap,file="offline.csv")


y<-c()
# 12 Access Point MACs
MACFIND<-union(offlineap[,"MAC"],y)

# Cleaning the data
cleanData<-function(Data){
  id.adhoc<-which(Data[,"Type"]==1)
  idnull<-which(Data[,"Type"]==0)
  offlineap<-Data[-append(id.adhoc,idnull),]
  
  
  # Exploratory Analysis For 
  # Signal Strength
  par(mfcol=c(1,2))
  SignalS<-as.numeric(offlineap[,"Signal"])
  
  signal.boxp<-boxplot(SignalS,main="Boxplot for Signal Strength")
  
  hist(SignalS,
       main="Histogram for Signal Strength",
  )
  outliers<-signal.boxp$out
  id.outliers<-list()
  ido<-c()
  for(i in 1:length(outliers)){
    id.outliers[[i]]<-which(outliers[i]==SignalS)
    ido<-union(id.outliers[[i]],ido)
  }
  
  floorofflineap<-offlineap[-ido,]
  return(floorofflineap)
}


DataOff<-cleanData(Data=offline)
write.csv(DataOff,file="offlinecleaned.csv")
