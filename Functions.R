# Part 1 : Data Processing

setwd("C:/Users/Eugene_PC/Desktop/Project2")
txt=readLines("offline.final.trace.txt")

# Task 1

processLine=function(x){
  
  #Dropping line with no information 
  if(length(grep("#",x))==1){return(NULL)}
  
  split=strsplit(x,"[;=,]")[[1]]
  
  #Dropping line with no data inside 
  if(length(split)==10){return(NULL)}
  
  #Changing Values
  nrow=length(split[11:length(split)])/4
  Mat1=matrix(split[11:length(split)],ncol=4,nrow=nrow,byrow=TRUE)
  
  #Fixed Values
  common=split[c(2,4,6,7,8,10)]
  Mat2=matrix(rep(common,nrow),ncol=6,byrow=TRUE)
  
  Mat=cbind(Mat2,Mat1)
  
  return(Mat)
}

# Offline

tmp=lapply(txt,processLine)
offline=as.data.frame(do.call("rbind", tmp))
names(offline)=c("time","scanMac","posX","posY","posZ","orientation","mac","signal","channel","type")

save(offline,file="offline.rda")



# Task 2 : Manually clean data


# Convert data that should be numeric

offline$time=as.numeric(levels(offline$time))[offline$time]
offline$posX=as.numeric(levels(offline$posX))[offline$posX]
offline$posY=as.numeric(levels(offline$posY))[offline$posY]
offline$posZ=as.numeric(levels(offline$posZ))[offline$posZ]
offline$orientation=as.numeric(levels(offline$orientation))[offline$orientation]
offline$signal=as.numeric(levels(offline$signal))[offline$signal]

# Explore the data

summary(offline) #posZ and scanMac have only one value

# Drop idhoc

id.adhoc=which(offline$type==1)
offline=offline[-id.adhoc,]

# We no longer need type as we dropped idhoc, and posZ and scanMac 

offline=offline[,c(1,3:4,6:9)]

# Drop the duplicated data

offline=offline[-duplicated(offline),]

summary(offline)

# Round to nearest 45 degrees

offline$round.orien=offline$orientation

offline=offline[c(1,2,3,4,8,5,6,7)]

offline$round.orien[which(offline$orientation>=0 & offline$orientation<22.5)]=0
offline$round.orien[which(offline$orientation>=22.5 & offline$orientation<67.5)]=45
offline$round.orien[which(offline$orientation>=67.5 & offline$orientation<112.5)]=90
offline$round.orien[which(offline$orientation>=112.5 & offline$orientation<157.5)]=135
offline$round.orien[which(offline$orientation>=157.5 & offline$orientation<202.5)]=180
offline$round.orien[which(offline$orientation>=202.5 & offline$orientation<247.5)]=225
offline$round.orien[which(offline$orientation>=247.5 & offline$orientation<292.5)]=270
offline$round.orien[which(offline$orientation>=292.5 & offline$orientation<337.5)]=315
offline$round.orien[which(offline$orientation>337.5 & offline$orientation<382.5)]=0 # 360 degree is the same as 0 degree

# Explore the mac address

MAC=union(offline$mac,y=c()) # There are 12 mac addresses

summary(offline$mac) # There are five mac addresses relatively lower than other seven

#00:04:0e:5c:23:fc  418      AVM GmbH 
#00:0f:a3:39:dd:cd  145619   Alpha Networks Inc.
#00:0f:a3:39:e0:4b	43508    Alpha Networks Inc.
#00:0f:a3:39:e1:c0	145862   Alpha Networks Inc.
#00:0f:a3:39:e2:10	19162    Alpha Networks Inc.
#00:14:bf:3b:c7:c6	126529   Cisco-Linksys, LLC
#00:14:bf:b1:97:81	120339   Cisco-Linksys, LLC
#00:14:bf:b1:97:8a	132961   Cisco-Linksys, LLC
#00:14:bf:b1:97:8d	121325   Cisco-Linksys, LLC
#00:14:bf:b1:97:90	122315   Cisco-Linksys, LLC
#00:30:bd:f8:7f:c5	301      BELKIN COMPONENTS
#00:e0:63:82:8b:a9	103      cabletron - yago systems, inc.

# We decided to drop these five

lowMacs=c("00:04:0e:5c:23:fc","00:0f:a3:39:e0:4b","00:0f:a3:39:e2:10","00:30:bd:f8:7f:c5","00:e0:63:82:8b:a9")

offline=offline[!(offline$mac%in%lowMacs),]

offlineManual=offline

# Now we have five Cisco&Linksys and two Alpha, but Lancom L-54g routers.
# We decide to just use these 7 routers 

#00:14:bf:3b:c7:c6  126529   Cisco-Linksys, LLC
#00:14:bf:b1:97:81	120339   Cisco-Linksys, LLC
#00:14:bf:b1:97:8a	132961   Cisco-Linksys, LLC
#00:14:bf:b1:97:8d	121325   Cisco-Linksys, LLC
#00:14:bf:b1:97:90	122315   Cisco-Linksys, LLC
#00:0f:a3:39:dd:cd  145619   Alpha Networks Inc.
#00:0f:a3:39:e1:c0  145862   Alpha Networks Inc.

keepMacs=c("00:14:bf:3b:c7:c6","00:14:bf:b1:97:81","00:14:bf:b1:97:8a","00:14:bf:b1:97:8d","00:14:bf:b1:97:90","00:0f:a3:39:dd:cd","00:0f:a3:39:e1:c0")

# Wrap the above task into a function called CleanData

cleanData=function(data,keepMacs){
  
  data$time=as.numeric(levels(data$time))[data$time]
  data$posX=as.numeric(levels(data$posX))[data$posX]
  data$posY=as.numeric(levels(data$posY))[data$posY]
  data$posZ=as.numeric(levels(data$posZ))[data$posZ]
  data$orientation=as.numeric(levels(data$orientation))[data$orientation]
  data$signal=as.numeric(levels(data$signal))[data$signal]
  
  id.adhoc=which(data$type==1)
  data=data[-id.adhoc,]
  
  data=data[,c(1,3:4,6:9)]
  
  data=data[-duplicated(data),]
  
  data$round.orien=data$orientation
  
  data=data[c(1,2,3,4,8,5,6,7)]
  
  data$round.orien[which(data$orientation>=0 & data$orientation<22.5)]=0
  data$round.orien[which(data$orientation>=22.5 & data$orientation<67.5)]=45
  data$round.orien[which(data$orientation>=67.5 & data$orientation<112.5)]=90
  data$round.orien[which(data$orientation>=112.5 & data$orientation<157.5)]=135
  data$round.orien[which(data$orientation>=157.5 & data$orientation<202.5)]=180
  data$round.orien[which(data$orientation>=202.5 & data$orientation<247.5)]=225
  data$round.orien[which(data$orientation>=247.5 & data$orientation<292.5)]=270
  data$round.orien[which(data$orientation>=292.5 & data$orientation<337.5)]=315
  data$round.orien[which(data$orientation>337.5 & data$orientation<382.5)]=0
  
  data=data[data$mac%in%keepMacs,]
  
  return(data)
}

# Checking offlineManual and offlineFunction

load("offline.rda")

offlineFunction=cleanData(offline,keepMacs)

identical(offlineManual,offlineFunction)