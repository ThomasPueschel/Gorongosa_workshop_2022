#####################################################################################################
### BASICS ###
#####################################################################################################

# R as calculator
1+1
calculation<-1+1
calculation

sum(1,1)

calculation<-sum(1,1)

#Vector
v<-1:6
length(v)
v<-c(1,5)
length(v)
v<-rep(12,10)
length(v)
#Rename vectors!
v1<-1:6
v2<-c(1,5)
v3<-rep(12,10)


#Data frame
data<-data.frame(1:3,4:6,7:9)
rownames(data)<-c("Species1","Species2","Species3") 
colnames(data)<-c("Trait1","Trait2","Trait3")
data
#Manipulating the data frame
data[,2]
data[2,]
data[2,2]
data[,-2]
data[-2,]
data2<-data[,-2]
data[2,2]<-10
data$Trait2

#Calculations with columns and adding columns
product<-data$Trait2*data$Trait3
product
data<-cbind(data,product)
data
#Ordering
data[order(data[,4]),]
data[order(data[,4],decreasing=TRUE),]
data1<-cbind(c(2,4,6),c(8,10,12))
data2<-cbind(c(12,10,8),c(6,4,2))
data1[order(match(data1[,1],data2[,2])),]
#The structure and summary of an object
str(data)
summary(data)

min(data)
min(data[,2])
max(data)
max(data[,2])

which(data[,2]>5)
data[,2][which(data[,2]>8)]
data$Trait2[which(data[,2]>5)]

which(data[,2]>5&data[,2]<9)
data[,2][which(data[,2]>5&data[,2]<9)]
data$Trait2[which(data[,2]>5&data[,2]<9)]

#Omitting NAs
str(airquality)
summary(airquality)
#Entire data set
na.omit(airquality)
data_noNAs<-na.omit(airquality)
str(data_noNAs)
#Subset through new data frame
data_noNAs_new<-as.data.frame(na.omit(cbind(airquality$Solar.R,airquality$Temp)))
colnames(data_noNAs_new)<-c("Solar.R","Temp")
str(data_noNAs_new)
#Subsetting a data frame
which(airquality$Month==5)
airquality$Temp[which(airquality$Month==5)]

#####################################################################################
#EXERCISE 1
#What is the average Wind speed at the 30th day of each month?
summary(airquality$Wind[which(airquality$Day==30)])
#Create a data frame without NAs that contains 'Solar.R', 'Temp', and 'Day'
newdataframe<-na.omit(cbind(airquality$Solar.R,airquality$Temp,airquality$Day))
summary(newdataframe)
#How many times did the Ozone value surpass 70? List these values.
length(which(airquality$Ozone>70))
airquality$Ozone[which(airquality$Ozone>70)]
#Given the instances in which the Ozone surpassed 70, how many times did the temperature surpass 85?
Temp_values<-airquality$Temp[which(airquality$Ozone>70)]
Temp_elements<-which(Temp_values>85)
Temp_values<-Temp_values[Temp_elements]

#Alternative (google: "R How to find common elements from multiple vectors?")
airquality$Temp[intersect(which(airquality$Ozone>70),which(airquality$Temp>85))]
#####################################################################################

#####################################################################################################
### GRAPHICS ###
#####################################################################################################
#Scatter plots
cars
str(cars)
summary(cars)

data<-cars
x<-data$speed
y<-data$dist
plot(x,y)
plot(y~x)
plot(cars)

#Titles, labels, etc
plot(cars,main="Cars example",xlab="Speed",ylab="Distance")
plot(cars,main="Cars example",xlab="Speed",ylab="Distance",pch=2)
plot(cars,main="Cars example",xlab="Speed",ylab="Distance",pch=2,cex=4)
#Colors
plot(cars,main="Cars example",xlab="Speed",ylab="Distance",pch=2,col="red")
plot(cars,main="Cars example",xlab="Speed",ylab="Distance",pch=24,col="red",bg="green")
pch_vector<-rep(17, length(cars$speed))
pch_vector
plot(cars,main="Cars example",xlab="Speed",ylab="Distance",pch=pch_vector)

setwd("")
pdf("Workspace/Day 01_Session 01_Plot cars example.pdf")
plot(cars,main="Cars example",xlab="Speed",ylab="Distance",pch=pch_vector)
dev.off()      
pdf("Workspace/Day 01_Session 01_Plot cars example.pdf",height=10,width=4)
plot(cars,main="Cars example",xlab="Speed",ylab="Distance",pch=pch_vector)
dev.off()      

#Points, lines, polygons
points(10,80)
points(10,80,pch=17,col="green",cex=10/5)
points(5,60,pch=17,col="green",cex=5/5)

lines(c(5,10),c(60,80))
lines(c(5,10),c(90,110),lty=2,lwd=3)
lines(c(5,10),c(100,120),lty=3,lwd=5)

abline(v=mean(cars$speed))
abline(h=mean(cars$dist))

?polygon
polygon(c(5,10,10),c(60,60,80))
polygon(c(5,5,10,10),c(80,60,60,80))

?arrows
arrows(7.5,50,7.5,60)

text(7.5,47,labels="Example")
#####################################################################################################
### LOADING AND SAVING DATA ###
#####################################################################################################

data<-read.table("Brain body size.txt",header=T,sep="\t",row.names=1)
str(data)
summary(data)
data<-log(data)
data[,1:2]<-log(data[,1:2])
str(data)
summary(data)
#####################################################################################
#EXERCISE 2
#Plot Brain (Y) to Body (X)
plot(data$Brain~data$Body)
#Highlight the species that have a brain size >4 and a body size <8 in a large full red circle
targets<-which(data$Brain>4&data$Body<8)
cex_vector<-rep(1,length(data$Brain))
cex_vector[targets]<-3
pch_vector<-rep(1,length(data$Brain))
pch_vector[targets]<-16
col_vector<-rep("black",length(data$Brain))
col_vector[targets]<-"red"
plot(data$Brain~data$Body,cex=cex_vector,pch=pch_vector,col=col_vector)
#Which species are these target species? 
rownames(data)[targets]
data[targets,1]
data[targets,1,drop=F]
#Add an arrow and a text box to indicate which genus they belong to
arrows(data$Body[targets[1]]-0.1,data$Brain[targets[1]]+0.2,data$Body[targets[1]]-0.1,data$Brain[targets[1]]+0.7)
text(data$Body[targets[1]]-0.1,data$Brain[targets[1]]+0.8,labels="Cebus")
#####################################################################################

setwd('')        
#Load and merge data
data<-read.table("Capitate.txt",header=T,sep="\t",row.names=1)
cbind(data,rownames(data))->dataC
data<-read.table("Triquetrum.txt",header=T,sep="\t",row.names=1)
cbind(data,rownames(data))->dataT
data<-merge(dataC,dataT,by.x="rownames(data)",by.y="rownames(data)")
rownames(data)<-data[,1]
str(data)
data<-data[,-1]
data
str(data)





