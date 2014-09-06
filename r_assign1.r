##############################################################

# Traffic Flow on Highways in California
# 6 col, 1740 obs
# cols: time (five min intervals), flow (# of cars) in each lane, occupancy (proportion of time with car over loop) in each lane

#clean data set
traffic <- read.csv("http://www.stat.berkeley.edu/~s133/data/flow-occ.csv", header=TRUE, stringsAsFactors=FALSE) 
newnames <- c('time', 'occ1', 'flow1', 'occ2', 'flow2', 'occ3', 'flow3')
names(traffic)<-newnames

#lane congestion
flowdat <- subset(traffic, select=c(flow1, flow2, flow3))
flowdat2 <- cbind(flowdat, max=names(flowdat)[apply(flowdat, 1, which.max)])
#apply 1 means apply to row; apply 2 means apply to column
table(flowdat2$max)

#lane2 has the greatest flow during 1282/1740 of the 5-min time intervals
#no lane is consistently higher than the other lanes

#relationship between occupancy and flow
flow<-c(traffic$flow1, traffic$flow2, traffic$flow3)
occ<-c(traffic$occ1, traffic$occ2, traffic$occ3)
plot(flow, occ, main="Occupancy vs. Flow", xlab="Flow", ylab="Occupancy")
#apparent direct relationship

#most congested time of the day
day <- rep(NA, length(traffic$time))
day<-c(rep(1:288, 6), 1:12)
traffic <- transform(traffic, timeday=day)
head(traffic)
#every twelve timedays makes 1 hour
summ <- as.data.frame(aggregate(traffic[,c('occ1', 'flow1', 'occ2', 'flow2', 'occ3', 'flow3')], traffic['timeday'], mean))
summ$timeday<-summ$timeday*(5/60)
tail(summ)
[which.max(summ$flow2),]
summ[which.max(summ$flow1),]
summ[which.max(summ$flow3),]
#create graphs
pdf('trafficflow.pdf')
with(summ, plot(timeday, flow1, col="red", main="Flow of Traffic Throughout a Day", xlab='24-Hr Time', ylab='Number of Cars'))
par(new=TRUE)
with(summ, plot(timeday, flow2, col="blue", xlab='24-Hr Time', ylab='Number of Cars', axes=FALSE))
par(new=TRUE)
with(summ, plot(timeday, flow3, col="green", xlab='24-Hr Time', ylab='Number of Cars', axes=FALSE))
with(summ, legend('topleft', legend=c('Lane 1', 'Lane 2', 'Lane 3'), col=c('red', 'blue', 'green'), pch=1))
dev.off()

pdf('trafficoccupancy.pdf')
with(summ, plot(timeday, occ1, col="red", main="Occupancy of Traffic Throughout a Day", xlab='24-Hr Time', ylab='Proportion of Cars'))
par(new=TRUE)
with(summ, plot(timeday, occ2, col="blue", xlab='24-Hr Time', ylab='Proportion of Cars', axes=FALSE))
par(new=TRUE)
with(summ, plot(timeday, occ3, col="green", xlab='24-Hr Time', ylab='Proportion of Cars', axes=FALSE))
with(summ, legend('topleft', legend=c('Lane 1', 'Lane 2', 'Lane 3'), col=c('red', 'blue', 'green'), pch=1))
dev.off()
