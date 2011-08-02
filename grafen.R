# demographics, age structure
par(mfrow=c(1,1))
data = read.csv('Data/altersstruktur.csv')
plot(data$Value[data$Age=='Population (hist&proj) 00-04, persons' & data$Sex=='All persons' & data$Time=='2011'], axes=F, ylim=c(0,5000), xlab="Years", ylab="Population", pch=3)
legend(1,200, c("US working hours", "German working hours"), pch=c(3, 4))
box()
axis(2)
axis(1, at=1:length(data$Time[data$Age=='Population (hist&proj) 00-04, persons' & data$Sex=='All persons' & data$Time=='2011']),lab=data$Time[data$Value[data$Age=='Population (hist&proj) 00-04, persons' & data$Sex=='All persons' & data$Time=='2011']])
# (...)
stop()
# working hours DE vs US
#doof wegen Jahresdaten! --> zu wenig Datenpunkte
par(mfrow=c(1,1))
data = read.csv('working hours D vs US.csv')
plot(data$Value[which(data$Country=='Germany' & data$Time>2006 & data$Employment.status=='Total employment')], axes=F, ylim=c(1350,1800), xlab="Years", ylab="working hours", pch=3)
points(data$Value[which(data$Country=='United States' & data$Time>2006 & data$Employment.status=='Total employment')], pch=4)
legend(1,200, c("US working hours", "German working hours"), pch=c(3, 4))
box()
axis(2)
axis(1, at=1:length(data$Time[which(data$Country=='Germany' & data$Time>2006 & data$Employment.status=='Total employment')]) ,lab=data$Time[which(data$Country=='Germany' & data$Time>2006 & data$Employment.status=='Total employment')])
#stop()


# German Unemployment vs US unemployment
#par(mfrow=c(2,1))
# first plot
#plot(rev(data$Total[data$year>=2008]), axes=F, ylim=c(0,10),type="l", xlab="Years", ylab="in %")
#legend(1,2, "German Unemployment Rate")
#box()
#axis(2)
#axis(1, at=which(vec!=""), lab=vec2[vec2>=2008])
# second plot
#plot(data2$Total[data2$year>=2008], axes=F, ylim=c(0,10),type="l", xlab="Years", ylab="in %")
#legend(1,2, "US Unemployment Rate")
#box()
#axis(2)
#axis(1, at=which(vec!=""), lab=vec2[vec2>=2008])
