# Graph of influence on unemployment that age has
res = do.simulation(start=2004, end=2010)

colSums(res$results$out_of_workforce_unemployed[,which(res$results$years>2004)])
# real change of employment
unemployed = c(4381281,4860909,4487305,3760072,3258451,3414545,3238421) #data from BA
years = 2004:2010
change = c()
change_pct = c()
for (i in 1:(length(unemployed)-1)) {
  change[i] = unemployed[i+1]-unemployed[i]
  change_pct[i] = (unemployed[i+1]-unemployed[i])/unemployed[i]
}
pct = c()
for (i in 2004:2009) 
  pct[which(i==years)] = sum(-res$results$out_of_workforce_unemployed[,which(res$results$years==i)+1])/change[which(i==years)]
change_due_to_workforce = colSums(res$results$out_of_workforce_unemployed[,res$results$years>2004])
sim_change = colSums(res$results$out_of_unemployment[,res$results$years>2004])
sim_pct = colSums(res$results$out_of_workforce_unemployed[,res$results$years>2004])/colSums(res$results$out_of_unemployment[,res$results$years>2004])
change[7] = NA; change_pct[7] = NA;
write.csv(list(actual_change.BA=change, change_pct.BA=change_pct, sim_change=sim_change, sim_change_due_to_workforce=change_due_to_workforce, sim_pct=sim_pct), 'graphs/sim_result_average_from_1998/ratio_of_unemployment_due_to_ageing.csv')
plot(sim_pct, type="l", lwd=2, ylab="", xlab="Year", axes=F, ylim=c(-0.2,0.3))
abline(h=0)
points(change_pct, type="l", lwd=2,, lty=2)
legend(2,0.1, c("ratio of people leaving unemployment because of age","historical change rate in unemployment"),lty=c(1,2))
axis(2)
box()
axis(1, at=1:7, labels=years)

stop()



# Graph of probabilities to become employed/unemployed over time
# I AM TRYING OUT CORRECTED VALUE BUT THEY HAVE BEEN CORRECTED WITH 1.183.108 which is the wrong number. Im not sure anymore which I used at the ZEW...
data = read.csv2('Data/extracted from iab/csv/probabilities.csv', stringsAsFactors=F)
data$pr_in_to_AL.grund = as.numeric(data$pr_in_to_AL.grund)
data$pr_out_of_AL.grund = as.numeric(data$pr_out_of_AL.grund)
yearly_data = read.csv2('Data/used for simulation/birthrate_mortality.csv', stringsAsFactors=F)
yearly_data$pr_in_to_AL_corrected=as.numeric(yearly_data$pr_in_to_AL_corrected)
yearly_data$pr_out_of_AL_corrected=as.numeric(yearly_data$pr_out_of_AL_corrected)
plot(data$pr_out_of_AL.grund[-1], type="l", lwd=2, ylim=c(0,0.21), axes=F, ylab="Probabilities", xlab="Year")
points(data$pr_in_to_AL.grund[-1], type="l", lty=2, lwd=2)
points(yearly_data$pr_out_of_AL_corrected[-1], type="l", lty=3, lwd=2)
points(yearly_data$pr_in_to_AL_corrected[-1], type="l", lty=4, lwd=2)
axis(2)
axis(1, at=1:29, labels = data$Time[-1])
box()
legend(1, 0.1, c("From unemployment into employment", "From employment to unemployment", "first corrected", "second corrected"), lty=c(1,2,3,4))
stop()


# Graph of probabilities to become employed/unemployed over time with projections from 2004 on
data = read.csv2('Data/extracted from iab/csv/probabilities.csv', stringsAsFactors=F)
data$pr_in_to_AL.grund = as.numeric(data$pr_in_to_AL.grund)
data$pr_out_of_AL.grund = as.numeric(data$pr_out_of_AL.grund)
yearly_data = read.csv2('Data/used for simulation/birthrate_mortality.csv', stringsAsFactors=F)
plot(data$pr_out_of_AL.grund[-1], type="l", lwd=2, ylim=c(0,0.21), xlim=c(1,length(data$pr_out_of_AL.grund[-1])+6), axes=F, ylab="Probabilities", xlab="Year")
points(data$pr_in_to_AL.grund[-1], type="l", lty=2, lwd=2)
avg1 = sum(data$pr_out_of_AL.grund[-1])/length(data$pr_out_of_AL.grund[-1])
avg2 = sum(data$pr_in_to_AL.grund[-1])/length(data$pr_in_to_AL.grund[-1])
points(c(29,35), c(avg1,avg1), type="l", col="red", lty=1, lwd=2) # first progression average
points(c(29,35), c(avg2,avg2), type="l", col="red", lty=2, lwd=2) # first progression average

vec = 1:length(data$pr_out_of_AL.grund[-1])
res1 = lm(data$pr_out_of_AL.grund[-1] ~ vec)
res2 = lm(data$pr_in_to_AL.grund[-1] ~ vec)
pr1 = data$pr_out_of_AL.grund[30] + res1$coefficients[2]*((30:36)-30)
pr2 = data$pr_in_to_AL.grund[30] + res2$coefficients[2]*((30:36)-30)
points(29:35, pr1, type="l", col="green", lty=1, lwd=2) # second projection... regression line
points(29:35, pr2, type="l", col="green", lty=2, lwd=2) # second projection... regression line

avg1 = sum(data$pr_out_of_AL.grund[24:30])/length(data$pr_out_of_AL.grund[24:30])
avg2 = sum(data$pr_in_to_AL.grund[24:30])/length(data$pr_in_to_AL.grund[24:30])
points(c(29,35), c(avg1,avg1), type="l", col="purple", lty=1, lwd=2) # first progression average
points(c(29,35), c(avg2,avg2), type="l", col="purple", lty=2, lwd=2) # first progression average

axis(2)
axis(1, at=1:(length(data$pr_out_of_AL.grund[-1])+6), labels = c(data$Time[-1],2005:2010))
box()
legend(1, 0.2, legend=c("From unemployment into employment", "From employment into unemployment", "First projection (long-run average)", "Second projection (regression)", "Third projection (average from 1998 on)", "..."), lty=c(1,2,1,1,1,0), col=c("black","black","red","green","purple","black"))
stop()


# percentage of out_of_unemployment that is due to elderly that move out of workforce
res = do.simulation(2004, 2010)
prb = colSums(res$results$out_of_workforce_unemployed)/colSums(res$results$out_of_unemployment)
year = res$results$year
print(year);
print(prb);


stop()

# elderly unemployment per year compared to average unemployment
par(mfrow = c(1,1))
data=read.csv2('Data/age_specific_unemployment.csv', stringsAsFactors=F)
workforce = as.numeric(data$Value[data$Series=="Labour Force" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Age=="Total"])
elderly_workforce = as.numeric(data$Value[data$Series=="Labour Force" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Age=="55 to 59"])
elderly_unemployment = as.numeric(data$Value[data$Series=="Unemployment" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Age=="55 to 59"]) / elderly_workforce
average_unemployment = as.numeric(data$Value[data$Series=="Unemployment" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Age=="Total"])/workforce
years = as.numeric(data$Time[data$Series=="Unemployment" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Age=="55 to 59"])

plot(years, elderly_unemployment, type="l", lty = 1, ylab="unemployment rate", xlab="Time")
points(years, average_unemployment, lty=2, type = "l")
legend(1970, 0.15, c("unemployment rate of 55 to 59 year olds","average unemployment rate"), lty=c(1,2))
stop()
# graphs for my simulation:
#unemployment by age in 2004
year = 2004
par(mfrow=c(2,1))
data=read.csv2('Data/age_specific_unemployment.csv', stringsAsFactors=F)
unemployed = as.numeric(data$Value[data$Series=="Unemployment" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Time==year & data$Age!="Total" & data$Age!="75+"])
ages = data$Age[data$Series=="Unemployment" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Time==year & data$Age!="Total" & data$Age!="75+"]
workforce = as.numeric(data$Value[data$Series=="Labour Force" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Time==year & data$Age!="Total"& data$Age!="75+"])
plot(unemployed/workforce, ylim=c(0,0.15), xlim=c(0,length(unemployed)), ylab="", type="l", lwd=2, axes=F, main="unemployment rate by age")
axis(2)
box()
axis(1, at=1:length(ages), labels=ages)
legend(0,0.05, "average unemployment rate", lwd=1, lty=2)
abline(h=as.numeric(data$Value[data$Series=="Unemployment" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Time==year & data$Age=="Total"])/as.numeric(data$Value[data$Series=="Labour Force" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Time==year & data$Age=="Total"]), lty=2)

total = as.numeric(data$Value[data$Series=="Unemployment" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Time==year & data$Age!="Total" & data$Age!="75+"])
barplot(total, xlim=c(0,length(ages)), space=c(0.5, rep(0,length(total)-1)), axes=F, main="number of unemployed people", ylim=c(0,600))
axis(2)
box()
axis(1, at=1:length(ages), labels=ages)

#axis(1, at=1:12,labels=1:12)

# average unemployed per age group
#abline(h=sum(as.numeric(data$Value[data$Series=="Unemployment" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Time=="2004" & data$Age=="Total"]))/length(as.numeric(data$Value[data$Series=="Labour Force" & data$Sex=="All persons" & data$Frequency=="Annual" & data$Time=="2004" & data$Age!="Total"])), lty=2)
#legend(0,150, "average number of unemployed per age group", lwd=1, lty=2)



stop()

#unemployment rate over time
par(mfrow=c(1,1))
data = c()
year = c()
for (i in 1: dim(res$results$unemployed)[2]) {
  data[i] = sum(res$results$unemployed[,i])/sum(res$results$in_workforce[,i])*100
  year[i] = res$results$years[i]
}
plot(year,data, type="l", ylab="unemployment rate", lwd=2)
abline(v = 2002, lty=2) # vertical lines
axis(1)
#axis(2, at=seq(10000000,40000000,2000000), label=seq(10,40,2))
legend(2000,26000000, "employed in millions")
box()
stop()
# workforce change over time
par(mfrow=c(1,1))
data = c()
year = c()
for (i in 1: dim(res$results$in_workforce)[2]) {
  data[i] = sum(res$results$in_workforce[,i])
  year[i] = res$results$years[i]
}
plot(year,data, type="l", ylab="workforce", axes=F, lwd=2, ylim=c(25000000,36000000))
abline(v = 2002, lty=2) # vertical lines
axis(1)
axis(2, at=seq(10000000,40000000,2000000), label=seq(10,40,2))
legend(2000,26000000, "workforce in millions")
box()
stop()
# Age distribution over time:
par(mfrow=c(2,2))
#res = do.simulation()
data = matrix(rep(0,4*11),nrow=11)
dnum = 0
for (d in c(1,16,26,34)) { # gather data for graphs for years T 1, 16, 26 and 34
  last_i = 0
  dnum = dnum + 1
  for (i in seq(10,110,10)) {
    data[i/10,dnum] = sum(res$results$population[(last_i:i)[-1],d])
    print((last_i:i)[-1])
    last_i = i
  }
}
print(data);
barplot(data[,1]/sum(data[,1]), horiz=T, main="1975", ylab="age", xlab="proportion of population") # initial values 1975
axis(2, at=seq(1,12), labels=seq(10,120,10))
barplot(data[,2]/sum(data[,2]), horiz=T, main="1990", ylab="age", xlab="proportion of population") # 1990
axis(2, at=seq(1,12), labels=seq(10,120,10))
barplot(data[,3]/sum(data[,3]), horiz=T, main="2000", ylab="age", xlab="proportion of population") # 2000
axis(2, at=seq(1,12), labels=seq(10,120,10))
barplot(data[,4]/sum(data[,4]), horiz=T, main="2008", ylab="age", xlab="proportion of population") # 2008
axis(2, at=seq(1,12), labels=seq(10,120,10))
#for (t in (res$start_year+1):res$end_year) {
#}
stop()
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
stop()


# German Unemployment vs US unemployment
par(mfrow=c(2,1))
# first plot
plot(rev(data$Total[data$year>=2008]), axes=F, ylim=c(0,10),type="l", xlab="Years", ylab="in %")
legend(1,2, "German Unemployment Rate")
box()
axis(2)
axis(1, at=which(vec!=""), lab=vec2[vec2>=2008])
# second plot
plot(data2$Total[data2$year>=2008], axes=F, ylim=c(0,10),type="l", xlab="Years", ylab="in %")
legend(1,2, "US Unemployment Rate")
box()
axis(2)
axis(1, at=which(vec!=""), lab=vec2[vec2>=2008])
