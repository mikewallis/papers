setwd("<location of accounting file>")
myData <- read.csv("accounting",sep=":", stringsAsFactors = FALSE, header = FALSE)
#
# the sge accounting file doesn't have a header row.
#
colnames(myData) <- c('qname','hostname','group','owner','job_name','job_ID','account','priority','submission_time','start_time','end_time','failed','exit_status','ru_wallclock','ru_utime','ru_stime','ru_maxrss','ru_ixrss','ru_ismrss','ru_idrss','ru_isrss','ru_minflt','ru_majflt','ru_nswap','ru_inblock','ru_oublock','ru_msgsnd','ru_msgrcv','ru_nsignals','ru_nvcsw','ru_nivcsw','project','department','granted_pe','slots','task_number','cpu','mem','io','category','iow','pe_taskid','maxvmem','arid','ar_sub_time')

# we don't need any of these (in fact we don't need most cols for this 
# exercise, but I'm being preemptively lazy and will need them in future)
# 
myData <- myData[ -c(17:30)]

# we don't want any accidental jobs that have sub, start or end dates 
# set before Jan 1st 2000, as these show problems with clocks.
#
myData <- myData[!(myData$submission_time < 978307200),]
myData <- myData[!(myData$start_time < 978307200),]
myData <- myData[!(myData$end_time < 978307200),]
myData$wait_time <- (myData$start_time - myData$submission_time)

# do we need real dates? not yet, but will do at some point
#myData$submission_time <- as.POSIXct(myData$submission_time,tz="UTC","1970-01-01 00:00:00")
#myData$start_time <- as.POSIXct(myData$start_time,tz="UTC","1970-01-01 00:00:00")
#myData$end_time <- as.POSIXct(myData$end_time,tz="UTC","1970-01-01 00:00:00")

# tidy this up
tmp <- sapply(strsplit(myData$category,"h_rt="), `[`, 2)
tmp <- sapply(strsplit(tmp,","), `[`, 1)
tmp <- sapply(strsplit(tmp," "), `[`, 1)
myData$h_rt <- as.integer(tmp)
rm (tmp)

runtime <- as.data.frame(myData[,'h_rt'])
waittime <- as.data.frame(myData[,'wait_time'])
wallclock <-as.data.frame(myData[,'ru_wallclock'])

#
# this is 96 hrs, because MARC1 has some 96hr queues; for arc2/3/polaris
# change these to 48
#
runtime$bins <- cut(myData$h_rt, breaks=96, labels=1:96,include.lowest = T)
waittime$bins <- cut(myData$wait_time, breaks=96, labels=1:96,include.lowest=T)
wallclock$bins <- cut(myData$ru_wallclock, breaks=96, labels=1:96,include.lowest=T)

#
# obvs, change all refs to specific machines in the below
# (this can almost certainly be looped, but I only started 
# learning R yesterday)
#
hist.data=hist(as.numeric(wallclock$bins), plot=F)
hist.data$counts[hist.data$counts>0] <- log(hist.data$counts[hist.data$counts>0], 10)
png(filename="actual_runtime_marc1.png", width=1200,height=1600,units="px")
plot(hist.data,lwd=5,lend=2,xlab="runtime (hrs)",ylab="number of jobs(log10)",main="Actual runtime, MARC1", col="dark green")
dev.off()

hist.data=hist(as.numeric(runtime$bins), plot=F)
hist.data$counts[hist.data$counts>0] <- log(hist.data$counts[hist.data$counts>0], 10)
png(filename="requested_runtime_marc1.png", width=1200,height=1600,units="px")
plot(hist.data,lwd=5,lend=2,xlab="runtime (hrs)",ylab="number of jobs(log10)",main="Requested runtime, MARC1", col="orange")
dev.off()

hist.data=hist(as.numeric(waittime$bins), plot=F)
hist.data$counts[hist.data$counts>0] <- log(hist.data$counts[hist.data$counts>0], 10)
png(filename="waittime_marc1.png", width=1200,height=1600,units="px")
plot(hist.data,lwd=5,lend=2,xlab="wait time (hrs)",ylab="number of jobs(log10)",main="Job wait time, MARC1",col="dark red")
dev.off()
