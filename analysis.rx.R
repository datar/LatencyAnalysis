filename = "20160302191200_2hft_vma_ib.csv"
description = 
"
Servers:two low latency server,
Program:xing's send and recv 20160302,
Features:
        no libvma;
        direct connect servers on ethernet;
        send packet as fast as possible,
"
data = as.matrix(read.csv(filename, header = FALSE))
sample.len = nrow(data)
data.delta = data[2:sample.len,]-data[1:sample.len-1,]

timeval.rx.send = data.delta[,1]*1e09+data.delta[,2]
timeval.rx.hardware = data.delta[,3]*1e09+data.delta[,4]
timeval.rx.user = data.delta[,5]*1e09+data.delta[,6]

timeval.rx = timeval.rx.user - timeval.rx.hardware
timeval.client = timeval.rx.hardware - timeval.rx.send

"The latency shift from sender software to receiver NIC"
summary(timeval.client)     
sd(timeval.client)
hist(timeval.client, breaks=200, probability = T, main = "latency shift\n sending software to receiving NIC hardware", xlab = "latency shift (ns)")

"The latency shift from receiver NIC to receiver software"
summary(timeval.rx)
sd(timeval.rx)
hist(timeval.rx[timeval.rx<5000 & timeval.rx>-5000], breaks=200, probability = T, main = "latency shift\n in receiving server", xlab = "latency shift (ns)")


