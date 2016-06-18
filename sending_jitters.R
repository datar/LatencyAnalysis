filename = "../testdata/solarflare/sf_20160419_send.csv"
description = "Time stamp of sending packets in software layer"
data = as.matrix(read.csv(filename, header = FALSE))
sample.num = length(data[,1])
latency = (data[2:sample.num,1]-data[1:sample.num-1,1])*10e9+data[2:sample.num,2]-data[1:sample.num-1,2]
hist(latency[latency<1000], breaks = 1000, main = "time intervel between sending two packets", xlab = "Time intervel(ns)")
duration = (data[sample.num,1]-data[1,1])*10e9+data[sample.num,2]-data[1,2]
sprintf("Sent:%d packets, Total time:%f second", sample.num, duration/1e9)
latency.eff = latency[10000:sample.num-1]
sprintf("Average= %f ns, SD = %f ns", duration/sample.num, sd(latency.eff))
