filename = "20160621_sockperflogLLSolarF4.csv"
system.name = "LL"
data.ll = read.csv(filename, header = FALSE)
data.ll.length = length(data.ll$V5)
latency.ll.net = data.ll$V5/2000
latency.ll.in = data.ll$V6[2:data.ll.length]
latency.ll.net.total = sum(latency.ll.net)*2
latency.ll.in.total = sum(latency.ll.in)/1000

sprintf("Sample number:%d", data.ll.length)
sprintf("latency avg=%.1f ns, sd=%.1f ns", mean(latency.ll.in), sd(latency.ll.in)) 
summary(latency.ll.in)


b = seq(0,60,1)
range.target = latency.ll.in <= 60
range.count = sum(range.target)
range.count.ratio = range.count/(data.ll.length-1)
range.latency = sum(latency.ll.in[range.target])
range.latency.ratio = range.latency/latency.ll.in.total/1000
text.capital = sprintf("Internal Latency on %s(<60 ns)", system.name)
text.comment = sprintf("Count in Range:%d, %.2f%% total sample", range.count, range.count.ratio*100)
hist(latency.ll.in[range.target], breaks = b,right = TRUE, 
     main = text.capital, sub = text.comment, xlab = "Time(ns)", ylab = "Count", 
     las = 1
)


b = seq(60,200,10)
range.target = (latency.ll.in > 60 & latency.ll.in <= 200)
range.count = sum(range.target)
range.count.ratio = range.count/(data.ll.length-1)
range.latency = sum(latency.ll.in[range.target])
range.latency.ratio = range.latency/latency.ll.in.total/1000
text.capital = sprintf("Internal Latency on %s (60 ns ~ 200 ns)", system.name)
text.comment = sprintf("Count in Range:%d, %.2f%% total sample", range.count, range.count.ratio*100)
hist(latency.ll.in[range.target], breaks = b,right = TRUE, 
     main = text.capital, sub = text.comment, xlab = "Time(ns)", ylab = "Count", 
     las = 1
)


b = seq(200,500,50)
range.target = (latency.ll.in > 200 & latency.ll.in <= 500)
range.count = sum(range.target)
range.count.ratio = range.count/(data.ll.length-1)
range.latency = sum(latency.ll.in[range.target])
range.latency.ratio = range.latency/latency.ll.in.total/1000
text.capital = sprintf("Internal Latency on %s (200 ns ~ 500 ns)", system.name)
text.comment = sprintf("Count in Range:%d, %.2f%% total sample", range.count, range.count.ratio*100)
hist(latency.ll.in[range.target], breaks = b,right = TRUE, 
     main = text.capital, sub = text.comment, xlab = "Time(ns)", ylab = "Count", 
     las = 1
)


b = seq(500,4000,500)
range.target = (latency.ll.in > 500 & latency.ll.in <= 4000)
range.count = sum(range.target)
range.count.ratio = range.count/(data.ll.length-1)
range.latency = sum(latency.ll.in[range.target])
range.latency.ratio = range.latency/latency.ll.in.total/1000
text.capital = sprintf("Internal Latency on %s (500 ns ~ 4000 ns)", system.name)
text.comment = sprintf("Count in Range:%d, %.2f%% total sample", range.count, range.count.ratio*100)
hist(latency.ll.in[range.target], breaks = b,right = TRUE, 
     main = text.capital, sub = text.comment, xlab = "Time(ns)", ylab = "Count", 
     las = 1
)

