filename = "20160613_WIO.csv"
system.name = "WIO"
data.wio = read.csv(filename, header = FALSE)
data.wio.length = length(data.wio$V5)
latency.wio.net = data.wio$V5/2000
latency.wio.in = data.wio$V6[2:data.wio.length]
latency.wio.net.total = sum(latency.wio.net)*2
latency.wio.in.total = sum(latency.wio.in)/1000

sprintf("Sample number:%d", data.wio.length)
sprintf("latency avg=%.1f ns, sd=%.1f ns", mean(latency.wio.in), sd(latency.wio.in)) 
summary(latency.wio.in)


b = seq(0,60,1)
range.target = latency.wio.in <= 60
range.count = sum(range.target)
range.count.ratio = range.count/(data.wio.length-1)
range.latency = sum(latency.wio.in[range.target])
range.latency.ratio = range.latency/latency.wio.in.total/1000
text.capital = sprintf("Internal Latency on %s(<60 ns)", system.name)
text.comment = sprintf("Count in Range:%d, %.2f%% total sample", range.count, range.count.ratio*100)
hist(latency.wio.in[range.target], breaks = b,right = TRUE, 
     main = text.capital, sub = text.comment, xlab = "Time(ns)", ylab = "Count", 
     las = 1
)


b = seq(60,200,10)
range.target = (latency.wio.in > 60 & latency.wio.in <= 200)
range.count = sum(range.target)
range.count.ratio = range.count/(data.wio.length-1)
range.latency = sum(latency.wio.in[range.target])
range.latency.ratio = range.latency/latency.wio.in.total/1000
text.capital = sprintf("Internal Latency on %s (60 ns ~ 200 ns)", system.name)
text.comment = sprintf("Count in Range:%d, %.2f%% total sample", range.count, range.count.ratio*100)
hist(latency.wio.in[range.target], breaks = b,right = TRUE, 
     main = text.capital, sub = text.comment, xlab = "Time(ns)", ylab = "Count", 
     las = 1
)


b = seq(200,500,50)
range.target = (latency.wio.in > 200 & latency.wio.in <= 500)
range.count = sum(range.target)
range.count.ratio = range.count/(data.wio.length-1)
range.latency = sum(latency.wio.in[range.target])
range.latency.ratio = range.latency/latency.wio.in.total/1000
text.capital = sprintf("Internal Latency on %s (200 ns ~ 500 ns)", system.name)
text.comment = sprintf("Count in Range:%d, %.2f%% total sample", range.count, range.count.ratio*100)
hist(latency.wio.in[range.target], breaks = b,right = TRUE, 
     main = text.capital, sub = text.comment, xlab = "Time(ns)", ylab = "Count", 
     las = 1
)

