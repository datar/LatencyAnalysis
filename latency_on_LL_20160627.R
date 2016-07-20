library("knitr")
filenames = c("20160627_LL_SF_64b.csv",
              "20160627_LL_SF_128b.csv",
              "20160627_LL_SF_256b.csv",
              "20160627_LL_SF_512b.csv",
              "20160627_LL_MLNX_64b.csv",
              "20160627_LL_MLNX_128b.csv",
              "20160627_LL_MLNX_256b.csv",
              "20160627_LL_MLNX_512b.csv"
              )
system.name = "LL"
sep = "##############################\n"
for(filename in filenames){
  data.ll = read.csv(filename, header = TRUE, colClasses=c("NULL", "integer", "integer"))
  data.ll.length = length(data.ll$inner)
  data.ll$inner[1] = as.integer(mean(data.ll$inner[2:data.ll.length]))
  latency.ll.in = data.ll$inner
  latency.ll.net = data.ll$net
  latency.ll.net.total = sum(data.ll$net)/1000
  latency.ll.in.total = sum(data.ll$inner)/1000
  ranges = rbind(c(0,60,1),
                 c(60,200,10),
                 c(200,500,50),
                 c(500,4000,500)
  )
  
  hist.table = data.frame(start=integer(), end=integer(), count=integer())
  
  for(c in 1:nrow(ranges)){
    range.start = ranges[c,1]
    range.end = ranges[c,2]
    range.step = ranges[c,3]
    b = seq(range.start, range.end, range.step)
    range.target = (latency.ll.in > range.start & latency.ll.in <= range.end)
    range.count = sum(range.target)
    range.count.ratio = range.count/(data.ll.length-1)
    range.latency = sum(latency.ll.in[range.target])
    range.latency.ratio = range.latency/latency.ll.in.total/1000
    text.capital = sprintf("Internal Latency on %s(%d ns ~ %d ns)", system.name, range.start, range.end)
    text.comment = sprintf("Count in Range:%d, %.4f%% total sample", range.count, range.count.ratio*100)
    
    cat(sprintf(sep))
    cat(sprintf("DATA:%s on %s\n", filename, system.name))
    cat(sprintf("RANGE:%d ns - %d ns\n", range.start, range.end))
    cat(sprintf("Sample Number:%d, %.4f%% of total %d samples\n", range.count, range.count.ratio*100, data.ll.length))
    
    h.data = hist(latency.ll.in[range.target], breaks = b,right = TRUE, 
                  main = text.capital, sub = text.comment, xlab = "Time(ns)", ylab = "Count", 
                  las = 1
    )
    hist.table = rbind(hist.table, cbind(h.data$breaks[1:length(h.data$breaks)-1], 
                                         h.data$breaks[1:length(h.data$breaks)-1]+range.step,
                                         h.data$counts))
  }
  cat(sprintf(sep))
  cat(sprintf("DATA:%s on %s\n", filename, system.name))
  cat(sprintf("Sample number:%d\n", data.ll.length))
  cat(sprintf("latency avg=%.1f ns, sd=%.1f ns\n", mean(latency.ll.in), sd(latency.ll.in)))
  print(summary(latency.ll.in))
  print(kable(hist.table, col.names = c("Start", "End", "Count")))
}