filenames = c("20160622_ll_60s_01.csv",
              "20160622_ll_60s_02.csv",
              "20160622_ll_60s_03.csv",
              "20160622_ll_60s_04.csv",
              "20160622_ll_60s_05.csv"
              )
system.name = "LL"

for(filename in filenames){
  data.ll = read.csv(filename, header = TRUE, colClasses=c("NULL", "integer", "integer"))
  data.ll.length = length(data.ll$inner)
  data.ll$inner[1] = as.integer(mean(data.ll$inner[2:data.ll.length]))
  latency.ll.in = data.ll$inner
  latency.ll.net = data.ll$net
  latency.ll.net.total = sum(data.ll$net)/1000
  latency.ll.in.total = sum(data.ll$inner)/1000
  
  sprintf("Data file:%s on %s", filename, system.name)
  sprintf("Sample number:%d", data.ll.length)
  sprintf("latency avg=%.1f ns, sd=%.1f ns", mean(latency.ll.in), sd(latency.ll.in)) 
  summary(latency.ll.in)
  
  ranges = rbind(c(0,60,1),
                 c(60,200,10),
                 c(200,500,50),
                 c(500,4000,500)
  )
  
  
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
    text.comment = sprintf("Count in Range:%d, %.2f%% total sample", range.count, range.count.ratio*100)
    h.data = hist(latency.ll.in[range.target], breaks = b,right = TRUE, 
                  main = text.capital, sub = text.comment, xlab = "Time(ns)", ylab = "Count", 
                  las = 1
    )
    h.data$breaks
    h.data$counts  
  }
}