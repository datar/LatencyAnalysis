analysis.sample.file<-function(filename){
    data = read.csv(filename, header = FALSE)
    latency = data$V5/2000
    interval = 0.1
    latency.median = median(latency)
    latency.core.upper = latency.median*(1+interval)
    latency.core.lower = latency.median*(1-interval)
    latency.core = latency[latency<latency.core.upper & latency>latency.core.lower]
    text.exp = sprintf("sample ratio of core area:%f",length(latency.core)/length(latency))
    out.file = sprintf("%s_latency_result.jpg", filename)
    jpeg(file = out.file)
    hist(latency, breaks = 200)
    dev.off()
}