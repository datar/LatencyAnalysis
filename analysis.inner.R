analysis.inner.file<-function(filename){
  data = as.matrix(read.csv(filename, header = FALSE))
  sample.len = nrow(data)
  data.reduce = data[2:sample.ltimeval.rx = data.reduce[,1]*1e09+data.reduce[,2]
  timeval.rx.hardware = data.reduce[,1]*1e09+data.reduce[,2]
  timeval.rx.user = data.reduce[,3]*1e09+data.reduce[,4]
  timeval.rx = timeval.rx.user - timeval.rx.hardware
  
  
  latency.core = latency[latency<latency.core.upper & latency>latency.core.lower]
  text.exp = sprintf("sample ratio of core area:%f",length(latency.core)/length(latency))
  out.file = sprintf("%s_latency_result.jpg", filename)
  jpeg(file = out.file)
  hist(latency.core, breaks = 200)
  dev.off()
}