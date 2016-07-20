data.bp = read.csv("/home/samba/Analysis/latency_map_in_20160707_t60_bypass1.csv", row.names=1, quote="")
latency.value = 1:501
normal.factor = 1:501
normal.factor[1:100] = 1
normal.factor[101:200] = 100
normal.factor[201:501] = 10000
data.sum = apply(data.bp, 2, sum)
plot(16:1399, data.sum[1:1384]/60e6, type = 'h',xlim=c(16,1400), ylim = c(0,0.25), xlab = "packet size", ylab = "M packets/sec", main = "20160707 WIO 60s sockperf onload")

data.bp = read.csv("/home/samba/Analysis/latency_map_in_20160707_t60_bypass1.csv", row.names=1, quote="")
latency.value = 1:501
normal.factor = 1:501
normal.factor[1:100] = 1
normal.factor[101:200] = 100
normal.factor[201:501] = 10000
data.sum = apply(data.bp, 2, sum)
plot(257:1399, data.sum[1:1143]/60e6, type = 'h', xlab = "packet size", ylab = "M packets/sec", main = "20160707 WIO 60s sockperf onload")
