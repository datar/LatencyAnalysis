latency.value = 1:501
normal.factor = 1:501
normal.factor[1:100] = 1
normal.factor[101:200] = 100
normal.factor[201:501] = 10000

latency.ll.in.bp = read.csv("/home/samba/testdata/20160712/LL_STUDY/latency_map_LL_in_20160712_t60_bypass1.csv", row.names=1, quote="")
latency.number.ll.in.bp = as.integer(rownames(latency.ll.in.bp))


latency.ll.net.bp = read.csv("/home/samba/testdata/20160712/LL_STUDY/latency_map_LL_net_20160712_t60_bypass1.csv", row.names=1, quote="")
latency.number.ll.net.bp = as.integer(rownames(latency.ll.net.bp))


latency.ll.in.na = read.csv("/home/samba/testdata/20160712/LL_STUDY/latency_map_LL_in_20160712_t60_bypass0.csv", row.names=1, quote="")
latency.number.ll.in.na = as.integer(rownames(latency.ll.in.na))


latency.ll.net.na = read.csv("/home/samba/testdata/20160712/LL_STUDY/latency_map_LL_net_20160712_t60_bypass0.csv", row.names=1, quote="")
latency.number.ll.net.na = as.integer(rownames(latency.ll.net.na))

latency.ll.bp.sum = apply(latency.ll.net.bp, 2, sum)
plot(16:256, latency.ll.bp.sum[1:241]/60e6, type = 'h',xlim=c(16,256), ylim = c(0.24,0.3), xlab = "packet size", ylab = "M packets/sec", main = "20160712 LL 60s sockperf onload")

latency.ll.na.sum = apply(latency.ll.net.na, 2, sum)
plot(16:256, latency.ll.na.sum[1:241]/60e6, type = 'h',xlim=c(16,256), xlab = "packet size", ylab = "M packets/sec", main = "20160712 LL 60s sockperf")


ggplot(data = data.frame(size = 16:256, count = latency.ll.bp.sum[1:241]/60e6), mapping = aes(x = size, fill = count)+geom_bar(stat= 'identity'))
ggplot(data = data.frame(size = 16:256, count = latency.ll.bp.sum[1:241]/60e6))
ggplot(data = data.frame(size = 16:256, count = latency.ll.bp.sum[1:241]/60e6), mapping = aes(x = size, y = count))+geom_bar(stat= 'identity')
