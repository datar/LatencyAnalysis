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

latency.wio.in.bp = read.csv("/home/samba/testdata/20160712/WIO_STUDY/latency_map_WIO_in_20160712_t60_bypass1.csv", row.names=1, quote="")
latency.number.wio.in.bp = as.integer(rownames(latency.wio.in.bp))

latency.wio.net.bp = read.csv("/home/samba/testdata/20160712/WIO_STUDY/latency_map_WIO_net_20160712_t60_bypass1.csv", row.names=1, quote="")
latency.number.wio.net.bp = as.integer(rownames(latency.wio.net.bp))

latency.wio.in.na = read.csv("/home/samba/testdata/20160712/WIO_STUDY/latency_map_WIO_in_20160712_t60_bypass0.csv", row.names=1, quote="")
latency.number.wio.in.na = as.integer(rownames(latency.wio.in.na))

latency.wio.net.na = read.csv("/home/samba/testdata/20160712/WIO_STUDY/latency_map_WIO_net_20160712_t60_bypass0.csv", row.names=1, quote="")
latency.number.wio.net.na = as.integer(rownames(latency.wio.net.na))


latency.ll.bp.sum = apply(latency.ll.net.bp, 2, sum)
latency.ll.na.sum = apply(latency.ll.net.na, 2, sum)
latency.wio.bp.sum = apply(latency.wio.net.bp, 2, sum)
latency.wio.na.sum = apply(latency.wio.net.na, 2, sum)



packet.number.plot.theme = 
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
  theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16))

plot.ll.bp.sum = ggplot(data = data.frame(size = 16:256, count = latency.ll.bp.sum[1:241]/60), mapping = aes(x = size, y = count))+
  geom_bar(stat= 'identity') + 
  coord_cartesian(ylim=c(24e4, 29e4)) + 
  ggtitle("Packet Number vs. Size [LL][onload]")
plot.ll.bp.sum + packet.number.plot.theme  

plot.ll.na.sum = ggplot(data = data.frame(size = 16:256, count = latency.ll.na.sum[1:241]/60), mapping = aes(x = size, y = count))+
  geom_bar(stat= 'identity') + 
  coord_cartesian(ylim=c(7e4, 9e4)) + 
  ggtitle("Packet Number vs. Size [LL][Kernel]")
plot.ll.na.sum + packet.number.plot.theme

plot.wio.bp.sum = ggplot(data = data.frame(size = 16:256, count = latency.wio.bp.sum[1:241]/60), mapping = aes(x = size, y = count))+
  geom_bar(stat= 'identity') + 
  coord_cartesian(ylim=c(20e4, 25e4)) + 
  ggtitle("Packet Number vs. Size [WIO][onload]")
plot.wio.bp.sum + packet.number.plot.theme  

plot.wio.na.sum = ggplot(data = data.frame(size = 16:256, count = latency.wio.na.sum[1:241]/60), mapping = aes(x = size, y = count))+
  geom_bar(stat= 'identity') + 
  coord_cartesian(ylim=c(7e4, 9e4)) + 
  ggtitle("Packet Number vs. Size [WIO][Kernel]")
plot.wio.na.sum + packet.number.plot.theme


diff.tp.ll.bp = (latency.ll.bp.sum[2:241] - latency.ll.bp.sum[1:240])/1e5
plot(16:255,diff.tp.ll.bp)
data.tp.ll.bp = data.frame(size=16:256, tp=latency.ll.bp.sum[1:241]/1e3)
model.tp.ll = lm(tp~size, data=data.tp.ll.bp)
summary(model.tp.ll)

data.tp.wio.bp = data.frame(size=16:256, tp=latency.wio.bp.sum[1:241]/1e3)
model.tp.wio = lm(tp~size, data=data.tp.wio.bp)
summary(model.tp.wio)

latency.ll.in.mean = latency.number.ll.in.bp %*% as.matrix(latency.ll.in.bp)*(1/latency.ll.bp.sum)
latency.ll.net.mean = latency.number.ll.net.bp %*% as.matrix(latency.ll.net.bp)*(1/latency.ll.bp.sum)
latency.wio.in.mean = latency.number.wio.in.bp %*% as.matrix(latency.wio.in.bp)*(1/latency.wio.bp.sum)
latency.wio.net.mean = latency.number.wio.net.bp %*% as.matrix(latency.wio.net.bp)*(1/latency.wio.bp.sum)


plot.latency.ll.net.mean = ggplot(data = data.frame(size = 16:256, latency = latency.ll.net.mean[1:241]), mapping = aes(x = size, y = latency))+
  geom_bar(stat= 'identity') + 
  coord_cartesian(ylim=c(3500,4250)) + 
  ggtitle("Network Latency vs. Size [LL][onload]")
plot.latency.ll.net.mean + packet.number.plot.theme



plot.latency.wio.net.mean = ggplot(data = data.frame(size = 16:256, latency = latency.wio.net.mean[1:241]), mapping = aes(x = size, y = latency))+
  geom_bar(stat= 'identity') + 
  coord_cartesian(ylim=c(4000,5000)) + 
  ggtitle("Network Latency vs. Size [WIO][onload]")
plot.latency.wio.net.mean + packet.number.plot.theme


plot.latency.ll.in.mean = ggplot(data = data.frame(size = 16:256, latency = latency.ll.in.mean[1:241]), mapping = aes(x = size, y = latency))+
  geom_bar(stat= 'identity') + 
  coord_cartesian(ylim=c(20,50)) + 
  ggtitle("Inner Latency vs. Size [LL][onload]")
plot.latency.ll.in.mean + packet.number.plot.theme


plot.latency.wio.in.mean = ggplot(data = data.frame(size = 16:256, latency = latency.wio.in.mean[1:241]), mapping = aes(x = size, y = latency))+
  geom_bar(stat= 'identity') + 
  coord_cartesian(ylim=c(20,65)) + 
  ggtitle("Inner Latency vs. Size [WIO][onload]")
plot.latency.wio.in.mean + packet.number.plot.theme


data.tp.ll.bp = data.frame(size=16:256, tp=latency.ll.bp.sum[1:241]/1e3, lat=latency.ll.net.mean[1:241])
model.tp.ll = lm(tp~size+lat, data=data.tp.ll.bp)
summary(model.tp.ll)

data.tp.ll.bp = data.frame(size=16:256, tp=latency.ll.bp.sum[1:241]/1e3, lat=1000000/latency.ll.net.mean[1:241])
model.tp.ll = lm(tp~size+lat, data=data.tp.ll.bp)
summary(model.tp.ll)

data.tp.ll.bp = data.frame(size=16:256, tp=latency.ll.bp.sum[1:241]/1e3, lat=latency.ll.net.mean[1:241])
model.lat.size.ll = lm(lat~size, data=data.tp.ll.bp)
summary(model.lat.size.ll)
plot(model.lat.size.ll)

data.tp.wio.bp = data.frame(size=16:256, tp=latency.wio.bp.sum[1:241]/1e3, lat=latency.wio.net.mean[1:241])
model.tp.wio = lm(tp~size+lat, data=data.tp.wio.bp)
summary(model.tp.wio)

data.tp.wio.bp = data.frame(size=16:256, tp=latency.wio.bp.sum[1:241]/1e3, lat=latency.wio.net.mean[1:241])
model.lat.size.wio = lm(lat~size, data=data.tp.wio.bp)
summary(model.lat.size.wio)


latency.ratio.ll.net.bp = apply(latency.ll.net.bp, 2, function(x) x/sum(x))
latency.ratio.ll.in.bp = apply(latency.ll.in.bp, 2, function(x) x/sum(x))
latency.ratio.wio.net.bp = apply(latency.wio.net.bp, 2, function(x) x/sum(x))
latency.ratio.wio.in.bp = apply(latency.wio.in.bp, 2, function(x) x/sum(x))

