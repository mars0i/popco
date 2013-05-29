# make a bunch of PDF files of activation plots
# this one is for a 2-pundit run, so I skip the first two persons, begin with #3, who's named "P01",
# then graph 1000 ticks.  This is from RUN351493575, which is the 40th run in mra3b2p.
for (j in 3:22){cat(j,"\n");pdf(file=paste0("mra3b2p1000ticksRUN351493575P",formatC(j-2,width=2,format="d",flag="0"),".pdf")); plot(1, type="n", ylim=c(-1,1), xlim=c(1,1000), ylab="degree of belief", xlab="time"); for (i in 1:38){lines(mra3b2p[j,i,,40], type="l", col=mycolors[i])};dev.off()}

# to display rather than make pdfs:
for (j in 3:22){dev.new(); plot(1, type="n", ylim=c(-1,1), xlim=c(1,1000), ylab="activation", xlab="time"); for (i in 1:38){lines(mra3b2p[j,i,,40], type="l", col=mycolors[i])}}


for (j in 3:22){cat(j,"\n");pdf(file=paste0("mra3b2p1000ticksRUN399263308P",formatC(j-2,width=2,format="d",flag="0"),".pdf")); plot(1, type="n", ylim=c(-1,1), xlim=c(1,1000), ylab="degree of belief", xlab="time"); for (i in 1:38){lines(mra3b2p[j,i,,47], type="l", col=mycolors[i])};dev.off()}

for (j in 3:22){cat(j,"\n");pdf(file=paste0("mra3b2p1000ticksRUN387661242P",formatC(j-2,width=2,format="d",flag="0"),".pdf")); plot(1, type="n", ylim=c(-1,1), xlim=c(1,1000), ylab="degree of belief", xlab="time"); for (i in 1:38){lines(mra3b2p[j,i,,46], type="l", col=mycolors[i])};dev.off()}
