
#testing code of curvigram
library("skyscapeR")
data(RugglesRSC)
curv1 <- curvigram(RugglesRSC$Dec,1)
curv2 <- curvigram(RugglesRSC$Dec,2)
datenow<- -4000
moontdecVR <- TopoDecfromSolarLunarEvent(JDutfromDate(datenow),c("moonmajor","moonminor","moonminor","moonmajor"),c(0,0,1,1))
suntdecVR <- TopoDecfromSolarLunarEvent(JDutfromDate(datenow),c("solstice","solstice"),c(0,1))
lunar <- sky.objects(c('moon','sun'),epoch=datenow,col=c("red","blue"),lty=c(3,2))
lunar
lunarVR=lunar
lunarVR$decs[1:4] <- moontdecVR
lunarVR$decs[5:6] <- suntdecVR
lunarVR
plotAz(c(93,108,105,98),obj=lunarVR,loc=c(52,0))
titlestring1g=paste("Moon-GeoDec(",datenow,"), ","1deg uncertainty",sep="")
titlestring2g=paste("Moon-GeoDec(",datenow,"), ","2deg uncertainty",sep="")
titlestring1t=paste("Moon-TopoDec(",datenow,"), ","1deg uncertainty",sep="")
titlestring2t=paste("Moon-TopoDec(",datenow,"), ","2deg uncertainty",sep="")
plotCurv(curv1,lunar,xlim=c(-45,-5),main=titlestring1g)
plotCurv(curv1,lunarVR,xlim=c(-45,-5),main=titlestring1t)
plotCurv(curv2,lunar,xlim=c(-45,-5),main=titlestring2g)
plotCurv(curv2,lunarVR,xlim=c(-45,-5),main=titlestring2t)

