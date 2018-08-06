options(digits = 10)

#Testresults for Declination-Altitude code
TopoAltfromAppAlt(0, 15, 1013.25) + 0.5598886
AppAltfromTopoAlt(0, 15, 1013.25) - 0.4721439
GeoAltfromTopoAlt(-0.559888644263346, "moonavg") - 0.3922204
TopoAltfromGeoAlt(0, "moonavg") + 0.952
ParallaxfromTopoAlt(-0.559888644263346, "moonavg") - 0.9521091
ParallaxfromGeoAlt(0, "moonavg") - 0.952
S_Maxpar("moonavg") - 0.952
GeoDecfromAppAlt(53, 0, 66, 0, "moonavg", 15, 1013.25) - 14.49181
GeoDecfromGeoAlt(53, 0, 66, 0) - 14.16885
TopoAltfromDip(3000, 5) + 1.755515
AppAltfromDip(3000, 5, 15, 1013.25, 0.0065) + 1.589868
AppAltfromHeights(5, 3000, 2000, 15, 1013.25, 0.0065) - 56.25335
TopoAltfromHeights(5, 3000, 2000) - 56.25059

#testreult for Periodicy
JDs <- c(123456, 55, 242345, 1000)
SolarDayOpt(c(JDutfromDate(2000))) - 24.00000017
Sunobliquity(c(170686)) - 24.12502276
JDutfromDate("3.0.0.0.0") - 1016283
JDutfromDate("35/12/1") - 1734175.5

#testign code of curvigram
library("skyscapeR")
data(RugglesRSC)
curv1 <- curvigram(RugglesRSC$Dec,1)
curv2 <- curvigram(RugglesRSC$Dec,2)
datenow<- -2000
moontdecVR <- TopoDecfromSolarLunarEvent(JDutfromDate(datenow),c("moonmajor","moonminor","moonminor","moonmajor"),c(0,0,1,1))
suntdecVR <- TopoDecfromSolarLunarEvent(JDutfromDate(datenow),c("solstice","solstice"),c(0,1))
lunar <- sky.objects(c('moon','sun'),epoch=datenow,col='red',lty=2)
lunar
lunarVR=lunar
lunarVR$decs[1:4] <- moontdecVR
lunarVR$decs[5:6] <- suntdecVR
lunarVR
titlestring1g=paste("Moon-GeoDec(",datenow,"), ","1deg uncertainty",sep="")
titlestring2g=paste("Moon-GeoDec(",datenow,"), ","2deg uncertainty",sep="")
titlestring1t=paste("Moon-TopoDec(",datenow,"), ","1deg uncertainty",sep="")
titlestring2t=paste("Moon-TopoDec(",datenow,"), ","2deg uncertainty",sep="")
plotCurv(curv1,lunar,xlim=c(-45,-5),main=titlestring1g)
plotCurv(curv1,lunarVR,xlim=c(-45,-5),main=titlestring1t)
plotCurv(curv2,lunar,xlim=c(-45,-5),main=titlestring2g)
plotCurv(curv2,lunarVR,xlim=c(-45,-5),main=titlestring2t)