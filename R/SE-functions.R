#this is onging work. At this moment I am not abel to link the swedll32.dll with R. 
# I am investigating
#below ar some experiments;-)
#I udnerstand the Visual Studio compilation does not work with R. 
#Don't know about minGW yet (although testing it was negative also). 


#dyn.load("swedll32.dll")

#S_swe_version <-function() {
nulstring<-rawToChar(as.raw(0x1))
svers<-nulstring
for (i in 1:255) {
svers <- paste(svers,nulstring,sep="")
}
nchar(svers)
tmp <- .C("swe_version",as.character(svers))
tmp
svers
#return(svers)}

#Public Declare Function swe_version Lib "C:\ARCHAEOCOSMO\AddIns\swedll32.dll" _
#Alias "_swe_version@4" ( _
#                        ByVal svers As String _
#) As String