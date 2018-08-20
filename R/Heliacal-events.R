library("swephR")

# 'Heliacal event related constants
HELIACAL_LONG_SEARCH    <-
  as.integer(128) # ' e.g. for mercury, if no event found, try next synodic period
SE_HELIACAL_HIGH_PRECISION <-
  as.integer(256) # if not set, program runs in faster low precision mode
SE_HELFLAG_NO_DETAILS         <-
  as.integer(1024) #return only the optimum time
SE_HELIACAL_AVKIND_VLM         <- as.integer(0)  #using VisLimMagn
SE_HELIACAL_AVKIND_VR          <- as.integer(66560) #using AV method
SE_HELIACAL_AVKIND_PTO         <- as.integer(2 ^ 16) #using AV method
SE_HELIACAL_AVKIND_MIN7       <- as.integer(2 ^ 17) #using AV method
SE_HELIACAL_AVKIND_MIN9      <- as.integer(2 ^ 18) #using AV method
SE_HELIACAL_OPTICAL_PARAMS     <-
  as.integer(512) #use the optical parameters
SE_HELFLAG_VISLIM_NOMOON       <- as.integer(8192)
SE_HELFLAG_VISLIM_DARK        <- as.integer(4096)


# Optical aid info
GOpticMag <- as.double(1) #optical magnification
GOpticTrans <- as.double(0.8) #optical transmission
GBinocular <- as.double(1) #1-binocular 0=monocular
GOpticDia <- as.double(50) #optical object diameter [mm]

HeliacalJDutSE <- function (JDNDaysUTStart,
                            Age,
                            SN,
                            Lat,
                            Longitude,
                            HeightEye,
                            TempE=TempDefault,
                            PresE=PressureDefault,
                            RH,
                            vr,
                            ObjectName,
                            TypeEvent,
                            AVkind = "vlm",
                            Magnify = 1) {
  functionvector <- data.frame(JDNDaysUTStart,
                               Age,
                               SN,
                               Lat,
                               Longitude,
                               HeightEye,
                               TempE,
                               PresE,
                               RH,
                               vr,
                               ObjectName,
                               TypeEvent,
                               AVkind,
                               Magnify,
    stringsAsFactors = FALSE
  )
  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] <- S_HeliacalJDut(
      functionvector$JDNDaysUT[i],
      functionvector$Age[i],
      functionvector$SN[i],
      functionvector$Lat[i],
      functionvector$Longitude[i],
      functionvector$HeightEye[i],
      functionvector$TempE[i],
      functionvector$PresE[i],
      functionvector$RH[i],
      functionvector$vr[i],
      functionvector$ObjectName[i],
      functionvector$TypeEvent[i],
      functionvector$AVkind[i],
      functionvector$Magnify[i]
    )
  }
  return(ResultVector)
}




###################################################################
S_HeliacalJDut <-
  function (JDNDaysUTStart,
            Age,
            SN,
            Lat,
            Longitude,
            HeightEye,
            Temperature=TempDefault,
            pressure=PressureDefault,
            RH,
            vr,
            ObjectName,
            TypeEvent,
            AVkind = "vlm",
            Magnify = 1) {

    ObjectN <- ObjectName
    dgeo <- c(Longitude, Lat, HeightEye)
    datm <- c(pressure, Temperature, RH, vr)
    dobs <- c(Age, SN, GBinocular, Magnify, Magnify * 5, GOpticTrans)
    helflag <-
      SE_HELIACAL_HIGH_PRECISION #+ SE_HELFLAG_NO_DETAILS '+ SE_HELIACAL_OPTICAL_PARAMS
    if (AVkind == "vr") {
      helflag <- helflag + SE_HELIACAL_AVKIND_VR
    }
    if (AVkind == "vlm") {
      helflag <- helflag + SE_HELIACAL_AVKIND_VLM
    }
    if (AVkind == "pto") {
      helflag <- helflag + SE_HELIACAL_AVKIND_PTO
    }
    if (AVkind == "min7") {
      helflag <- helflag + SE_HELIACAL_AVKIND_MIN7
    }
    if (AVkind == "min9") {
      helflag <- helflag + SE_HELIACAL_AVKIND_MIN9
    }
    i <-
      swe_heliacal_ut(JDNDaysUTStart,
                      dgeo,
                      datm,
                      dobs,
                      ObjectN,
                      TypeEvent,
                      helflag)
    if (i$return == 0) {
      if (i$dret[2] == 0) {
        Heliacal <- i$dret[1]
      }
      else {
        Heliacal <- i$dret[2]
      }
    }
    else {
      Heliacal <- i$serr
    }
    return (Heliacal)
  }