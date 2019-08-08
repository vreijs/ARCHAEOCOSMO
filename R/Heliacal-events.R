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
SE_HELIACAL_AVKIND_PTO         <-
  as.integer(2 ^ 16) #using AV method
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

# choices in Schaefer's model
nL2ftCandle = 0.000000295
ftCandle2nL = 1 / nL2ftCandle
nL2erg = 1.02E-15
erg2nL = 1 / nL2erg #erg to nLambert
Bnight = 1479 #[nL] if determining myself: 1645
scaleHwater = 3000 #[m] Ricchiazzi [1997] 8200 Schaefer [2000]
scaleHrayleigh = 8515 #[m] Su [2003] 8200 Schaefer [2000]
scaleHaerosol = 3745 #m Su [2003] 1500 Schaefer [2000]
scaleHozone = 20000 #[m] Schaefer [2000]
astr2tau = 0.921034037197618  #LN(10 ^ 0.4)
tau2astr = 1 / astr2tau
ActualRA = 0 #switch: If actual RA must be calcualted ActualRA=1
PLSV = 0 #if Planet, Lunar and Stellar Visibility formula is needed PLSV=1
criticalangle = 0 #[deg]
MaxCountSynodicPeriod = 5 #determines how many synodic periods an event will be sought
MaxTryHours = 4
AddAzimuth = 0 #switch, 0 no additional Object azimuth otherwise plus or minus
TimeStepDefault = 1 #[min] step size for finding events
LocalMinStep = 8 #[min] step size for getting over local minimum
AdjustDarkTime = 25 #[min] Time it takes to adjust to darkness: http://en.wikipedia.org/wiki/Adaptation_(eye) 'new
AdjustLightTime = 5 #[min] Time it takes to adjust to light: http://en.wikipedia.org/wiki/Adaptation_(eye) 'new
InclMoonVLM = 1 #1 if Moon need to be included 0 if not.
Schaefer90 = 0 #0 if 50% and 1 if 90% visibility
SchaeferRefDef = 1 #Btwi according to "XI" if 1993 and "XV" if 2000 paper and a number for XV/number


#' @export
HeliacalJDutSE <- function (JDNDaysUTStart,
                            Age,
                            SN,
                            Lat,
                            Longitude,
                            HeightEye,
                            TempE = TempDefault,
                            PresE = PressureDefault,
                            RH,
                            vr,
                            ObjectName,
                            TypeEvent,
                            AVkind = "vlm",
                            Magnify = 1) {
  functionvector <- data.frame(
    JDNDaysUTStart,
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
            Temperature = TempDefault,
            pressure = PressureDefault,
            RH,
            vr,
            ObjectName,
            TypeEvent,
            AVkind = "vlm",
            Magnify = 1) {
    ObjectN <- ObjectName
    dgeo <- c(Longitude, Lat, HeightEye)
    datm <- c(pressure, Temperature, RH, vr)
    dobs <-
      c(Age, SN, GBinocular, Magnify, Magnify * 5, GOpticTrans)
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

# ###################################################################
# S_HeliacalAngle <-
#   function(Magn,
#            Age,
#            SN,
#            AziO,
#            AltM,
#            AziM,
#            JDNDaysUT,
#            AziS,
#            Lat,
#            HeightEye,
#            Temperature,
#            pressure,
#            RH,
#            vr,
#            TypeAngle = 0) {
#     # ' Magn [-]
#     # ' age [Year]
#     # ' SN [-]
#     # ' AziO [deg]
#     # ' AltM [deg]
#     # ' AziM [deg]
#     # ' JDNDaysUT [-]
#     # ' AziS [deg]
#     # ' Lat [deg]
#     # ' HeightEye [m]
#     # ' Temperature [C]
#     # ' Pressure [mbar]
#     # ' RH [%]
#     # ' VR [km]
#     # ' TypeAngle [0=heliacal, 1=Arcus Visonis, 2=Sun's altitude]
#     # ' HeliacalAngle [deg]
#
#     if (PLSV == 1) {
#       if (TypeAngle == 0) {
#         Angle <- criticalangle
#       }
#       if (TypeAngle == 1) {
#         Angle <- criticalangle + Magn * 2.492 + 13.447
#       }
#       if (TypeAngle == 2) {
#         Angle <- -(Magn * 2.492 + 13.447)
#       } #Magn * 1.1 + 8.9
#       return (angle)
#     }
#     Magnify<-1
#     minx <- 2
#     maxx <- 20
#     xmin = 0
#     ymin <- 10000
#     for (X in minx:maxx) {
#       Arc <-
#         S_TopoArcVisionisSE(
#           Magn,
#           Age,
#           SN,
#           X,
#           AziO,
#           AltM,
#           AziM,
#           JDNDaysUT,
#           AziS,
#           Lat,
#           HeightEye,
#           Temperature,
#           pressure,
#           RH,
#           vr,Magnify
#         )
#       if (Arc < ymin) {
#         ymin <- Arc
#         xmin <- X
#       }
#     }
#     XL <- xmin - 1
#     XR <- xmin + 1
#     YR <-
#       S_TopoArcVisionisSE(
#         Magn,
#         Age,
#         SN,
#         XR,
#         AziO,
#         AltM,
#         AziM,
#         JDNDaysUT,
#         AziS,
#         Lat,
#         HeightEye,
#         Temperature,
#         pressure,
#         RH,
#         vr,Magnify
#       )
#     YL <-
#       S_TopoArcVisionisSE(
#         Magn,
#         Age,
#         SN,
#         XL,
#         AziO,
#         AltM,
#         AziM,
#         JDNDaysUT,
#         AziS,
#         Lat,
#         HeightEye,
#         Temperature,
#         pressure,
#         RH,
#         vr,Magnify
#       )
#     # http://en.wikipedia.org/wiki/Bisection_method
#     while (abs(XR - XL) > 0.1) {
#       #Calculate midpoint of domain
#       Xm <- (XR + XL) / 2
#       DELTAx <- 0.025
#       xmd <- Xm + DELTAx
#       Ym <-
#         S_TopoArcVisionisSE(
#           Magn,
#           Age,
#           SN,
#           Xm,
#           AziO,
#           AltM,
#           AziM,
#           JDNDaysUT,
#           AziS,
#           Lat,
#           HeightEye,
#           Temperature,
#           pressure,
#           RH,
#           vr,Magnify
#         )
#       ymd <-
#         S_TopoArcVisionisSE(
#           Magn,
#           Age,
#           SN,
#           xmd,
#           AziO,
#           AltM,
#           AziM,
#           JDNDaysUT,
#           AziS,
#           Lat,
#           HeightEye,
#           Temperature,
#           pressure,
#           RH,
#           vr,Magnify
#         )
#       if (Ym >= ymd) {
#         #Throw away left half
#         XL <- Xm
#         YL <- Ym
#       }
#       else {
#         #Throw away right half
#         XR <- Xm
#         YR <- Ym
#       }
#     }
#     Xm <- (XR + XL) / 2
#     Ym <- (YR + YL) / 2
#     if (TypeAngle == 0) {
#       Angle <- Xm
#     }
#     if (TypeAngle == 1) {
#       Angle <- Ym
#     }
#     if (TypeAngle == 2) {
#       Angle <- Xm - Ym
#     }
#     return(Andle)
#   }
#
#
###################################################################

S_TopoArcVisionisSE <-
  function(Magn,
           Age,
           SN,
           AltO,
           AziO,
           AltM,
           AziM,
           JDNDaysUT,
           AziS,
           Lat,
           HeightEye,
           Temperature,
           pressure,
           RH,
           vr,
           Magnify = 1) {
    dgeo <- c(0, Lat, HeightEye)
    datm <- c(pressure, Temperature, RH, vr)
    dobs <-
      c(Age, SN, GBinocular, Magnify, Magnify * 5, GOpticTrans)
    
    helflag <-
      SE_HELIACAL_HIGH_PRECISION + SE_HELIACAL_OPTICAL_PARAMS
    i <-
      swe_topo_arcus_visionis(JDNDaysUT,
                              dgeo,
                              datm,
                              dobs,
                              helflag,
                              Magn,
                              AziO,
                              AltO,
                              AziS,
                              AziM,
                              AltM)
    if (i$return == 0) {
      TAV = i$tav
    }
    else {
      TAV = i$serr
    }
    return(TAV)
  }
