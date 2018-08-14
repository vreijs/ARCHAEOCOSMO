library("swephR")

ObjectLoc <- function (JDNDaysUT,
                       Lat,
                       Longitude,
                       HeightEye,
                       TempE = TempDefault,
                       PresE = PressureDefault,
                       ObjectName,
                       Angle,
                       NoDeltaT = 1) {
  functionvector <- data.frame(JDNDaysUT,
                               Lat,
                               Longitude,
                               HeightEye,
                               TempE,
                               PresE,
                               ObjectName,
                               Angle,
                               NoDeltaT, stringsAsFactors = FALSE)
  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] <- S_ObjectLoc(
      functionvector$JDNDaysUT[i],
      functionvector$Lat[i],
      functionvector$Longitude[i],
      functionvector$HeightEye[i],
      functionvector$TempE[i],
      functionvector$PresE[i],
      functionvector$ObjectName[i],
      functionvector$Angle[i],
      functionvector$NoDeltaT[i]
    )
  }
  return(ResultVector)
}


###################################################################
S_ObjectLoc <-
  function(JDNDaysUT,
           Lat,
           Longitude,
           HeightEye,
           TempE=TempDefault,
           PresE=PressureDefault,
           ObjectName,
           Angle,
           NoDeltaT = 1) {
    # ' JDNDaysUT [Days]
    # ' Lat [deg]
    # ' Longitude [deg]
    # ' HeightEye [m]
    # ' TempE [C]
    # ' PresE [mbar]
    # ' ObjectName [-]
    # ' Angle (0 = TopoAlt, 1 = Azi, 2=Topo Declination, 3=Topo Rectascension, 4=AppAlt,5=Geo Declination, 6=Geo Rectascension,7=GeoAlt, 8=Latitude,9=Longitude)
    # ' NoDeltaT 1=no DeltaT 0=DeltaT to remove
    # ' ObjectLoc [deg]
    
 #   ObjectName <- tolower(ObjectName)
 #   Loc <-as.double(0)
    iflag <- Ephemeris
    if ((Angle != 8)
        && (Angle != 9)) {
      iflag <- iflag + SEFLG_EQUATORIAL
    }
    if (Angle < 5) {
      iflag <- iflag + SEFLG_TOPOCTR
    }
    if (Angle == 7) {
      Angle <- 0
    }
    geopos <- c(Longitude, Lat, HeightEye)
    swephR:::swe_set_topo(Longitude, Lat, HeightEye)
    swephR:::swe_set_ephe_path(DirEphemeris)
    if (NoDeltaT == 1) {
      tjd_tt <- JDNDaysUT
    }
    else {
      tjd_tt <- JDNDaysUT + S_DeltaT(JDNDaysUT) / D2S
    }
    Planet <- S_DeterObject(ObjectName)
    if (Planet != -1) {
      i <- swephR:::swe_calc(tjd_tt, Planet, iflag)
      #print (i)
    }
    else {
      i <- swephR:::swe_fixstar(ObjectName, tjd_tt, iflag)
    }
    if ((Angle == 2) || (Angle == 5) || (Angle == 8)) {
    print("output swe_calc (i): ")
      print (i)
      print(paste("value of i$xx[2]: ",i$xx[2]))
      Loc <- i$xX[2]
      print(paste("Loc <- i$xx[2]: ",Loc))
    }
    else {
      if ((Angle == 3)
          || (Angle == 6) || (Angle == 9)) {
        Loc <- i$xX[1]
      }
      else {
        xin <- i$xx[1:2]
        i <- swephR:::swe_azalt(JDNDaysUT,
                         SE_EQU2HOR,
                         geopos,
                         PresE,
                         TempE,
                         xin)
      }
      #change altitude due to form of the moon
      #need to add also change to azimuth (depending on position sun!)
      #for now this azi influence is assumed small
      #if Planet == SE_MOON) { #new
      #        xaz(1) = xaz(1) - 0.707 * AvgRadiusMoon / 2      #new
      #        xaz(0) = xaz(0) + 0.707 * AvgRadiusMoon / 2 #new
      #} 'new
      if (Angle == 0) {
        Loc <- i$xaz[2]
      }
      if (Angle == 4) {
        Loc <- AppAltfromTopoAlt(i$xaz[2], TempE, PresE)
      }
      if (Angle == 1) {
        Loc <- i$xaz[2] + 180
        if (Loc >= 360) {
          Loc <- Loc - 360
        }
      }
    }
    return(Loc)
  }

###################################################################
S_DeterObject <- function(ObjectNamei) {
  ObjectName <- tolower(ObjectNamei)
  Obj <- -1
  if (ObjectName == "sun") {
    Obj <- SE_SUN
  }
  if (ObjectName == "pluto") {
    Obj <- SE_PLUTO
  }
  if (ObjectName == "venus") {
    Obj <- SE_VENUS
  }
  if (ObjectName == "mars") {
    Obj <- SE_MARS
  }
  if (ObjectName == "mercury") {
    Obj <- SE_MERCURY
  }
  if (ObjectName == "jupiter") {
    Obj <- SE_JUPITER
  }
  if (ObjectName == "saturn") {
    Obj <- SE_SATURN
  }
  if (ObjectName == "moon") {
    Obj <- SE_MOON
  }
  if (ObjectName == "neptune") {
    Obj <- SE_NEPTUNE
  }
  if (ObjectName == "uranus") {
    Obj <- SE_URANUS
  }
  if (ObjectName == "earth") {
    Obj <- SE_EARTH
  }
  return(Obj)
}
