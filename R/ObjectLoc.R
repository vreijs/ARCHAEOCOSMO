library("swephR")

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
           NoDeltaT = 0) {
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
      print (i)
    }
    else {
      i <- swephR:::swe_fixstar(ObjectName, tjd_tt, iflag)
    }
    if ((Angle == 2)
        || (Angle == 5) || (Angle == 8)) {
      Location <- i$xX[1]
    }
    else {
      if ((Angle == 3)
          || (Angle == 6) || (Angle == 9)) {
        Location <- i$xX[0]
      }
      else {
        xin <- i$xx[1:2]
        i <-
          swephR:::swe_azalt(JDNDaysUT,
                         SE_EQU2HOR,
                         geopos,
                         PresE,
                         TempE)
      }
      #change altitude due to form of the moon
      #need to add also change to azimuth (depending on position sun!)
      #for now this azi influence is assumed small
      #if Planet == SE_MOON) { #new
      #        xaz(1) = xaz(1) - 0.707 * AvgRadiusMoon / 2      #new
      #        xaz(0) = xaz(0) + 0.707 * AvgRadiusMoon / 2 #new
      #} 'new
      if (Angle == 0) {
        Location <- xaz[1]
      }
      if (Angle == 4) {
        Location <- AppAltfromTopoAlt(xaz[1], TempE, PresE)
      }
      if (Angle == 1) {
        Location <- xaz[0] + 180
        if (Location >= 360) {
          Location <- Location - 360
        }
      }
    }
    return(Location)
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