####################################################################
# (c) V. Reijs, 2006-2019
# ARCHAEOCOSMO library (leaf from version 1.01)
# astronomical, geodetic and meteorological formula
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program (http://www.fsf.org/licensing/licenses/ );
#    if not, write to the Free Software
#    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
#
# The author (web.victor.reijs@gmail.com ) is interested in constructive feedback.
# http://archaeocosmology.org/eng/archaeocosmology.htm
#
# Explantion of the procdures can also be found at
# http://www.archaeocosmology.org/eng/archaeocosmoprocedures.htm
####################################################################

#software version
Version = "1.01"


#optimization delta
epsilon <- 0.001
deltaJD <- 0.00001


#default waardes
TempDefault <- 15
PressureDefault <- 1013.25
RimDefault <- 0
LapseDefault <- 0.0065
DeltaAppAltDefault <- 1e-7

#lowest apparent altitude to provide
LowestAppAlt <- -3.5

#Switches for different formula/principles
HeightType <- 1 #0=RGO (bottom), 1=average heights (top+bottom)/2
DryRefractSource <- 2 #0=RGO, 1=JOSA, 1972, 2= Ciddor, Werf, 2003,
WetRefractSource <- 0 #0=RGO, 1= HOSI, 2=Ciddor,
PWTSource <- 0 #0=power-law/RGO PL2, 1=Clausius CC2, 2=Ciddor CC4
GravitySource <-
  4 #0=RGO, 1=Wikipedia,2=Exp. Suppl. 1992,3=van der Werf, 4=Hinze/Marcel, 5=Hinze
REarthSource <- 0 #0=RGO (constant), 1=WGS84 method, '2=vander Werf

#Constants needed for refraction calculations
EPS <- 0.0001 #["]
REpsilon <- 0.000000000001 #[%]
#MD <- 28.966 #[kg] Mol weight of dry air Hohenkerk
MD <- 28.964 #[kg] Mol weight of Handbook of Chemistry and Physics
MW <-
  18.016 #[kg] Mol weight of water vapor Handbook of Chemistry and Physics
#GCR <- 8314.36 #[L/kmol/K] Hohenkerk
GCR <- 8314.4598 #[L/kmol/K] vander Werf & 2014 CODATA
DELTA <- 18.36
PwConst <- 0.0000112684
TempNulDiff <- 0.000001
Tropolapse <- 0.0065
C2K <- 273.15 #[K]
Integration <- 0 #0=refraction, 2=phi,3=S, 4=Height, 5=Airmass

# WGS84 ellipsoid constants
# http://w3sli.wcape.gov.za/Surveys/Mapping/wgs84.htm
RA <- 6378136.6   #[m]
Rb <- 6356752.314 #[m]
AverageLat <-
  44.9278424478245 #Latitude when Earthradius is (ra+rb)/2
EarthDefault <- 6378120 #[m]

# angle constants
Deg2Rad <- pi / 180
Rad2Deg <- 1 / Deg2Rad
Min2Deg <- 1 / 60

# time constants
Y2D <- 365.25 #[Day]
D2Y <- 1 / Y2D #[Year]
D2H <- 24 #[Hour]
H2S <- 3600 #[sec]
D2S <- D2H * H2S #[sec]
S2H <- 1 / H2S #[Hour]
JC2D <- 36525 #[Day]
M2S <- 60 #[sec]

MoonInclination <- 5.1453964 #[Deg]
MoonPerturb <- 0.145 #[Deg]
MoonDistance <- 384410.4978 #[km]
MoonAvgPar <- 0.952 #[deg]
MoonMaxPar <- 1.004 #[deg]
MoonMinPar <- 0.9 #[deg]
SunPar <- 0.00224 #[deg]

# average radius of moon/sun disc
AvgRadiusSun <- 15.999 / 60 #[Deg] at 2007 CE or BCE
AvgRadiusMoon <- 15.541 / 60 #[Deg] at 2007 CE or BCE
#AvgRadius <- (AvgRadiusSun + AvgRadiusMoon) / 2 #[Deg]
AvgRadius <- 16 / 60 #[Deg]

###################################################################
ARCHAEOCOSMO <- function() {
  return(Version)
}

###################################################################
#' Determine Topocentric altitude from Apparent altitude
#'
#' The astronomical refraction is determined using the Sinclair's formula. This formula is based
#' on the International Standard Atmosphere (ISA: which by the way does not mean the 'average' atmosphere;-).
#' Due to the uncertainties aorund the atmsopheric condition of the Boundary Layers, the calcuated 
#' refractions will not be very accurate (say with 0.5deg).
#' Below an Apparent altitude of -3.5deg, the Topocentric altitude is made equal.
#'
#' @param AppAlt the Apparent altitude (deg), double
#' @param TempE Temperature at height eyes (C, default=15C), double
#' @param PresE Airpressure at height eyes (mbar, default=1013.25mbar), double
#'
#' @return TopoAlt thee Topocentric altitude (deg), double
#'
#' @examples
#' TopoAltfromAppAlt(0,15,1013.25)
#' #-0.5598886443
#' 
#' TopoAltfromAppAlt(c(-4,-0.5,0,2,20))
#' #-4.0000000000 -1.1817229649 -0.5598886443  1.7010159897 19.9561578991
#'
#' @author Victor Reijs, \email{lists@@archaeocosmology.org}
#' @references Bennett, G.G. 1982. 'The calculation of astronomical refraction in marine navigation', 
#'     Journal of Inst. navigation, Vol 35: pp. 255-59.
#' @seealso \url{http://www.archaeocosmology.org/eng/refract.htm}
#' @export
TopoAltfromAppAlt <- function (AppAlt,
                               TempE = TempDefault,
                               PresE = PressureDefault) {
  functionvector <- data.frame(AppAlt, TempE, PresE)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_TopoAltfromAppAlt(functionvector$AppAlt[i],
                                          functionvector$TempE[i],
                                          functionvector$PresE[i])
  }
  return(ResultVector)
}

###################################################################
S_TopoAltfromAppAlt <- function (AppAlt,
                                 TempE = TempDefault,
                                 PresE = PressureDefault) {
  #  AppAlt [deg]
  # TempE [C]
  # PresE [mbar]
  # TopoAltitudefromAppAlt [deg]
  
  a = TempE
  if (AppAlt >= LowestAppAlt) {
    # Bennett formula, 1982, The calculation of astronomical refraction in marine navigation, page 255-259, formula B
    # When Appalt=17.904104638432, both formula are the same.
    if (AppAlt > 17.904104638432) {
      R = 0.97 / tan(AppAlt * Deg2Rad)
    }
    else {
      R = (34.46 + 4.23 * AppAlt + 0.004 * AppAlt ^ 2) / (1 + 0.505 * AppAlt + 0.0845 * AppAlt ^ 2)
    }
    R = (PresE - 80) / 930 / (1 + 0.00008 * (R + 39) * (TempE - 10)) * R
    Angle = AppAlt - R * Min2Deg
  }
  else {
    Angle = AppAlt
  }
  return(Angle)
}


###################################################################
#' Determine Apparent altitude from Topocentric altitude
#'
#' The astronomical refraction is determined using the inverse of Sinclair's formula. This formula is based
#' on the International Standard Atmosphere (ISA: which by the way does not mean the 'average' atmosphere;-).
#' Due to the uncertainties aorund the atmsopheric condition of the Boundary Layers, the calcuated 
#' refractions will not be very accurate (say with 0.5deg).
#' Below a Topocentric altitude of -3.5deg, the Apparent altitude is made equal.
#'
#' @param TopoAlt the Topocentric altitude (deg), double
#' @param TempE Temperature at height eyes (C, default=15C), double
#' @param PresE Airpressure at height eyes (mbar, default=1013.25mbar), double
#'
#' @return AppAlt de Apparent altitude (deg), double
#'
#' @examples
#' AppAltfromTopoAlt(0,15,1013.25)
#' #0.4721438916
#' 
#' AppAltfromTopoAlt(c(-4,-0.5,0,2,20))
#' #-4.00000000000  0.04956607457  0.47214389159  2.27870879488 20.04373829337
#'
#' @author Victor Reijs, \email{lists@@archaeocosmology.org}
#' @references Bennett, G.G. 1982. 'The calculation of astronomical refraction in marine navigation', 
#'     Journal of Inst. navigation, Vol 35: pp. 255-59.
#' @seealso \url{http://www.archaeocosmology.org/eng/refract.htm}
#' @export
AppAltfromTopoAlt <- function (TopoAlt,
                               TempE = TempDefault,
                               PresE = PressureDefault) {
  functionvector <- data.frame(TopoAlt, TempE, PresE)
  #print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_AppAltfromTopoAlt(functionvector$TopoAlt[i],
                                          functionvector$TempE[i],
                                          functionvector$PresE[i])
  }
  return(ResultVector)
}

###################################################################
S_AppAltfromTopoAlt <- function (TopoAlt,
                                 TempE = TempDefault,
                                 PresE = PressureDefault) {
  # TopoAlt [deg]
  # TempE [C]
  # PresE [mbar]
  # AppAltfromTopoAlt [deg]
  
  # using methodology of Newtown derivatives (analogue to what Swiss Emphemeris uses)
  newAppAlt <- TopoAlt
  newTopoAlt <- 0
  oudAppAlt <- newAppAlt
  oudTopoAlt <- newTopoAlt
  for (i in  0:5) {
    newTopoAlt <-
      newAppAlt - S_TopoAltfromAppAlt(newAppAlt, TempE, PresE)
    verschil <- newAppAlt - oudAppAlt
    oudAppAlt <- newTopoAlt - oudTopoAlt - verschil
    if ((verschil != 0) && (oudAppAlt != 0)) {
      verschil <-
        newAppAlt - verschil * (TopoAlt + newTopoAlt - newAppAlt) / oudAppAlt
    }
    else {
      verschil <- TopoAlt + newTopoAlt
    }
    oudAppAlt <- newAppAlt
    oudTopoAlt <- newTopoAlt
    newAppAlt <- verschil
  }
  Angle <- TopoAlt + newTopoAlt
  if (Angle < LowestAppAlt) {
    ANgle <- TopoAlt
  }
  return(Angle)
}

#' @export
####################################################################
GeoAltfromTopoAlt <- function (TopoAlt, ObjectDist) {
  functionvector <-
    data.frame(TopoAlt, ObjectDist, stringsAsFactors = FALSE)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_GeoAltfromTopoAlt(functionvector$TopoAlt[i],
                                          functionvector$ObjectDist[i])
  }
  return(ResultVector)
}

###################################################################
S_GeoAltfromTopoAlt <- function (TopoAlt, ObjectDist) {
  # TopoAlt [deg]
  # ObjectDist [sun,moonavg,moonnearest,moonfurthest,star,topo]
  # GeoAltfromTopoAlt [deg]
  
  # V. Reijs, 2002; derivative of http://www.stjarnhimlen.se/comp/ppcomp.html#13 when TopoAlt is the base
  Angle <- TopoAlt + S_ParAltfromTopoAlt(TopoAlt, ObjectDist)
  return(Angle)
}

#' @export
###################################################################
TopoAltfromGeoAlt <- function (GeoAlt, ObjectDist) {
  functionvector <-
    data.frame(GeoAlt, ObjectDist, stringsAsFactors = FALSE)
  #print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_TopoAltfromGeoAlt(functionvector$GeoAlt[i],
                                          functionvector$ObjectDist[i])
  }
  return(ResultVector)
}

###################################################################
S_TopoAltfromGeoAlt <- function (GeoAlt, ObjectDist) {
  # GeoAlt [deg]
  # ObjectDist [sun,moonavg,moonnearest,moonfurthest,star,topo]
  # TopoAltfromGeoAlt [deg]
  
  # http://www.stjarnhimlen.se/comp/ppcomp.html#13
  Angle <- GeoAlt - S_ParAltfromGeoAlt(GeoAlt, ObjectDist)
  return(Angle)
}

#' @export
###################################################################
ParAltfromTopoAlt <- function (TopoAlt, ObjectDist) {
  functionvector <-
    data.frame(TopoAlt, ObjectDist, stringsAsFactors = FALSE)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_ParAltfromTopoAlt(functionvector$TopoAlt[i],
                                            functionvector$ObjectDist[i])
  }
  return(ResultVector)
}

###################################################################
S_ParAltfromTopoAlt <- function (TopoAlt, ObjectDist) {
  # TopoAlt [deg]
  # ObjectDist [sun,moonavg,moonnearest,moonfurthest,star,topo]
  # ParAltfromTopoAlt [deg]
  
  ObjectDist <- tolower(ObjectDist)
  TopoAlti <- TopoAlt * Deg2Rad
  mpar <- S_Maxpar(ObjectDist)
  # parallax formula derived from http://www.stjarnhimlen.se/comp/ppcomp.html#13
  Angle = mpar * cos(TopoAlti)
  # compensation for celestial objects determined by V. Reijs, 2005
  if (grepl("moon", ObjectDist)) {
    Angle = Angle - sin(TopoAlti * 2) * (0.0165 * mpar - 0.0078)
  }
  return(Angle)
}

#' @export
###################################################################
ParAltfromGeoAlt <- function (GeoAlt, ObjectDist) {
  functionvector <-
    data.frame(GeoAlt, ObjectDist, stringsAsFactors = FALSE)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_ParAltfromTopoAlt(functionvector$GeoAlt[i],
                                            functionvector$ObjectDist[i])
  }
  return(ResultVector)
}

###################################################################
S_ParAltfromGeoAlt <- function (GeoAlt, ObjectDist) {
  # GeoAlt [deg]
  # ObjectDist [sun,moonavg,moonnearest,moonfurthest,star,topo]
  # ParAltfromGeoAlt [deg]
  
  GeoAlti <- GeoAlt * Deg2Rad
  mpar <- S_Maxpar(ObjectDist)
  # parallax formula coming from http://www.stjarnhimlen.se/comp/ppcomp.html#13
  Angle <- mpar * cos(GeoAlti)
  return(Angle)
}

#' @export
###################################################################
Maxpar <- function (ObjectDist) {
  functionvector <- data.frame(ObjectDist)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_Maxpar(functionvector$ObjectDist[i])
  }
  return(ResultVector)
}

###################################################################
S_Maxpar <- function (ObjectDist) {
  # ObjectDist [sun,moonavg,moonnearest,moonfurthest,star,topo,moonmajor,moonminor,solstice]
  # S_Maxpar [deg]
  
  ObjectDist <- tolower(ObjectDist)
  Parallax <- 0
  # average parallax determined by V. Reijs based on average earth-moon distance
  if (ObjectDist == "moonavg")  {
    Parallax <- MoonAvgPar
  }
  if (ObjectDist == "moonmajor")  {
    Parallax <- MoonMaxPar
  }
  if (ObjectDist == "moonminor")  {
    Parallax <- MoonMinPar
  }
  # maximum parallax determined by V. Reijs based on minimum earth-moon distance
  if (ObjectDist == "moonnearest") {
    Parallax <- MoonMaxPar
  }
  # minimum parallax determined by V. Reijs based on maximum earth-moon distance
  if (ObjectDist == "moonfurthest") {
    Parallax <- MoonMinPar
  }
  # from Russell Simpson, Variability in the Astronomical Refraction of the Rising & Setting Sun, Astronomical Society of the Pacific, 115, page 1256-1261.
  if (ObjectDist == "sun") {
    Parallax <- SunPar
  }
  if (ObjectDist == "solstice") {
    Parallax <- SunPar
  }
  if (ObjectDist == "star") {
    Parallax <- 0
  } #no parallax
  return(Parallax)
}

#' @export
###################################################################
GeoDecfromAppAlt <-
  function(Lat,
           AppAlt,
           Azi,
           Rim = RimDefault,
           ObjectDist,
           TempE = TempDefault,
           PresE = PressureDefault) {
    functionvector <-
      data.frame(Lat,
                 AppAlt,
                 Azi,
                 Rim,
                 ObjectDist,
                 TempE,
                 PresE,
                 stringsAsFactors = FALSE)
    #  print(functionvector)
    ResultVector <- c(0)
    for (i in 1:nrow(functionvector))
    {
      ResultVector[i] = S_GeoDecfromAppAlt(
        functionvector$Lat[i],
        functionvector$AppAlt[i],
        functionvector$Azi[i],
        functionvector$Rim[i],
        functionvector$ObjectDist[i],
        functionvector$TempE[i],
        functionvector$PresE[i]
      )
    }
    return(ResultVector)
  }

###################################################################
S_GeoDecfromAppAlt <-
  function(Lat,
           AppAlt,
           Azi,
           Rim = RimDefault,
           ObjectDist,
           TempE = TempDefault,
           PresE = PressureDefault) {
    # Lat [deg]
    # AppAlt [deg]
    # Azi [deg]
    # Rim [-1,0, 1] -1=bottom, 0=center, 1=top
    # ObjectDist [moonavg, moonnearest,moonfurthest,sun,star,topo]
    # TempE [C]
    # PresE [mbar]
    # GeoDecfromAppAlt [deg]
    
    ObjectDist <- tolower(ObjectDist)
    TopoAlt <- S_TopoAltfromAppAlt(AppAlt, TempE, PresE)
    if (ObjectDist != "topo") {
      GeoAlt <- S_GeoAltfromTopoAlt(TopoAlt, ObjectDist)
    }
    else {
      GeoAlt <- TopoAlt
    }
    if (ObjectDist == "star") {
      Rim = 0
    }
    DecAngle <- S_GeoDecfromGeoAlt(Lat, GeoAlt, Azi, Rim)
    return(DecAngle)
  }

#' @export
###################################################################
TopoDecfromAppAlt <-
  function(Lat,
           AppAlt,
           Azi,
           Rim = RimDefault,
           TempE = TempDefault,
           PresE = PressureDefault) {
    functionvector <-
      data.frame(Lat,
                 AppAlt,
                 Azi,
                 Rim,
                 TempE,
                 PresE,
                 stringsAsFactors = FALSE)
    #  print(functionvector)
    ResultVector <- c(0)
    for (i in 1:nrow(functionvector))
    {
      ResultVector[i] = S_TopoDecfromAppAlt(
        functionvector$Lat[i],
        functionvector$AppAlt[i],
        functionvector$Azi[i],
        functionvector$Rim[i],
        functionvector$TempE[i],
        functionvector$PresE[i]
      )
    }
    return(ResultVector)
  }

###################################################################
S_TopoDecfromAppAlt <-
  function(Lat,
           AppAlt,
           Azi,
           Rim = RimDefault,
           TempE = TempDefault,
           PresE = PressureDefault) {
    # Lat [deg]
    # AppAlt [deg]
    # Azi [deg]
    # Rim [-1,0, 1] -1=bottom, 0=center, 1=top
    # TempE [C]
    # PresE [mbar]
    # DecfromAppAlt [deg]
    
    DecAngle <- S_GeoDecfromAppAlt(Lat,
               AppAlt,
               Azi,
               ObjectDist="topo",
               Rim = RimDefault,
               TempE = TempDefault,
               PresE = PressureDefault)
    return(DecAngle)
  }

#' @export
###################################################################
GeoDecfromGeoAlt <-
  function(Lat,
           GeoAlt,
           Azi,
           Rim = RimDefault) {
    functionvector <- data.frame(Lat, GeoAlt, Azi, Rim)
    #   print(functionvector)
    ResultVector <- c(0)
    for (i in 1:nrow(functionvector))
    {
      ResultVector[i] = S_GeoDecfromGeoAlt(
        functionvector$Lat[i],
        functionvector$GeoAlt[i],
        functionvector$Azi[i],
        functionvector$Rim[i]
      )
    }
    return(ResultVector)
  }

###################################################################
S_GeoDecfromGeoAlt <-
  function (Lat, GeoAlt, Azi, Rim = RimDefault) {
    # Lat [deg]
    # GeoAlt [deg]
    # Azi [deg]
    # Rim [-1,0, 1] -1=bottom, 0=center, 1=top
    # GeoDecfromGeoAlt [deg]
    
    Lati <- Lat * Deg2Rad
    GeoAlti <- (GeoAlt - Rim * AvgRadius) * Deg2Rad
    Azii <- Azi * Deg2Rad
    # http://archaeocosmology.org/eng/accuracy.htm
    DecAngle <-
      asin(sin(Lati) * sin(GeoAlti) + cos(Lati) * cos(GeoAlti) * cos(Azii))
    DecAngle  <- DecAngle  * Rad2Deg
    return(DecAngle)
  }

#' @export
###################################################################
TopoAltfromDip <- function (HObs, HeightDist, Lat = AverageLat) {
  # Hobs [m]
  # HeightDist [m]
  # Latitude [deg]
  # TopeAltfromDip [deg]
  
  functionvector <- data.frame(HObs, HeightDist, Lat)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_TopoAltfromDip(functionvector$HObs[i],
                                       functionvector$HeightDist[i],
                                       functionvector$Lat[i])
  }
  return(ResultVector)
}

###################################################################
S_TopoAltfromDip <- function (HObs, HeightDist, Lat = AverageLat) {
  # Hobs [m]
  # HeightDist [m]
  # Latitude [deg]
  # TopeAltfromDip [deg]
  
  Rear <- REarth(Lat)
  # converted by V. Reijs, 1944 to SI-units and explicit ka from Thom, A., 1973, page 31
  if (HObs >= HeightDist) {
    #using non-approximation of geometric dip angle: http://mintaka.sdsu.edu/GF/explain/atmos_refr/dip.html
    #    ANgle <- -Rad2Deg * arccos(1 / (1 + (HObs - HeightDist) / RA))
    #pure geometrical
    Angle <- -Rad2Deg * acos((Rear + HeightDist) / (Rear + HObs))
  }
  else {
    Angle = NA
  }
  return(Angle)
}

#' @export
###################################################################
AppAltfromHeights <- function (HeightEye,
                               HeightDist,
                               Distance,
                               TempE = TempDefault,
                               PresE = PressureDefault,
                               Lapse = LapseDefault,
                               Restricted = 0,
                               Lat = AverageLat) {
  functionvector <-
    data.frame(HeightEye,
               HeightDist,
               Distance,
               TempE,
               PresE,
               Lapse,
               Restricted,
               Lat)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_AppAltfromHeights(
      functionvector$HeightEye[i],
      functionvector$HeightDist[i],
      functionvector$Distance[i],
      functionvector$TempE[i],
      functionvector$PresE[i],
      functionvector$Lapse[i],
      functionvector$Restricted[i],
      functionvector$Lat[i]
    )
  }
  return(ResultVector)
}

##################################################################
S_AppAltfromHeights <-
  function (HeightEye,
            HeightDist,
            Distance,
            TempE = TempDefault,
            PresE = PressureDefault,
            Lapse = LapseDefault,
            Restricted = 0,
            Lat = AverageLat) {
    # HeightEye [m]
    # HeightDist [m]
    # Distance [m]
    # TempE [C]
    # PresE [mbar]
    # Lapse [K/m]
    # Lat [deg]
    # AppAltfromHeights [deg]
    
    Rear <- S_REarth(Lat)
    Distancei <- Distance / 1000
    #using Thom formula (3.5) with Ra is the earth's radius
    #geometrical approach
    TopoAlt <-
      S_TopoAltfromHeights(HeightEye, HeightDist, Distance, Lat, 1)
    TopoDistance <-
      S_TopoAltfromHeights(HeightEye, HeightDist, Distance, Lat, 2)
    #distance is the goeid based distance X. So there is an approximation used for the ray distance
    Angle <- TopoAlt + S_Rt(TempE, PresE, Lapse, TopoDistance)
    if (Restricted == 0) {
      Dip <- S_AppAltfromDip(HeightEye, 0, TempE, PresE, Lapse, Lat)
      if (Dip > Angle) {
        Angle = Dip
      }
      if (is.na(Dip)) {
        Angle = NA
      }
    }
    return(Angle)
  }

#' @export
###################################################################
TopoAltfromHeights <- function (HeightEye,
                                HeightDist,
                                Distance,
                                Lat = AverageLat) {
  functionvector <- data.frame(HeightEye, HeightDist, Distance, Lat)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_TopoAltfromHeights(
      functionvector$HeightEye[i],
      functionvector$HeightDist[i],
      functionvector$Distance[i],
      functionvector$Lat[i]
    )
  }
  return(ResultVector)
}

###################################################################
S_TopoAltfromHeights <-
  function (HeightEye,
            HeightDist,
            Distance,
            Lat = AverageLat,
            TypeResult = 1) {
    # HeightEye [m]
    # HeightDist [m]
    # Distance [m]
    # Latitude [deg]
    # TypeResult '1=topoalt, 2=topo ray distance
    # TopoAltfromHeights [deg]
    
    Rear <- S_REarth(Lat)
    Phi <- Distance / Rear
    H2 <- Rear + HeightDist
    H1 <- Rear + HeightEye
    if (TypeResult == 1) {
      Angle = Rad2Deg * atan2((cos(Phi) * (H2) - (H1)), (sin(Phi) * (H2)))
    }
    if (TypeResult == 2) {
      Angle = sqrt((cos(Phi) * (H2) - (H1)) ^ 2 + (sin(Phi) * (H2)) ^ 2)
    }
    return(Angle)
  }

###################################################################
S_Rt <- function (TempE = TempDefault,
                PresE = PressureDefault,
                Lapse = LapseDefault,
                Distance) {
  # TempE [K]
  # PresE [mbar]
  # Lapse [K/m]
  # distance [km]
  # Rt [deg]
  
  # Bomford, G., Geodesy, 1980
  #S_Rt = distance * 252 * PresE / (S_Kelvin(TempE) ^ 2) * (0.0342 - Lapse) / RA * Rad2Deg
  #Wikipedia https://en.wikipedia.org/wiki/Atmospheric_refraction
  #S_Rt = distance * 16.3 * PresE / (S_Kelvin(TempE) ^ 2) * (0.0342 - Lapse) / 60 / 60 / 2
  #Thom
  Refract = 0.0083 * RefractConstfromLapse(Lapse) * Distance / 1000 * PresE / (S_Kelvin(TempE) ^ 2)
  return(Refract)
}

#' @export
###################################################################
AppAltfromDip <- function (HObs,
                           HeightDist,
                           TObs = TempDefault,
                           PObs = PressureDefault,
                           Lapse = LapseDefault,
                           Lat = AverageLat) {
  functionvector <-
    data.frame(HObs, HeightDist, TObs, PObs, Lapse, Lat)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_AppAltfromDip(
      functionvector$HObs[i],
      functionvector$HeightDist[i],
      functionvector$TObs[i],
      functionvector$PObs[i],
      functionvector$Lapse[i],
      functionvector$Lat[i]
    )
  }
  return(ResultVector)
}

###################################################################
S_AppAltfromDip <-
  function (HObs,
            HeightDist,
            TObs = TempDefault,
            PObs = PressureDefault,
            Lapse = LapseDefault,
            Lat = AverageLat) {
    # Hobs [m]
    # HeightDist [m]
    # Tobs [C]
    # PObs [mbar]
    # Lapse [K/m]
    # Lat [deg]
    # AppAltfromDip [deg]
    
    Rear <- REarth(Lat)
    Katm <- RefractConstfromLapse(Lapse)
    Ta <- S_Kelvin(TObs)
    if (HObs >= HeightDist) {
      # converted by V. Reijs, 1944 to SI-units and explicit ka from Thom, A., 1973, page 31
      # using approximation of dipangle
      #    Angle <- -0.03203 * ((Hobs - HeightDist) ^ 0.5) * ((1 - 1.848042142 * Katm * PObs / ta / ta) ^ 0.5)
      #using non-approximation of geometric dip angle: http://mintaka.sdsu.edu/GF/explain/atmos_refr/dip.html
      #    TopoAlt <- -Rad2Deg * acos(1 / (1 + (HObs - HeightDist) / (RA + HeightDist)))
      TopoAlt <- S_TopoAltfromDip(HObs, HeightDist, Lat)
      #divide Rearth by (1-k): A. Young
      AppAltRef = -Rad2Deg * acos(1 / (1 + (HObs - HeightDist) / (Rear / (1 - Katm * 0.0238) + HeightDist)))
      Refraction <- TopoAlt - AppAltRef
      #    WeatherComp = (273.15 + 7.2222) ^ 2 / 1012.53021133334 * PObs / Ta / Ta
      WeatherComp <- (273.15 + 15) ^ 2 / 1013.25 * PObs / Ta / Ta
      Refraction2 <- Refraction * WeatherComp
      Angle <- TopoAlt - Refraction2
    }
    #    AppAltfromDip = -Rad2Deg * arccos(1 / (1 + (HObs - HeightDist) / (RA + HeightDist) * (1 - Katm * 0.0238 * WeatherComp)))
    else {
      AppAltfromDip <- NA
    }
    return(Angle)
  }

#' @export
###################################################################
RefractConstfromLapse <- function (Lapse = LapseDefault) {
  # Lapse [K/m]
  # RefractConstfromLapse []
  
  # the 0.0342 from Bomford G. (1980, Geodesy)
  # http://mintaka.sdsu.edu/GF/explain/atmos_refr/bending.html#curvature
  Lapse = (0.0342 - Lapse) / (0.154 * 0.0238)
  return(Lapse)
}

#' @export
###################################################################
StdGeoDec <-
  function (Lat, StdLat, GeoAlt, StdGeoAlt, Azi, StdAzi) {
    functionvector <-
      data.frame(Lat, StdLat, GeoAlt, StdGeoAlt, Azi, StdAzi)
    ResultVector <- c(0)
    for (i in 1:nrow(functionvector))
    {
      ResultVector[i] = S_StdGeoDec(
        functionvector$Lat[i],
        functionvector$StdLat[i],
        functionvector$GeoAlt[i],
        functionvector$StdGeoAlt[i],
        functionvector$Azi[i],
        functionvector$StdAzi[i]
      )
    }
    return(ResultVector)
  }

###################################################################
S_StdGeoDec <-
  function(Lat, StdLat, GeoAlt, StdGeoAlt, Azi, StdAzi) {
    # Lat [deg]
    # StdLat [deg]
    # GeoAlt [deg]
    # StdGeoAlt [deg]
    # Azi [deg]
    # StdAzi [deg]
    # StdGeoDec [deg]
    
    Lati <- Lat * Deg2Rad
    StdLati <- StdLat * Deg2Rad
    GeoAlti <- GeoAlt * Deg2Rad
    StdGeoAlti <- StdGeoAlt * Deg2Rad
    Azii <- Azi * Deg2Rad
    StdAzii <- StdAzi * Deg2Rad
    #http://archaeocosmology.org/eng/accuracy.htm
    GeoDeci <- S_GeoDecfromGeoAlt(Lat, GeoAlt, Azi) * Deg2Rad
    StdDecLat <-
      abs(StdLati / cos(GeoDeci) * (cos(Lati) * sin(GeoAlti) - sin(Lati) * cos(GeoAlti) *
                                      cos(Azii))) * Rad2Deg
    StdDecGeoAlt <-
      abs(StdGeoAlti / cos(GeoDeci) * (sin(Lati) * cos(GeoAlti) - cos(Lati) * sin(GeoAlti) *
                                         cos(Azii))) * Rad2Deg
    StdDecAzi <-
      abs(StdAzii / cos(GeoDeci) * (sin(Lati) * sin(GeoAlti) - cos(Lati) * cos(GeoAlti) *
                                      sin(Azii))) * Rad2Deg
    Error <- sqrt(StdDecLat ^ 2 + StdDecGeoAlt ^ 2 + StdDecAzi ^ 2)
    return(Error)
  }

#' @export
###################################################################
GCenLatfromGDetLat <-
  function (GDetLat) {
    functionvector <-
      data.frame(GDetLat)
    ResultVector <- c(0)
    for (i in 1:nrow(functionvector))
    {
      ResultVector[i] = S_GCenLatfromGDetLat(functionvector$GDetLat[i])
    }
    return(ResultVector)
  }

###################################################################
S_GCenLatfromGDetLat <- function(GDetLat) {
  # ' GDetLat [deg]
  # ' GCenLatfromGDetLat [deg]
  
  # Wikipedia, http://en.wikipedia.org/wiki/Latitude
  Angle <- atan(tan(GDetLat * Deg2Rad) * (Rb / RA) ^ 2) * Rad2Deg
  return(Angle)
}

#' @export
###################################################################
GDetLatfromGCenLat <-
  function (GCenLat) {
    functionvector <-
      data.frame(GCenLat)
    ResultVector <- c(0)
    for (i in 1:nrow(functionvector))
    {
      ResultVector[i] = S_GDetLatfromGCenLat(functionvector$GCenLat[i])
    }
    return(ResultVector)
  }

###################################################################
S_GDetLatfromGCenLat <- function(GCenLat) {
# ' GCenLat [deg]
# ' GDetLatfromGCenLat [deg]

# Geodetic is closer to astronomical laittude and geocentric: https://www.cv.nrao.edu/~rfisher/Ephemerides/earth_rot.html#obs_coord
# Wikipedia, http://en.wikipedia.org/wiki/Latitude relating to WGS84
# Inverse of GCenLatfromGDetLat
Angle <- atan(tan(GCenLat * Deg2Rad) / ((Rb / RA) ^ 2)) * Rad2Deg
return(Angle)}


#' @export
###################################################################
REarth <- function (Lat = AverageLat) {
  # Lat [deg]
  # REarth [m]
  
  functionvector <- data.frame(Lat)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_REarth(functionvector$Lat[i])
  }
  return(ResultVector)
}

###################################################################
S_REarth <- function (Lat = AverageLat) {
  # Lat [deg]
  # REarth [m]
  
  Lati <- Lat * Deg2Rad
  if (REarthSource == 0) {
    Radius = EarthDefault
  }
  if (REarthSource == 1) {
    # http://www.aerobaticsweb.org/SSA/BGA/wg84figs.html
    Radius <-
      1 / (cos(Lati) * (((1 / RA ^ 2) + (tan(
        Lati
      ) / Rb) ^ 2)) ^ 0.5)
  }
  if (REarthSource == 2) {
    # from van der Werf
    Radius = 6356766
  }
  return(Radius)
}

###################################################################
#' Determine the rise angle of the celestial object
#'
#' blabla
#'
#' @param Lat the apparent altitude (deg), double  vector
#' @param GeoDec the geocentric declination (deg), double  vector
#' @param AppAlt the apparent altitude (deg), double vector
#' @param TempE Temperature at Height eyes (C), default=15C, double vector
#' @param PresE Airpressure at height eye (mbar, default=1013.25mbar), double vector
#' @param ObjectDist the name of celestial object ("moonavg","moonnearest","moonfurthest","sun","star","topo"]), charector vector
#' @param Rim the place to be taken on the clestial object's disc (default=0), integer (bottom=-1, centre=0, top=1) vector
#'
#' @return RiseAngle the apparent rise angle (deg), double
#'
#' @examples
#' RiseAngle(55,12,0,10,1000,"moonavg",0) 
#' #28.33387469
#'
#' @export
RiseAngle <-
  function (Lat,
            GeoDec,
            AppAlt,
#            DeltaAppAlt = DeltaAppAltDefault,
            TempE = TempDefault,
            PresE = PressureDefault,
            ObjectDist,
            Rim = RimDefault) {
    functionvector <-
      data.frame(Lat,
                 GeoDec,
                 AppAlt,
#                 DeltaAppAlt,
                 TempE,
                 PresE,
                 ObjectDist,
                 Rim,
                 stringsAsFactors = FALSE)
    print(functionvector)
    ResultVector <- c(0)
    for (i in 1:nrow(functionvector))
    {
      ResultVector[i] = S_RiseAngle(
        functionvector$Lat[i],
        functionvector$GeoDec[i],
        functionvector$AppAlt[i],
#        functionvector$DeltaAppAlt[i],
        functionvector$TempE[i],
        functionvector$PresE[i],
        functionvector$ObjectDist[i],
        functionvector$Rim[i]
      )
    }
    return(ResultVector)
  }

###################################################################
#the DeltaAppAlt argument is removed (made constant) compared to VBA code!
S_RiseAngle <-
  function(Lat,
           GeoDec,
           AppAlt,
#           DeltaAppAlt = DeltaAppAltDefault,
           TempE = TempDefault,
           PresE = PressureDefault,
           ObjectDist,
           Rim = RimDefault) {
    # ' Lat [Deg]
    # ' GeoDec [Deg]
    # ' AppAlt [deg]
    # ' DeltaAppAlt [deg]
    # ' TempE [C]
    # ' PresE [mbar]
    # ' ObjectDist [moonavg,moonnearest,moonfurthest,sun,star,topo]
    # ' Rim [-1,0,1]
    # ' RiseAngle [deg]
    
    GeoDeci <- GeoDec * Deg2Rad
    AppAlt1 <- AppAlt
    AppAlt2 <- AppAlt + DeltaAppAltDefault
    TopoAlt1 <- TopoAltfromAppAlt(AppAlt1, TempE, PresE)
    TopoAlt2 <- TopoAltfromAppAlt(AppAlt2, TempE, PresE)
    GeoAlt1 <- S_GeoAltfromTopoAlt(TopoAlt1, ObjectDist)
    GeoAlt2 <- S_GeoAltfromTopoAlt(TopoAlt2, ObjectDist)
    Azi1 <- S_AzifromGeoAlt(Lat, GeoAlt1, GeoDec, Rim)
    Azi2 <- S_AzifromGeoAlt(Lat, GeoAlt2, GeoDec, Rim)
    DeltaAzii <- (Azi2 - Azi1) * Deg2Rad
    DeltaAppAlti <- DeltaAppAltDefault * Deg2Rad
    # below should be a spherical angle determination, but angles are assumed small enough
    # Reijs, 2006
    Angle <- atan(DeltaAppAlti / DeltaAzii) * Rad2Deg
    return(Angle)
  }

###################################################################
S_AzifromGeoAlt <- function(Lat, GeoAlt, GeoDec, Rim) {
  # ' Lat [deg]
  # ' GeoAlt [deg]
  # ' GeoDec [deg]
  # ' Rim [-1,0, 1] -1=bottom, 0=center, 1=top
  # ' AzifromGeoAlt [deg]
  
  Lati <- Lat * Deg2Rad
  GeoAlti <- (GeoAlt - Rim * AvgRadius) * Deg2Rad
  GeoDeci <- GeoDec * Deg2Rad
  # http://archaeocosmology.org/eng/accuracy.htm
  Hoek <-
    (sin(GeoDeci) - sin(Lati) * sin(GeoAlti)) / (cos(Lati) * cos(GeoAlti))
  #If Abs(Hoek) <= 1 Then
  Angle <- acos(Hoek) * Rad2Deg
  return(Angle)
}

###################################################################
S_AzifromAppAlt <-
  function(Lat,
           AppAlt,
           GeoDec,
           Rim = RimDefault,
           ObjectDist,
           TempE = TempDefault,
           PresE = PressureDefault) {
    # ' Lat [deg]
    # ' AppAlt [deg]
    # ' GeoDec [deg]
    # ' Rim [-1,0, 1] -1=bottom, 0=center, 1=top
    # ' ObjectDist [moonavg, moonnearest,moonfurthest,sun,star,topo]
    # ' TempE [C]
    # ' PresE [mbar]
    # ' AzimuthfromAppAlt [deg]
    
    TopoAlt <- S_TopoAltfromAppAlt(AppAlt, TempE, PresE)
    GeoAlt <- S_GeoAltfromTopoAlt(TopoAlt, ObjectDist)
    Angle <- S_AzifromGeoAlt(Lat, GeoAlt, GeoDec, Rim)
    return(Angle)
  }

#' @export
###################################################################
TopoDecfromSolarLunarEvent <-
  function (JDNDays,
            Object,
            NS) {
    functionvector <-
      data.frame(JDNDays,
                 Object,
                 NS,
                 stringsAsFactors = FALSE)
#    print(functionvector)
    ResultVector <- c(0)
    for (i in 1:nrow(functionvector))
    {
      ResultVector[i] = S_TopoDecfromSolarLunarEvent(
        functionvector$JDNDays[i],
        functionvector$Object[i],
        functionvector$NS[i]
      )
    }
    return(ResultVector)
  }

###################################################################
S_TopoDecfromSolarLunarEvent <- function(JDNDays, Object, NS) {
    
    # ' JDNDays [Day]
    # ' Object [solstice,moonmajor,moonminor]
    # ' NS (0=South-Winter,1=North-Summer)
    # ' TopoDecfromSolarLunarEvent [deg]
    
    Angle <- S_GeoDecfromSolarLunarEvent(JDNDays, Object, NS,"topo")
return(Angle)}

#' @export
###################################################################
GeoDecfromSolarLunarEvent <-
  function (JDNDays,
            Object,
            NS,
            DeclType = "geo",GeoAlt=0,Rim=0,Lat=53) {
    functionvector <-
      data.frame(JDNDays,
                 Object,
                 NS, DeclType,GeoAlt,Rim,Lat,
                 stringsAsFactors = FALSE)
#    print(functionvector)
    ResultVector <- c(0)
    for (i in 1:nrow(functionvector))
    {
      ResultVector[i] = S_GeoDecfromSolarLunarEvent(
        functionvector$JDNDays[i],
        functionvector$Object[i],
        functionvector$NS[i],
        functionvector$DeclType[i],
        functionvector$GeoAlt[i],
        functionvector$Rim[i],
        functionvector$Lat[i]
      )
    }
    return(ResultVector)
  }


###################################################################
#there is a swap of the NS and DeclType arguments compared to VBA code!
S_GeoDecfromSolarLunarEvent <-
  function(JDNDays, Object, NS, DeclType = "geo",GeoAlt=0,Rim=0,Lat=53) {
    # ' JDNDays [Day]
    # ' Object [solstice,moonmajor,moonminor]
    # ' NS (0=South-Winter,1=North-Summer)
    # ' DeclType [geo,topo]
    # ' GeoDecfromSolarLunarEvent [deg]
    
    Perturbation <- 1
    Object <- tolower(Object)
    Angle <- S_Sunobliquity(JDNDays)
    if (Object == "moonmajor") {
      Angle <- Angle + (MoonInclination + Perturbation * MoonPerturb)
    }
    if (Object == "moonminor") {
      Angle <- Angle - (MoonInclination + Perturbation * MoonPerturb)
    }
    if (NS == 0) {
      Angle = -Angle
    }
    if (DeclType == "topo") {
         Angle <- S_TopoDecfromGeoDec(Angle, Object,GeoAlt,Rim,Lat)
    }
    return(Angle)
  }

###################################################################
S_HourAngle <- function(Alt, Dec, Lat) {
  # ' Alt [deg]
  # ' Dec [deg]
  # ' Lat [deg]
  # ' HourAngle [hour]
  
  Alti <- Alt * Deg2Rad
  Deci <- Dec * Deg2Rad
  Lati <- Lat * Deg2Rad
  # from http://star-www.st-and.ac.uk/~fv/webnotes/chapt12.htm
  Angle <-
    ((sin(Alti) - sin(Lati) * sin(Deci)) / cos(Lati) / cos(Deci))
  if (Angle > 1) {
    Angle <- 1
  }
  if (Angle < -1) {
    Angle <- -1
  }
  Angle = acos(Angle) * Rad2Deg / 15
  return(Angle)
}

#does now rok (missing SE)
###################################################################
S_HourAnglefromTopoAlt <-
  function(JDNDaysUT,
           Lat,
           Longitude,
           HeightEye,
           TempE,
           PresE,
           ObjectName,
           AngleTarget) {
    # ' AngleTarget [deg]
    # ' DateObjectAlt [-]
    
    Angle = S_HourAngle(
      AngleTarget,
      S_ObjectLoc(
        JDNDaysUT,
        Lat,
        Longitude,
        HeightEye,
        TempE,
        PresE,
        ObjectName,
        2
      ),
      Lat
    )
    return(Angle)
  }

###################################################################
S_GeoDecfromDay <-
  function(DaysSummer,
           Obl,
           TropYear,
           AnoYear,
           Ecc,
           Perihelion) {
    # ' DaysSummer [-]
    # ' Obl [Deg]
    # ' TropYear [Day]
    # ' AnoYear [Day]
    # ' Ecc [-]
    # ' Perihelion [Day]
    # ' GeoDecfromDay [Deg]
    
    Obli <- Obl * Deg2Rad
    # V. Reijs, 2004, http://archaeocosmology.org/eng/season.htm
    Angle <-
      asin(sin(Obli) * cos((
        360 / TropYear * DaysSummer + Ecc * 2 * Rad2Deg * sin(360 / AnoYear * (DaysSummer - Perihelion) * Deg2Rad)
      ) * Deg2Rad)) * Rad2Deg
    return(Angle)
  }

###################################################################
S_DayfromGeoDec <- function(GeoDec, JDNDays, AfterSummer) {
  # ' GeoDec [Deg]
  # ' JDNDays [Day]
  # ' AfterSummer [0, 1]
  # ' DayfromGeoDec [-]
  
  TropYear <- S_TropicalYear(JDNDays, "mean")
  AnoYear <- S_AnomalisticYear(JDNDays)
  Obli <- S_Sunobliquity(JDNDays)
  Ecc <- S_Eccentricity(JDNDays)
  Perihelion <- S_PerihelionNumber(JDNDays)
  maxerror <- 0.0000001
  richting <- 1
  stap <- 1
  # V. Reijs, http://archaeocosmology.org/ eng / season.htm
  dayoud <- S_DayfromGeoDecSimple(GeoDec, JDNDays)
  if (AfterSummer == 1) {
    dayoud = -dayoud
  }
  if (Obli >= GeoDec) {
    declioud <-
      S_GeoDecfromDay(dayoud, Obli, TropYear, AnoYear, Ecc, Perihelion)
    difoud <- declioud - GeoDec
    if (difoud < 0) {
      signoud = -1
    } else {
      signoud = 1
    }
    while (abs(difoud) > maxerror) {
      daynew <- dayoud + richting * stap
      declinew <-
        S_GeoDecfromDay(daynew, Obli, TropYear, AnoYear, Ecc, Perihelion)
      difnew <- declinew - GeoDec
      if (difnew < 0) {
        signnew = -1
      } else {
        signnew = 1
      }
      if (signnew == signoud) {
        if (abs(difnew) > abs(difoud)) {
          richting = -richting
        }
      }
      else {
        richting = -richting
        stap <- stap / 2
      }
      difoud <- difnew
      signoud <- signnew
      dayoud <- daynew
    }
  }
  else {
    dayoud = NA
  }
  Angle <- dayoud
  return(Angle)
}

###################################################################
S_DayfromGeoDecSimple <- function(GeoDec, JDNDays) {
  # ' GeoDec [Deg]
  # ' JDNDays [Day]
  # ' DayfromGeoDecSimple [-]
  
  anglei <- GeoDec * Deg2Rad
  Obliquityi <- S_Sunobliquity(JDNDays) * Deg2Rad
  # Ruggles, 1999, Astronomy in prehistoric Britain and Ireland, page 24
  if (Obliquityi > anglei) {
    Day = acos(sin(anglei) / sin(Obliquityi)) * Rad2Deg / 0.9856
  }
  else {
    Day = NA
  }
  return(Day)
}

###################################################################
S_DayfromAngle <- function(PathAngle, JDNDays) {
  # ' PathAngle [Deg]
  # ' JDNDays [Day]
  # ' DayfromAngle [-]
  
  TropYear <- S_TropicalYear(JDNDays, "mean")
  AnoYear <- S_AnomalisticYear(JDNDays)
  Ecc <- S_Eccentricity(JDNDays)
  Perihelion <- S_PerihelionNumber(JDNDays)
  maxerror <- 0.0000001
  richting <- 1
  stap <- 1
  # V. Reijs, http://archaeocosmology.org/eng/season.htm
  dayoud <- (PathAngle / 90) * 365.2424 / 4
  angleoud <-
    S_AngleinSunsPath(dayoud, TropYear, AnoYear, Ecc, Perihelion)
  difoud <- angleoud - PathAngle
  if (difoud < 0) {
    signoud = -1
  } else {
    signoud = 1
  }
  while (abs(difoud) > maxerror) {
    daynew <- dayoud + richting * stap
    anglenew <-
      S_AngleinSunsPath(daynew, TropYear, AnoYear, Ecc, Perihelion)
    difnew <- anglenew - PathAngle
    if (difnew < 0) {
      signnew = -1
    } else {
      signnew = 1
    }
    if (signnew == signoud) {
      if (abs(difnew) > abs(difoud)) {
        richting = -richting
      }
    }
    else {
      richting <- -richting
      stap <- stap / 2
    }
    difoud <- difnew
    signoud <- signnew
    dayoud <- daynew
  }
  Day = dayoud
  return(Day)
}

##################################################################
S_GeoAltfromGeoDecHour <- function(Lat, GeoDec, GeoHour) {
  # ' Lat [deg]
  # ' GeoDec [deg]
  # ' GeoHour [deg]
  # ' GeoAltfromGeoDecHour [deg]
  
  Lati <- Lat * Deg2Rad
  GeoHouri <- GeoHour * Deg2Rad
  GeoDeci <- GeoDec * Deg2Rad
  # Astropnomy with personal computer, P. Dufffett-Smith
  Angle = asin(sin(GeoDeci) * sin(Lati) + cos(GeoDeci) * cos(GeoHouri) * cos(Lati))
  Angle = Angle * Rad2Deg
  return(Angle)
}

#' @export
###################################################################
TopoDecfromGeoDec <- function (GeoDec, ObjectDist,GeoAlt=0,Rim=0,Lat=53) {
  
  functionvector <- data.frame(GeoDec, ObjectDist,GeoAlt,Rim,Lat,stringsAsFactors = FALSE)
 # print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector)) {
      ResultVector[i] = S_TopoDecfromGeoDec(functionvector$GeoDec[i],functionvector$ObjectDist[i],functionvector$GeoAlt[i],functionvector$Rim[i],functionvector$Lat[i])
  }
  return(ResultVector)
}

##################################################################
S_TopoDecfromGeoDec <- function(GeoDec, ObjectDist,GeoAlt=0,Rim=0,Lat=53) {
  # GeoDec [deg]
  # ObjectDist [sun,moonavg,moonnearest,moonfurthest,star
  # Gealt [deg]
  # Rim (-1,0,1) at above Alt
  # greographic latitude [deg]
  # TopoDecfromGeoDec [deg]
  
  # print(ObjectDist)
       Angle <- GeoDec - S_ParDecfromGeoAltLat(Lat,GeoAlt,Rim,GeoDec,ObjectDist)
   return (Angle)
}

#' @export
###################################################################
ParDecfromGeoAltLat <- function(Lat=53,GeoAlt=0,Rim=0,GeoDec, ObjectDist) {
  
  functionvector <- data.frame(Lat,GeoAlt,Rim,GeoDec, ObjectDist,stringsAsFactors = FALSE)
  # print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector)) {
    ResultVector[i] = S_TopoDecfromGeoDec(functionvector$Lat[i],functionvector$GeoAlt[i],functionvector$Rim[i],functionvector$GeoDec[i],functionvector$ObjectDist[i])
  }
  return(ResultVector)
}

##################################################################
S_ParDecfromGeoAltLat <- function(Lat=53,GeoAlt=0,Rim=0,GeoDec, ObjectDist) {
  # greographic latitude [deg]
  # Gealt [deg]
  # Rim (-1,0,1) at above Alt
  # GeoDec [deg]
  # ObjectDist [sun,moonavg,moonnearest,moonfurthest,star
  # ParDecfromGeoAltLat [deg]
  
  # print(ObjectDist)
  Lati <- Lat * Deg2Rad
  GeoDeci <- GeoDec * Deg2Rad
  if (ObjectDist == "star") {Rim = 0}
  # http://www.stjarnhimlen.se/comp/ppcomp.html#13
  HA = S_HourAngle((GeoAlt - Rim * AvgRadius), GeoDec, Lat) * 15 * Deg2Rad
  mpar = S_Maxpar(ObjectDist)
  if (Lati != 0) {    
     GHELP <- atan(tan(Lati) / cos(HA))
    ParDecfromGeoAltLat <- mpar * sin(Lati) * sin(GHELP - GeoDeci) / sin(GHELP)}
  else {ParDecfromGeoAltLat <- mpar * sin(-GeoDeci) * cos(HA)}
  return (ParDecfromGeoAltLat)
}


