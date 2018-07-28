####################################################################
# (c) V. Reijs, 2006-2018
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
# http://www.iol.ie/~geniet/eng/archaeocosmology.htm
#
# A few functions are linked from Swiss Ephemeris (http://www.astro.com/swisseph/):
#   ' ObjectLoc, PlanetStar and DeltaTSE
# If one does not download Swiss Ephemeris; ObjectLoc and HeliacalAngle, etc.
# are the functions that will not work (the rest is still there)
#
# Explantion of the procdures can also be found at
# http://www.archaeocosmology.org/eng/archaeocosmoprocedures.htm
####################################################################

#Determines which algorimths are used
keuze <- "VR" #for DeltaT can be "VR" or "swisseph"
FormAstroRefrac <-
  "sinclair" #for Astronomical refraction can be "bennetth" or "sinclair"
Chapront <- TRUE

#info on LOD/deltaT formula, gained from Simplex method by V. Reijs, 2006
#using Stephenson 1997 data
#StartYear <- 1857.688 #[year]
#Average <- 1.7223697 #[msec/cy]
#Periodicy <- 1554.422 #[year]
#Amplitude <- 3.712246 #[msec]
#Phase <- -0.03652 #[deg]
#using Stephenson&Morrison, 2004 data
StartYear <- 1820 #[year]
Average <- 1.80546834626888 #[msec/cy]
Periodicy <- 1443.67123144531 #[year]
Amplitude <- 3.75606495492684 #[msec]
Phase <- 0 #[deg]

#epochs
#http://www.ephemeris.com/space-time.html
J1900 <- 2415020
J2000 <- 2451545

#Maya parameter
GMTCorrelation <-
  584283 #GMT Correlation ('Modified THompson 2': http://research.famsi.org/date_mayaLC.php
BaseMayaHour <- 0 #???
ClassicSM <-
  29.53333 #Glyph X of Maya Lunar Series, 1986, J.H. Linden, page 134
CopanSM <-
  29.5302 #Glyph X of Maya Lunar Series, 1986, J.H. Linden, page 134
PalenqueSM <-
  29.53086 #Glyph X of Maya Lunar Series, 1986, J.H. Linden, page 134
ModernMayanSM <- 29.53058573 #average value between 682CE and 878CE
ModernSM <- 29.5305805 #average value between 3114BCE and 878CE
BaseHaab <-
  8 #0.0.0.0.0 is equal to 8 : http://mathdl.maa.org/mathDL/46/?pa=content&sa=viewDocument&nodeId=3536&pf=1
BaseTzolkin <-
  4 #0.0.0.0.0 is equal to 4:Ajaw http: // mathdl.maa.org / mathDL / 46 /  ? pa = content &sa = viewDocument & nodeId = 3536 & pf = 1
BaseGlyphG <-
  0  #0.0.0.0.0 is equal to 0:G9: http://research.famsi.org/date_mayaLC.php
BaseGlyphZ <-
  3  #determined to get value 5 as stated in '2012: Science&Prophecy of ancient Maya', Mark van Stone, page 113
BaseCopanSM <-
  CopanSM - 22 #Glyph X of Maya Lunar Series, 1994, J.H. Linden, page 351
BaseCobaSM <- CopanSM - 23 #Glyph X of Coba Stela 1, Stone, page 44
BasePalenqueSM <- PalenqueSM - 24 #1986, J.H. Linden, page 127
BaseModernSM <-
  ModernSM - 11.4948 #based on new moon before basedate using SkyMap
BaseClassicSM <-
  ClassicSM - 2 #extrapolation of copan and palenque to be determined


###################################################################
DateConversion <- function (JDNDays, Yeartype, Epoch) {
  # JDNDays [Day]
  # Yeartype [julian,tropical]
  # Epoch [year]
  # DateCoversion [year]
  
  Yeartype <- tolower(Yeartype)
  if (Epoch == 1900) {
    EpochJDN = J1900
  }
  if (Epoch == 2000) {
    EpochJDN = J2000
  }
  DeltaJDN <- JDNDays - EpochJDN
  if (Yeartype == "julian") {
    Conversion = DeltaJDN * D2Y
  }
  if (Yeartype == "tropical") {
    Conversion = DeltaJDN / TY(DeltaJDN * D2Y / 100 / 2)
  }
  return(Conversion)
}

###################################################################
JDutfromDate <- function (DateString,
                          hour = 0,
                          DeltaCorrelation = 0) {
  functionvector <-
    data.frame(DateString, hour, DeltaCorrelation, stringsAsFactors = FALSE)
  
  #    print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_JDutfromDate(
      functionvector$DateString[i],
      functionvector$hour[i],
      functionvector$DeltaCorrelation[i]
    )
  }
  return(ResultVector)
}

###################################################################
S_JDutfromDate <- function (DateString,
                            hour = 0,
                            DeltaCorrelation = 0) {
  # DateString [yyyy/mm/dd] or [0.0.0.0.0]
  # Hour [-]
  # DeltaCorrelation [Day]
  # JDutfromDate [Days]
  
  Correlation <- GMTCorrelation + DeltaCorrelation
  M <- 1
  D <- 0
  Maya <- 0
  ystring <- DateString
  yend <- unlist(gregexpr(pattern = "/", ystring))[1]
  yendMaya <- unlist(gregexpr(pattern = "\\.", ystring))[1]
  if (yend != -1) {
    Y <- as.numeric(substr(ystring, 1, yend - 1))
    mstring <- substr(ystring, yend + 1, nchar(ystring))
    mend <- unlist(gregexpr(pattern = "/", mstring))[1]
    if (mend != -1) {
      M <- as.numeric(substr(mstring, 1, mend - 1))
      dstring <- substr(mstring, mend + 1, nchar(mstring))
      D <- as.numeric(dstring)
    }
    else {
      M = as.numeric(mstring)
      D = 1
    }
  }
  else {
    if (yendMaya != -1) {
      baktun <- as.numeric(substr(ystring, 1, yendMaya - 1))
      mstring <- substr(ystring, yendMaya + 1, nchar(ystring))
      yendMaya <- unlist(gregexpr(pattern = "\\.", mstring))[1]
      katun <- as.numeric(substr(mstring, 1, yendMaya - 1))
      mstring <- substr(mstring, yendMaya + 1, nchar(mstring))
      yendMaya <- unlist(gregexpr(pattern = "\\.", mstring))[1]
      tun <- as.numeric(substr(mstring, 1, yendMaya - 1))
      mstring <- substr(mstring, yendMaya + 1, nchar(mstring))
      yendMaya <- unlist(gregexpr(pattern = "\\.", mstring))[1]
      winal <- as.numeric(substr(mstring, 1, yendMaya - 1))
      mstring <- substr(mstring, yendMaya + 1, nchar(mstring))
      kin <- as.numeric(mstring)
      Maya <- 1
    }
    else {
      Y <- as.numeric(ystring)
      M <- 1
      D <- 1
    }
  }
  ut <- hour
  if (Maya == 0) {
    stmonth <- sprintf("%02.0f", M)
    stday <- sprintf("%02.0f", D)
    yyear <-
      as.numeric(paste(sprintf("%04.0f", Y), stmonth, stday, sep = ""))
    # P. Bretagnon, Planetary Programs and tables from -4000 to +2800, 1986, page 6
    if (M > 2) {
      Ym <- Y
      mm <- M
    }
    else {
      Ym <- Y - 1
      mm <- M + 12
    }
    dd <- D
    cm <- 0
    if (Ym < 0) {
      cm = -0.75
    }
    dm <- ut / 24
    Bmonth <- 0
    # Gregorian calendar from 1582/10/15, before that Julian calendar
    if (yyear >= 15821015) {
      am <- Intdiffer(Ym / 100)
      Bmonth <- 2 - am + Intdiffer(am / 4)
    }
    JDut <-
      Intdiffer(365.25 * Ym + cm) + Intdiffer(30.6001 * (mm + 1)) + dd + dm + 1720994.5 + Bmonth
  }
  else {
    #GMT correlation
    #Maya date Long Count 0.0.0.0.0 equivalent with 3114 BCE Sept. 6 (Julian Calendar and GMt correlataion) http://en.wikipedia.org/wiki/Mesoamerican_Long_Count_calendar
    JDut <-
      baktun * 144000 + katun * 7200 + tun * 360 + winal * 20 + kin + (ut + BaseMayaHour) / 24 + (Correlation)
  }
  return(JDut)
}

###################################################################
Intdiffer <- function (Number) {
  # number
  # Intdiffer (Intdiffer(-4.98)=-4)
  
  if (Number >= 0 || Number == floor(Number)) {
    newinteger = floor(Number)
  }
  else {
    newinteger = floor(Number) + 1
  }
  return(newinteger)
}

###################################################################
Sunobliquity <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_Sunobliquity(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_Sunobliquity <- function (JDNDays) {
  # JDNDays [Day]
  # Sunobliquity [deg]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  Tbret <- DateConversion(JDNDays, "julian", 2000) / 1000
  if (Chapront) {
    # J Hilton et al, Celest.Mech.Dyn.Astron. 94 (2006), 351
    Tbret <- 10 * Tbret
    obl <-
      (84381.406 + (-46.836769 + (
        -0.0001831 + (0.0020034 + (-0.000000576 + (-0.0000000434) * Tbret) * Tbret) * Tbret
      ) * Tbret) * Tbret) / 3600
  }
  else {
    # P. Bretagnon, Planetary Programs and tables from -4000 to +2800, 1986, page 6
    obl <-
      23.4392911 - 0.130025833 * Tbret - 0.00000430556 * Tbret ^ 2 + 0.000555347 * Tbret ^ 3 - 0.00000142722 * Tbret ^ 4 - 0.000000693528 * Tbret ^ 5 - 0.0000000108472 * Tbret ^ 6 + 0.000000000197778 * Tbret ^ 7
  }
  return(obl)
}

###################################################################
SolarDayOpt <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_SolarDayOpt(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_SolarDayOpt <- function (JDNDays) {
  # JDNDays [Day]
  # SolarDayOpt [Hour]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm#analysis
  OffSetYear <- (S_JDutfromDate(StartYear, 0) - JDNDays) / 365.25
  DayL <-
    -(OffSetYear / 100 * Average) + Amplitude * sin((Phase + 360 * OffSetYear / Periodicy) * Deg2Rad)
  DayL <- 24 + DayL / 1000 / 3600
  return(DayL)
}

###################################################################
SiderealDay <- function (JDNDays, COD = 0) {
  functionvector <- data.frame(JDNDays, COD)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_SiderealDay(functionvector$JDNDays[i], functionvector$COD[i])
  }
  return(ResultVector)
}

###################################################################
S_SiderealDay <- function (JDNDays, COD = 0) {
  # JDNDays [Day]
  # COD [msec/century]
  # SiderealDay [Hour]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  DayL <-
    S_HarmonicSum(S_TropicalYear(JDNDays, "mean") * D2H,
                  S_SolarDay(JDNDays, COD),-1)
  return(DayL)
}

###################################################################
HarmonicSum <- function (PeriodA, PeriodB, PlusMin = 1) {
  functionvector <- data.frame(PeriodA, PeriodB, PlusMin)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_HarmonicSum(functionvector$PeriodA[i],
                                    functionvector$PeriodB[i],
                                    functionvector$PlusMin[i])
  }
  return(ResultVector)
}

###################################################################
S_HarmonicSum <- function (PeriodA, PeriodB, PlusMin = 1) {
  # PeriodA [x]
  # PeriodB [x]
  # PlusMin [-1: minus/same direction, 1: plus/opposite direction]
  # HarmonicSum [x]
  
  PlusMin <- -PlusMin
  HSum <- PeriodA * PeriodB / (PlusMin * PeriodA + PeriodB)
  if (HSum <= 0) {
    HSum = PeriodA * PeriodB / (PeriodA + PlusMin * PeriodB)
  }
  return(HSum)
}

###################################################################
S_HD <- function(PeriodA, PeriodB) {
  HSum <- S_HS(PeriodA,-PeriodB)
  return(HSum)
}

###################################################################
S_HS <- function(PeriodA, PeriodB) {
  HSum <- PeriodA * PeriodB / (PeriodB + PeriodA)
  return(HSum)
}



###################################################################
TropicalYear <- function (JDNDays, TropType = "mean") {
  functionvector <- data.frame(JDNDays, TropType)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_TropicalYear(functionvector$JDNDays[i], functionvector$TropType[i])
  }
  return(ResultVector)
}

###################################################################
S_TropicalYear <- function (JDNDays, TropType = "mean") {
  # JDNDays [Date]
  # TropType [mean,vsop,spring,autumn,summer,winter]
  # TropicalYear [Day]
  
  TropType = tolower(TropType)
  if (TropType == "mean") {
    Epoch <- 2000
    Julcent <- DateConversion(JDNDays, "julian", Epoch) / 100
    YearL <- TY(Julcent)
  }
  if (TropType == "vsop") {
    Epoch <- 2000
    Julmil <- DateConversion(JDNDays, "julian", Epoch) / 1000
    YearL <- TYVSOP(Julmil)
  }
  if (TropType == "spring") {
    Day1 <- DayfromAngle(-90, JDNDays)
    Day2 <- DayfromAngle(270, JDNDays)
    YearL <- Day2 - Day1
  }
  if (TropType == "summer") {
    Day1 <- DayfromAngle(0, JDNDays)
    Day2 <- DayfromAngle(360, JDNDays)
    YearL <- Day2 - Day1
  }
  if (TropType == "autumn") {
    Day1 <- DayfromAngle(90, JDNDays)
    Day2 <- DayfromAngle(450, JDNDays)
    YearL = Day2 - Day1
  }
  if (TropType == "winter") {
    Day1 <- DayfromAngle(180, JDNDays)
    Day2 <- DayfromAngle(540, JDNDays)
    YearL <- Day2 - Day1
  }
  return (YearL)
}

###################################################################
TY <- function (Julcent) {
  # Julcent [Julian Century]
  # TY [Day]
  
  if (Chapront) {
    YearL = 365.2421896698 - 0.00000615359 * Julcent - 0.000000000729 * Julcent ^ 2 + 0.000000000264 * Julcent ^ 3
  }
  else {
    # http://www.treasure-troves.com/astro/TropicalYear.html
    YearL = 365.2421896698 - 0.00000615359 * Julcent - 0.000000000729 * Julcent ^ 2 + 0.000000000264 * Julcent ^ 3
  }
  return(YearL)
}

###################################################################
TYVSOP <- function (Julmil) {
  # Julmil [Julian Millenium]
  # TYVSOP [Day]
  
  #The history of the tropical year, Meeus J. and Savoie, D.
  #British Astronomical association, 102, 1, 1992
  YearL <-
    365.242189623 - 0.000061522 * Julmil - 0.0000000609 * Julmil ^ 2 + 0.00000026525 * Julmil ^ 3
  retunr(YearL)
}

###################################################################
SolarDay <- function (JDNDays, COD = 0) {
  functionvector <- data.frame(JDNDays, COD)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_SolarDay(functionvector$JDNDays[i], functionvector$COD[i])
  }
  return(ResultVector)
}

###################################################################
S_SolarDay <- function (JDNDays, COD = 0) {
  # JDNDays [Day]
  # COD [msec/century]
  # SolarDay [Hour]
  
  if (COD == 0) {
    # determine more complex formula (using Simplex optimization)
    DayL <- S_SolarDayOpt(JDNDays)
  }
  else {
    # Simple lineair formula
    CODi = COD / 1000 * S2H
    # http://www.iol.ie/~geniet/eng/moonfluct.htm
    Julcent <-
      (S_JDttfromJDut(S_JDutfromDate(StartYear, 0), COD) - JDNDays) / 365.25 / 100
    DayL <- 24 - Julcent * CODi
  }
  return(DayL)
}

###################################################################
S_JDttfromJDut <- function (JDNDaysUT, COD = 0) {
  # JDNDaysUT [yyyy/mm/dd]
  # COD [msec/cy]
  # JDttfromJDut [Days]
  
  JDtt <- JDNDaysUT + S_DeltaT(JDNDaysUT, COD) / D2S
  return(JDtt)
}

###################################################################
DeltaT <- function (JDNDays, COD = 0) {
  functionvector <- data.frame(JDNDays, COD)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_DeltaTVR(functionvector$JDNDays[i], functionvector$COD[i])
  }
  return(ResultVector)
}

###################################################################
S_DeltaT <- function (JDNDays, COD = 0) {
  # JDNDays [Days]
  # COD [msec/cy]
  # DeltaT [Sec]
  
  if (keuze == "swisseph") {
    DT <- S_DeltaTSE(JDNDays, COD)
  }
  if (keuze == "VR") {
    DT <- S_DeltaTVR(JDNDays, COD)
  }
  return(DT)
}

###################################################################
S_DeltaTVR <- function (JDNDays, COD = 0) {
  # JDNDays [Day]
  # COD [msec/cy]
  # DeltaTVR [Sec]
  
  # Determined by V. Reijs
  OffSetYear <- (S_JDutfromDate(StartYear, 0) - JDNDays) / 365.25
  if (COD == 0) {
    DT <-
      (OffSetYear ^ 2 / 100 / 2 * Average + Periodicy / 2 / pi * Amplitude * (cos((
        2 * pi * OffSetYear / Periodicy
      )) - 1)) * Y2D
  }
  else {
    DT <- OffSetYear ^ 2 / 100 / 2 * COD * Y2D
  }
  DT = DT / 1000
  return(DT)
}

###################################################################
EarthRotation <- function (JDNDays, COD = 0) {
  functionvector <- data.frame(JDNDays, COD)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_EarthRotation(functionvector$JDNDays[i], functionvector$COD[i])
  }
  return(ResultVector)
}

###################################################################
S_EarthRotation <- function(JDNDays, COD = 0) {
  # JDNDays [Day]
  # COD [msec/century]
  # EarthRotation [Hour]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  Period = S_HarmonicSum(S_LunarSolarPrecession(JDNDays) * Y2D * D2H,
                         S_SiderealDay(JDNDays, COD),
                         1)
  return(Period)
}

###################################################################
LunarSolarPrecession <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  # print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_LunarSolarPrecession(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_LunarSolarPrecession <- function(JDNDays) {
  # JDNDays [Day]
  # LunarSolarPrecession [Year]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  if (Chapront) {
    Tbret <- DateConversion(JDNDays, "julian", 2000) / 100
    #PO3rev01
    #    crev1 = (5038.481507 + Tbret * ((-1.0790069 * 2 + Tbret * (-0.00114045 * 3 + Tbret * (0.000132851 * 4 - Tbret * 0.0000000951 * 5))))) / 3600 / 100
    #    Lrev1 = 360 / crev1
    #PO3rev02
    #    crev2 = (5038.48209 + Tbret * ((-1.0789921 * 2 + Tbret * (-0.0011404 * 3 + Tbret * (0.000132851 * 4 - Tbret * 0.0000000951 * 5))))) / 3600 / 100
    #    Lrev2 = 360 / crev2
    ctp <-
      (5028.7946 + Tbret * (2.224066 + Tbret * (0.0002319 - Tbret * 0.00009412))) / 3600 / 100
    Ltp <- 360 / ctp
    #P03
    cP03pa <-
      (5028.796195 + Tbret * ((
        1.1054348 * 2 + Tbret * (
          0.00007964 * 3 + Tbret * (-0.000023857 * 4 - Tbret * 0.0000000383 * 5)
        )
      ))) / 3600 / 100
    LP03pa <- 360 / cP03pa
    Period <- LP03pa
  }
  #        dif = Ltp - LP03pa
  else {
    #derived from P. Bretagnon, Planetary Programs and tables from -4000 to +2800, 1986, page 6
    Tbret <- DateConversion(JDNDays, "julian", 2000) / 1000
    Period <-
      360 / (
        13.96971278 + 0.030888083 * 2 * Tbret + 0.0000214778 * 3 * Tbret ^ 2 - 0.0000653656 * 4 * Tbret ^ 3 - 0.000000501528 * 5 * Tbret ^ 4 + 0.000000048475 * 6 * Tbret ^ 5 + 0.0000000036375 * 7 * Tbret ^ 6
      ) * 1000
  }
  return(Period)
}

###################################################################
SiderealYear <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_SiderealYear(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_SiderealYear <- function(JDNDays) {
  # JDNDays [Day]
  # SiderealYear [Day]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  if (Chapront) {
    Tbret = DateConversion(JDNDays, "julian", 2000) / 100
    YearL = 365.256362953 + Tbret * (0.0000001139 + Tbret * (-0.000000000076 - 0.00000000000169 * Tbret))
  }
  else {
    YearL = S_HarmonicSum(S_TropicalYear(JDNDays, "mean"),
                          S_LunarSolarPrecession(JDNDays) * Y2D,
                          1)
  }
  return(YearL)
}

###################################################################
EclipticYear <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_EclipticYear(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_EclipticYear <- function(JDNDays) {
  # JDNDays [Day]
  # EclipticYear [Day]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  # using my own methodology of simple periods
  YearL <-
    S_HarmonicSum(S_TropicalYear(JDNDays, "mean"),
                  S_LunarNodalCycle(JDNDays) * Y2D,
                  -1)
  return(YearL)
}

###################################################################
LunarNodalCycle <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_LunarNodalCycle(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_LunarNodalCycle <- function(JDNDays) {
  # JDNDays [Day]
  # LunarNodalCycle [Year]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  Period <-
    S_HD(S_ICRSLunarNodalCycle(JDNDays),
         S_LunarSolarPrecession(JDNDays))
  return(Period)
}

###################################################################
ICRSLunarNodalCycle <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_ICRSLunarNodalCycle(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_ICRSLunarNodalCycle <- function(JDNDays) {
  # JDNDays [Day]
  # ICRSLunarNodalCycle [Year]
  
  # (derived by T. Peters from Chapront [2002], page 704)
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  tret <- DateConversion(JDNDays, "julian", 2000) / 100
  Period = (6793.476501 + tret * (0.0124002 + tret * (0.000022325 - tret * 0.00000013985))) * D2Y
  return(Period)
}

###################################################################
EarthRotationperSiderealYear <- function (JDNDays, COD = 0) {
  functionvector <- data.frame(JDNDays, COD)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_EarthRotationperSiderealYear(functionvector$JDNDays[i], functionvector$COD[i])
  }
  return(ResultVector)
}

###################################################################
S_EarthRotationperSiderealYear <- function (JDNDays, COD = 0) {
  # JDNDays [Day]
  # COD [msec/century]
  # EarthRotationperSiderealYear [-]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  Period = S_SiderealYear(JDNDays) * D2H / S_EarthRotation(JDNDays, COD)
  return(Period)
}

###################################################################
EquatorPrecession <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_EquatorPrecession(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_EquatorPrecession <- function(JDNDays) {
  # JDNDays [Day]
  # EquatorPrecession [Year]
  
  #is same as LunarSolarPrecession
  Period <- S_LunarSolarPrecession(JDNDays)
  return (Period)
}

###################################################################
AnomalisticYear <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_AnomalisticYear(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_AnomalisticYear <- function(JDNDays) {
  # JDNDays [Day]
  # AnomalisticYear [Day]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  # using my own methodology of simple periods
  YearL <-
    S_HarmonicSum(S_TropicalYear(JDNDays, "mean"),
                  S_ClimaticPrecession(JDNDays) * Y2D,
                  1)
  return(YearL)
}

###################################################################
ClimaticPrecession <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_ClimaticPrecession(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_ClimaticPrecession <- function(JDNDays) {
  # JDNDays [Day]
  # ClimaticPrecession [Year]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  dd <- DateConversion(JDNDays, "julian", 1900) / 10000 * Y2D
  # drived from Expl. Suppl, page 98
  Period <-
    (360 / (
      0.0000470684 + 0.0000339 * 2 / 10000 * dd + 0.00000007 * 3 / 10000 * dd ^ 2
    ) * D2Y)
  return(Period)
}

###################################################################
AnomalisticMonth <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_AnomalisticMonth(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_AnomalisticMonth <- function(JDNDays) {
  # JDNDays [Day]
  # AnomalisticMonth [Day]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  # using my own methodology of simple periods
  Period <-
    HarmonicSum(SiderealMonth(JDNDays), LunarApseCycle(JDNDays) * Y2D, 1)
  return(Period)
}

TropicalMonth <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_TropicalMonth(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_TropicalMonth <- function(JDNDays) {
  # JDNDays [Day]
  # TropicalMonth [Day]
  
  if (Chapront) {
    MonthL <-
      HarmonicSum(LunarSolarPrecession(JDNDays) * Y2D,
                  -SiderealMonth(JDNDays),
                  1)
  }
  else {
    # http://www.iol.ie/~geniet/eng/moonfluct.htm
    dd <- DateConversion(JDNDays, "julian", 1900) / 10000 * Y2D
    # drived from Expl. Suppl, page 107
    MonthL <-
      360 / (13.1763965268 - 0.000085 / 10000 * 2 * dd + 0.000000039 / 10000 * 3 * dd ^ 2)
  }
  return(MonthL)
}

SynodicMonth <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_SynodicMonth(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_SynodicMonth <- function(JDNDays) {
  # JDNDays [Day]
  # SynodicMonth [Day]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  # using my own methodology of simple periods
  MonthL <-
    S_HarmonicSum(S_TropicalYear(JDNDays, "mean"),
                  S_TropicalMonth(JDNDays),
                  1)
  return(MonthL)
}

SiderealMonth <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_SiderealMonth(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_SiderealMonth <- function(JDNDays) {
  # JDNDays [Day]
  # SiderealMonth [Day]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  # using my own methodology of simple periods
  if (Chapront) {
    # derived by T. Peters from Chapront [2002], page 704
    tret <- DateConversion(JDNDays, "julian", 2000) / 100
    MonthL <-
      27.32166155356 + tret * (0.000000216673 + tret * (-0.00000000031243 + tret * 1.9989E-12))
  }
  else {
    # using my own methodology of simple periods
    MonthL <-
      S_HarmonicSum(S_LunarSolarPrecession(JDNDays) * Y2D,
                    S_TropicalMonth(JDNDays),
                    1)
  }
  return(MonthL)
}

DraconicMonth <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_DraconicMonth(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_DraconicMonth <- function(JDNDays) {
  # JDNDays [Day]
  # DraconicMonth [Day]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  # using my own methodology of simple periods
  MonthL = S_HarmonicSum(S_LunarNodalCycle(JDNDays) * Y2D,
                         S_TropicalMonth(JDNDays),
                         -1)
  retunr(MonthL)
}

Eccentricity <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_Eccentricity(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_Eccentricity <- function(JDNDays) {
  # JDNDays [Day]
  # Eccentricity [-]
  
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  # from Expl. Suppl, page 98
  Julcent <- DateConversion(JDNDays, "julian", 1900) / 100
  Period <-
    0.01675104 - 0.0000418 * Julcent - 0.000000126 * Julcent ^ 2
  return(Period)
}

PerihelionNumber <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_PerihelionNumber(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_PerihelionNumber <- function(JDNDays) {
  # JDNDays [Day]
  # PerihelionNumber [Day]
  
  # Determined by V. Reijs (using SkyMap)
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  # http://www.iol.ie/~geniet/eng/season.htm
  Julcent <- DateConversion(JDNDays, "julian", 2000)
  Number <- 197.26 + Julcent * 0.017808333
  return(Number)
}

ICRSLunarApseCycle <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_ICRSLunarApseCycle(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_ICRSLunarApseCycle <- function(JDNDays) {
  # JDNDays [Day]
  # ICRSLunarApseCycle [Year]
  
  # (Chapront [2002], page 704)
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  tret <- DateConversion(JDNDays, "julian", 2000) / 100
  Period <-
    (3232.60542496 + tret * (0.0168939 + tret * (0.000029833 - tret * 0.00000018809))) * D2Y
  return(Period)
}

LunarApseCycle <- function (JDNDays) {
  functionvector <- data.frame(JDNDays)
  #  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] = S_LunarApseCycle(functionvector$JDNDays[i])
  }
  return(ResultVector)
}

###################################################################
S_LunarApseCycle <- function(JDNDays) {
  # JDNDays [Day]
  # LunarApseCycle [Year]
  
  # Determined by V. Reijs
  # http://www.iol.ie/~geniet/eng/moonfluct.htm
  Period <-
    S_HS(S_LunarSolarPrecession(JDNDays),
         S_ICRSLunarApseCycle(JDNDays))
  return(Period)
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
  # V. Reijs, http://www.iol.ie/~geniet/eng/season.htm
  dayoud <- (PathAngle / 90) * 365.2424 / 4
  angleoud <-
    S_AngleinSunsPath(dayoud, TropYear, AnoYear, Ecc, Perihelion)
  difoud <- angleoud - PathAngle
  if (difoud < 0) {
    signoud <- -1
  } else {
    signoud <- 1
  }
  while (abs(difoud) > maxerror) {
    daynew <- dayoud + richting * stap
    anglenew <-
      S_AngleinSunsPath(daynew, TropYear, AnoYear, Ecc, Perihelion)
    difnew <- anglenew - PathAngle
    if (difnew < 0) {
      signnew <- -1
    } else {
      signnew <- 1
    }
    if (signnew == signoud) {
      if (abs(difnew) > abs(difoud)) {
        richting <- -richting
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
  return(dayoud)}

###################################################################
S_AngleinSunsPath <- function(DaysSummer, TropYear, AnoYear, Ecc, Perihelion) {
  # ' DaysSummer [-]
  # ' TropYear [Day]
  # ' AnoYear [Day]
  # ' Ecc [-]
  # ' Perihelion [Day]
  # ' AngleinSunsPath [Deg]
  
  # V. Reijs, 2004, http://www.iol.ie/~geniet/eng/season.htm
  Angle <- 360 / TropYear * DaysSummer + Ecc * 2 * Rad2Deg * sin(360 / AnoYear * (DaysSummer - Perihelion) * Deg2Rad)
  return(Angle)}


options(digits = 10)
JDs <- c(123456, 55, 242345, 1000)
SolarDayOpt(c(JDutfromDate(2000))) - 24.00000017
Sunobliquity(c(170686)) - 24.12502276
JDutfromDate("3.0.0.0.0") - 1016283
JDutfromDate("35/12/1") - 1734175.5


# using http://stat.ethz.ch/R-manual/R-devel/library/base/html/dynload.html
x <- dyn.load("swedll32.dll")
x
getLoadedDLLs()
getDLLRegisteredRoutines(x)
#using https://darrenjw.wordpress.com/2010/12/30/calling-c-code-from-r/
#y <- .C("swe_get_tid_acc")
#b <- .C("_swe_deltat@8",as.double(123456))
#z <- .C("_swe_get_tid_acc@0")
