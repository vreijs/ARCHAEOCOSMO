###################################################################
S_SolarEvent <- function(JDNDaysUT, SolarEventType, NoDeltaT = 0) {
  # ' JDNDaysUT [Days]
  # ' SolarEventType (0=SE, 1= MAY, 2=SS, 3= AUG, 4=AE, 5=NOV, 6=WS, 7=FEB)
  # ' NoDeltaT (0,1)
  # ' SolarEvent [Days]
  
  # Dim X(6) As Double
  # Dim serr As String
  
  iflag <- Ephemeris
  if (NoDeltaT == 1) {
    Tijd <- JDNDaysUT
  }
  else {
    Tijd <- JDNDaysUT + S_DeltaT(JDNDaysUT) / D2S
  }
  swe_set_ephe_path(DirEphemeris)
  i <- swe_calc(Tijd, SE_SUN, iflag)
  Longi <- i$xx[1]
  # http://www.hermetic.ch/eqsol/eqsol.htm
  LongGoal <- SolarEventType * 45
  difference <- ((LongGoal - Longi) / 360) * Y2D
  if (difference < 0) {
    difference <- difference + Y2D
  }
  if (difference == 0) {
    SolarEvent <- Tijd
  }
  else {
    #xR=T1 Xl=T2
    XR <- Tijd + difference + 5
    i <- swe_calc(XR, SE_SUN, iflag)
    #Yr=Long1 Yl=Long2
    YR <- LongGoal - i$xx[1]
    if ((LongGoal == 0) && (i$xx[1] > 180)) {
      YR <- -(i$xx[1] - 360)
    }
    XL <- XR - 10
    i = swe_calc(XL, SE_SUN, iflag)
    YL <- LongGoal - i$xx[1]
    if ((LongGoal == 0) && (i$xx[1] > 180)) {
      YL = -(i$xx[1] - 360)
    }
    if ((YL * YR) != 0) {
      while (abs(XR - XL) > epsilon / 100) {
        #Calculate midpoint of domain
        Xm = (XR + XL) / 2
        i <- swe_calc(Xm,
                      SE_SUN,
                      iflag)
        Ym <- LongGoal - i$xx[1]
        if ((LongGoal == 0) && (i$xx[1] > 180)) {
          Ym <- -(i$xx[1] - 360)
        }
        if ((YL * Ym) > 0) {
          #Throw away left half
          XL <- Xm
          YL <- Ym
        }
        else {
          #Throw away right half
          XR <- Xm
          YR <- Ym
        }
      }
      Xm <- (XR + XL) / 2
    }
    else {
      Xm <- 0
    }
    Day <- Xm
  }
  if ((NoDeltaT == 1) || (Day = 0)) {
    Day = Day
  }
  else {
    Day <- Day - DeltaT(Day) / D2S
  }
  return(Day)
}
