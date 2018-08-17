EclipseWhenUT <- function (JDNDaysUT,
                           Lat,
                           Longitude,
                           HeightEye,
                           TempE = TempDefault,
                           PresE = PressureDefault,
                           ObjectName,
                           EclipseType,
                           AppAlt = 0,
                           EventPhase = "g") {
  functionvector <- data.frame(
    JDNDaysUT,
    Lat,
    Longitude,
    HeightEye,
    TempE,
    PresE,
    ObjectName,
    EclipseType,
    AppAlt,
    EventPhase,
    stringsAsFactors = FALSE
  )
  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] <- S_EclipseWhenUT(
      functionvector$JDNDaysUT[i],
      functionvector$Lat[i],
      functionvector$Longitude[i],
      functionvector$HeightEye[i],
      functionvector$TempE[i],
      functionvector$PresE[i],
      functionvector$ObjectName[i],
      functionvector$EclipseType[i],  functionvector$AppAlt[i],      functionvector$EventPhase[i]
    )
  }
  return(ResultVector)
}

###################################################################
S_EclipseWhenUT <-
  function(JDNDaysUT,
           Lat,
           Longitude,
           HeightEye,
           TempE = TempDefault,
           PresE = PressureDefault,
           ObjectName,
           EclipseType,
           AppAlt = 0,
           EventPhase = "g") {
    # ' JDNDaysUT [Days]
    # ' Lat [deg]
    # ' Longitude [deg]
    # ' HeightEye [m]
    # ' ObjectName [sun, moon]
    # ' EclipseType (0=all, 1=total, 2=partial, 3=penumbral)
    # ' AppAlt [deg]
    # EventPhase (g, u1,u2,u3, u4)
    # ' EclipseWhenUT [Days]
    
    # Dim tret(10) As Double
    # Dim attr(20) As Double
    # Dim geopos(2) As Double
    # Dim serr As String
    DateDeltaT<-0
    
    EventPhase <- tolower(EventPhase)
    if (EventPhase == "g") {
      EventPhase = 0
    }
    underhor <- AppAlt - AvgRadius
    ObjectName = tolower(ObjectName)
    iflag <- Ephemeris + SEFLG_EQUATORIAL
    geopos <- c(Longitude, Lat, HeightEye)
    ifltype <- 0
    if (ObjectName == "sun") {
      if (EventPhase == "u1") {
        EventPhase <- 1
      }
      if (EventPhase == "u2") {
        EventPhase <- 2
      }
      if (EventPhase == "u3") {
        EventPhase <- 3
      }
      if (EventPhase == "u4") {
        EventPhase <- 4
      }
      if (EclipseType == 1) {
        ifltype <- SE_ECL_TOTAL
      }
      if (EclipseType == 2) {
        ifltype <- SE_ECL_PARTIAL
      }
    }
    if (ObjectName == "moon") {
      if (EventPhase == "u1") {
        EventPhase <- 2
      }
      if (EventPhase == "u2") {
        EventPhase <- 4
      }
      if (EventPhase == "u3") {
        EventPhase <- 5
      }
      if (EventPhase == "u4") {
        EventPhase <- 3
      }
      if (EclipseType == 1) {
        ifltype <- SE_ECL_TOTAL
      }
      if (EclipseType == 2) {
        ifltype <- SE_ECL_PARTIAL
      }
      if (EclipseType == 3) {
        ifltype <- SE_ECL_PENUMBRAL
      }
    }
    swe_set_topo(Longitude, Lat, HeightEye)
    swe_set_ephe_path(DirEphemeris)
    Planet <- S_DeterObject(ObjectName)
    found <- FALSE
    while (found == FALSE) {
      if (Planet == SE_SUN) {
        i <-
          swe_sol_eclipse_when_loc(JDNDaysUT, iflag, geopos, FALSE)
      }
      if (Planet == SE_MOON) {
        eclipsefound <- FALSE
        while (eclipsefound == FALSE) {
          i <- swe_lun_eclipse_when(JDNDaysUT, iflag, ifltype, FALSE)
          vispartial <- FALSE
          vistotal <- FALSE
          vispenumbral <- FALSE
          vismiddle <- FALSE
          locmiddle <-
            S_ObjectLoc(i$tret[1],
                        Lat,
                        Longitude,
                        HeightEye,
                        TempE,
                        PresE,
                        ObjectName,
                        0)
          if (locmiddle > underhor) {
            vismiddle <- TRUE
          }
          if (i$tret[2] != 0)
          {
            locpartialbegin <-
              S_ObjectLoc(i$tret[2],
                          Lat,
                          Longitude,
                          HeightEye,
                          TempE,
                          PresE,
                          ObjectName,
                          0)
            locpartialend <-
              S_ObjectLoc(i$tret[4],
                          Lat,
                          Longitude,
                          HeightEye,
                          TempE,
                          PresE,
                          ObjectName,
                          0)
            if ((locpartialbegin > underhor) ||
                (locpartialend > underhor)) {
              vispartial <- TRUE
            }
          }
          if (i$tret[5] != 0) {
            loctotalbegin <-
              S_ObjectLoc(i$tret[5],
                          Lat,
                          Longitude,
                          HeightEye,
                          TempE,
                          PresE,
                          ObjectName,
                          0)
            loctotalend <-
              S_ObjectLoc(i$tret[6],
                          Lat,
                          Longitude,
                          HeightEye,
                          TempE,
                          PresE,
                          ObjectName,
                          0)
            if ((loctotalbegin > underhor) ||
                (loctotalend > underhor)) {
              vistotal <- TRUE
            }
          }
          if (i$tret[7] != 0) {
            locpenumbralbegin <-
              S_ObjectLoc(i$tret[7],
                          Lat,
                          Longitude,
                          HeightEye,
                          TempE,
                          PresE,
                          ObjectName,
                          0)
            locpenumbralend <-
              S_ObjectLoc(i$tret[8],
                        Lat,
                        Longitude,
                        HeightEye,
                        TempE,
                        PresE,
                        ObjectName,
                        0)
            if ((locpenumbralbegin > underhor) ||
                (locpenumbralend > underhor)) {
              vispenumbral <- TRUE
            }
            if (EclipseType == 1) {
              if (vistotal || vismiddle) {
                eclipsefound <- TRUE
              }
              else {
                JDNDaysUT <- i$tret[1] + 1
              }
            }
            if (EclipseType == 2) {
              if (vispartial || vismiddle) {
                eclipsefound <- TRUE
              }
              else
                JDNDaysUT <- i$tret[1] + 1
            }
          }
          if (EclipseType == 3) {
            if (vispenumbral || vismiddle) {
              eclipsefound <- TRUE
            }
            else {
              JDNDaysUT <- i$tret[1] + 1
            }
          }
          if (EclipseType == 0) {
            if (vispenumbral || vispatrial || vistotal || vismiddle) {
              eclipsefound <- TRUE
            }
            else {
              JDNDaysUT <- i$tret[1] + 1
            }
          }
        }
      }
      if (ifltype != 0) {
        if (0 == (ifltype && i)) {
          JDNDaysUT <- i$tret[1] + 1
        }
        else {
          found <- TRUE
        }
      }
      else {
        found <- TRUE
      }
    }
    DateDeltaT[1] <- -1
    if (EventPhase >= 10) {
      if (Planet == SE_SUii$N) {
        DateDeltaT[1] <- ii$attr[EventPhase - 10 + 1]
      }
      else {
        ii <- swe_lun_eclipse_how(i$tret[1], iflag, geopos, FALSE)
        DateDeltaT[1] <- ii$attr[EventPhase - 10 + 1]
      }
    }
    else {
      DateDeltaT[1] <- i$tret[EventPhase + 1]
    }
    DateDeltaT[2] <- DeltaT(DateDeltaT[1])
    Eclipse <- DateDeltaT[1]
    return(Eclipse)
  }
