RiseSet <-   function(JDNDaysUT,
                      Lat,
                      Longitude,
                      HeightEye,
                      TempE = TempDefault,
                      PresE = PressureDefault,
                      ObjectName,
                      RSEvent,
                      Rim = RimDefault,
                      AppAlt = 0) {
  functionvector <- data.frame(
    JDNDaysUT,
    Lat,
    Longitude,
    HeightEye,
    TempE,
    PresE,
    ObjectName,
    RSEvent,
    Rim,
    AppAlt,
    stringsAsFactors = FALSE
  )
  print(functionvector)
  ResultVector <- c(0)
  for (i in 1:nrow(functionvector))
  {
    ResultVector[i] <- S_RiseSet(
      functionvector$JDNDaysUT[i],
      functionvector$Lat[i],
      functionvector$Longitude[i],
      functionvector$HeightEye[i],
      functionvector$TempE[i],
      functionvector$PresE[i],
      functionvector$ObjectName[i],
      functionvector$RSEvent[i],
      functionvector$Rim[i],
      functionvector$AppAlt[i]
    )
  }
  return(ResultVector)
}

###################################################################
S_RiseSet <-
  function(JDNDaysUT,
           Lat,
           Longitude,
           HeightEye,
           TempE = TempDefault,
           PresE = PressureDefault,
           ObjectName,
           RSEvent,
           Rim = RimDefault,
           AppAlt = 0) {
    # ' JDNDaysUT [Days]
    # ' Lat [deg]
    # ' Longitude [deg]
    # ' HeightEye [m]
    # ' TempE [C]
    # ' PresE [mbar]
    # ' ObjectName (string)
    # ' RSEvent (1=rise, 2=set,4=upper meridian transit,8=lower meridian transit)
    # ' Rim [-1=bottom,0=center,1=top]
    # ' RiseSet [Day]
    
    # Dim X(6) As Double
    # Dim xin(2) As Double
    # Dim xaz(2) As Double
    # Dim geopos(2) As Double
    # Dim serr As String
    
    # if (substring(ObjectName,1,1) != "+") {
    #   ObjectName <- StrConv(ObjectName, 2)
    # }
    # else {
    #   ObjectName <- ","+Mid(ObjectName, 2)
    # }
    swe_set_ephe_path(DirEphemeris)
    geopos <- c(Longitude, Lat, HeightEye)
    EventType <- RSEvent
    if (Rim == 0) {
      EventType <- EventType + SE_BIT_DISC_CENTER
    }
    if (Rim == -1) {
      EventType <- EventType + SE_BIT_DISC_BOTTOM
    }
    Planet <- S_DeterObject(ObjectName)
    if (Planet != -1) {
      Result <-
        swe_rise_trans_true_hor(JDNDaysUT,
                                Planet,
                                "",
                                Ephemeris,
                                EventType,
                                geopos,
                                PresE,
                                TempE,
                                AppAlt)
    }
    else {
      Result <-
        swe_rise_trans_true_hor(JDNDaysUT,-1,
                                ObjectName,
                                Ephemeris,
                                EventType,
                                geopos,
                                PresE,
                                TempE,
                                AppAlt)
    }
    if (Result$return == 0) {
      Date <- Result$tret
    }
    else {
      Date <- NULL
    }
    return(Date)
  }
