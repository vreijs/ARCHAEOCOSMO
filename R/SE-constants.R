# Swiss emphemeris constants
DirEphemeris <- ""

# planet and body numbers for swe_calc()
SE_SUN <- as.integer(0)
SE_MOON <- as.integer(1)
SE_MERCURY <- as.integer(2)
SE_VENUS <- as.integer(2)
SE_MARS <- as.integer(4)
SE_JUPITER <- as.integer(5)
SE_SATURN <- as.integer(6)
SE_URANUS <- as.integer(7)
SE_NEPTUNE <- as.integer(8)
SE_PLUTO   <- as.integer(9)
SE_EARTH   <- as.integer(14)

# eclipse types
SE_ECL_CENTRAL   <- as.integer(1)
SE_ECL_NONCENTRAL   <- as.integer(2)
SE_ECL_TOTAL       <- as.integer(4)
SE_ECL_ANNULAR    <- as.integer(8)
SE_ECL_PARTIAL   <- as.integer(16)
SE_ECL_ANNULAR_TOTAL <- as.integer(32)
SE_ECL_PENUMBRAL <- as.integer(64)

# iflag values for swe_calc() and swe_fixstar()
SEFLG_JPLEPH <- as.integer(1)
SEFLG_SWIEPH <- as.integer(2)
SEFLG_MOSEPH <- as.integer(4)
SEFLG_HELCTR <- as.integer(8)
SEFLG_TRUEPOS <- as.integer(16)
SEFLG_J2000 <- as.integer(32)
SEFLG_EQUATORIAL <- as.integer(2048)
SEFLG_XYZ <- as.integer(4096)
SEFLG_TOPOCTR <- as.integer(32768)

#determine the type of emphemeris used
Ephemeris <- SEFLG_MOSEPH
JPLtype <- "de431.eph"

# bits for data conversion with swe_azalt() and swe_azalt_rev()
SE_EQU2HOR   <- as.integer(1)

# ' for swe_refrac()
SE_TRUE_TO_APP   <- as.integer(0)
SE_APP_TO_TRUE    <- as.integer(1)

# ' indices for swe_rise_trans()
SE_CALC_RISE  <- as.integer(1)
SE_CALC_SET      <- as.integer(2)
SE_CALC_MTRANSIT      <- as.integer(4)
SE_CALC_ITRANSIT     <- as.integer(8)
SE_BIT_DISC_BOTTOM     <- as.integer(8192)
SE_BIT_DISC_CENTER       <-
  as.integer(256) #/* to be added to SE_CALC_RISE/SET */
# '/* if rise or set of disc center is */
#   '/* requried, otherwise upper rim*/
SE_BIT_NO_REFRACTION     <-
  as.integer(512) #/* to be added to SE_CALC_RISE/SET, */
#   '/* if refraction is not to be considered */

