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
SEFLG_NONUT          	<- as.integer(64) #no nutation, i.e. mean equinox of date 
SEFLG_SPEED          	<- as.integer(256) #high precision speed (analyt. comp.)
SEFLG_NOGDEFL 	<- as.integer(512) #turn off gravitational deflection 
SEFLG_NOABERR 	<- as.integer(1024) # turn off 'annual' aberration of light 
#define SEFLG_ASTROMETRIC (SEFLG_NOABERR|SEFLG_NOGDEFL) // astrometric positions
SEFLG_EQUATORIAL <- as.integer(2048)
SEFLG_XYZ <- as.integer(4096)
SEFLG_RADIANS       	<- as.integer(8192) # coordinates in radians, not degrees 
SEFLG_BARYCTR       	<- as.integer(16384) # barycentric positions 
SEFLG_TOPOCTR <- as.integer(32768)
SEFLG_SIDEREAL<- as.integer(64*1024) # sidereal positions 
SEFLG_ICRS	<- as.integer(128*1024) # ICRS (DE406 reference frame) 
SEFLG_DPSIDEPS_1980     <- as.integer(256*1024) # reproduce JPL Horizons * 1962 - today to 0.002 arcsec
SEFLG_JPLHOR    <- SEFLG_DPSIDEPS_1980
SEFLG_JPLHOR_APPROX     <- as.integer(512*1024) # approximate JPL Horizons 1962 - today

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

