# Swiss emphemeris constants
DirEphemeris <- "C:\\ARCHAEOCOSMO\\ephe"

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

# #solar eclipse types
# Const SE_ECL_CENTRAL   As Integer = 1
# Const SE_ECL_NONCENTRAL     As Integer = 2
# Const SE_ECL_TOTAL       As Integer = 4
# Const SE_ECL_ANNULAR     As Integer = 8
# Const SE_ECL_PARTIAL    As Integer = 16
# Const SE_ECL_ANNULAR_TOTAL  As Integer = 32
# Const SE_ECL_PENUMBRAL As Integer = 64

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
 Ephemeris <- SEFLG_SWIEPH
 JPLtype <- "de431.eph"

# bits for data conversion with swe_azalt() and swe_azalt_rev()
SE_EQU2HOR   <- as.integer( 1)

# ' for swe_refrac()
SE_TRUE_TO_APP   <-as.integer( 0)
SE_APP_TO_TRUE    <-as.integer(1)

# ' indices for swe_rise_trans()
SE_CALC_RISE  <-as.integer(1)
SE_CALC_SET      <-as.integer(2)
SE_CALC_MTRANSIT      <-as.integer(4)
SE_CALC_ITRANSIT     <-as.integer(8)
SE_BIT_DISC_BOTTOM     <-as.integer(8192)
SE_BIT_DISC_CENTER       <-as.integer(256) #/* to be added to SE_CALC_RISE/SET */
# '/* if rise or set of disc center is */
#   '/* requried, otherwise upper rim*/
SE_BIT_NO_REFRACTION     <-as.integer(512) #/* to be added to SE_CALC_RISE/SET, */
#   '/* if refraction is not to be considered */
 
# 'Heliacal event related constants
HELIACAL_LONG_SEARCH    <-as.integer(128) # ' e.g. for mercury, if no event found, try next synodic period
SE_HELIACAL_HIGH_PRECISION <-as.integer(256) # if not set, program runs in faster low precision mode
SE_HELFLAG_NO_DETAILS         <-as.integer(1024) #return only the optimum time
SE_HELIACAL_AVKIND_VLM         <-as.integer(0)  #using VisLimMagn
SE_HELIACAL_AVKIND_VR          <-as.integer(66560) #using AV method
SE_HELIACAL_AVKIND_PTO         <-as.integer(2 ^ 16) #using AV method
SE_HELIACAL_AVKIND_MIN7       <-as.integer(2 ^ 17) #using AV method
SE_HELIACAL_AVKIND_MIN9      <-as.integer(2 ^ 18) #using AV method
SE_HELIACAL_OPTICAL_PARAMS     <-as.integer(512) #use the optical parameters
SE_HELFLAG_VISLIM_NOMOON       <-as.integer(8192)
SE_HELFLAG_VISLIM_DARK        <-as.integer(4096)
