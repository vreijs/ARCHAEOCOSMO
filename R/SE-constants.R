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
# Const SE_TRUE_TO_APP        As Long = 0
# Const SE_APP_TO_TRUE        As Long = 1
# 
# ' indices for swe_rise_trans()
# Const SE_CALC_RISE      As Long = 1
# Const SE_CALC_SET       As Long = 2
# Const SE_CALC_MTRANSIT      As Long = 4
# Const SE_CALC_ITRANSIT      As Long = 8
# Const SE_BIT_DISC_BOTTOM      As Long = 8192
# Const SE_BIT_DISC_CENTER        As Long = 256 '/* to be added to SE_CALC_RISE/SET */
# '/* if rise or set of disc center is */
#   '/* requried, otherwise upper rim*/
# Const SE_BIT_NO_REFRACTION      As Long = 512 '/* to be added to SE_CALC_RISE/SET, */
#   '/* if refraction is not to be considered */
# 
# 'Heliacal event related constants
# Const SE_HELIACAL_LONG_SEARCH       As Long = 128 ' e.g. for mercury, if no event found, try next synodic period
# Const SE_HELIACAL_HIGH_PRECISION    As Long = 256 ' if not set, program runs in faster low precision mode
# Const SE_HELFLAG_NO_DETAILS         As Long = 1024 'return only the optimum time
# Const SE_HELIACAL_AVKIND_VLM        As Long = 0  'using VisLimMagn
# Const SE_HELIACAL_AVKIND_VR         As Long = 66560 'using AV method
# Const SE_HELIACAL_AVKIND_PTO        As Long = 2 ^ 16 '4096 'using AV method
# Const SE_HELIACAL_AVKIND_MIN7       As Long = 2 ^ 17 '8192 'using AV method
# Const SE_HELIACAL_AVKIND_MIN9       As Long = 2 ^ 18 '16384 'using AV method
# Const SE_HELIACAL_OPTICAL_PARAMS    As Long = 512 'use the optical parameters
# Const SE_HELFLAG_VISLIM_NOMOON      As Long = 8192
# Const SE_HELFLAG_VISLIM_DARK        As Long = 4096
# 
