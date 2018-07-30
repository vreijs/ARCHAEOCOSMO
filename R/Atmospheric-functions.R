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

###################################################################
S_Kelvin <- function (temp = TempDefault) {
  # Temp [C]
  # Kelvin [K]
  
  # http://en.wikipedia.org/wiki/Kelvin
  TempK = temp + C2K
  return(TempK)
}

###################################################################
S_Celcius <- function(temp) {
  # ' Temp [K]
  # ' Celcius [C]
  
  # http://en.wikipedia.org/wiki/Kelvin
  return(temp - C2K)
}
