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
# A few functions are linked from Swiss Ephemeris (http://www.astro.com/swisseph/):
#   ' ObjectLoc, PlanetStar and DeltaTSE
# If one does not download Swiss Ephemeris; ObjectLoc and HeliacalAngle, etc.
# are the functions that will not work (the rest is still there)
#
# Explantion of the procdures can also be found at
# http://www.archaeocosmology.org/eng/archaeocosmoprocedures.htm
####################################################################

###################################################################
S_DistanceAngle <- function(LatA, LongA, LatB, LongB) {
  # ' LatA [rad]
  # ' LongA [rad]
  # ' LatB [rad]
  # ' LongB [rad]
  # ' DistanceAngle [rad]
  
  dlon <- LongB - LongA
  dlat <- LatB - LatA
  # Haversine formula
  # http://www.movable-type.co.uk/scripts/GIS-FAQ-5.1.html
  # R.W. Sinnott, Virtues of the Haversine, Sky and Telescope, vol. 68, no. 2, 1984, p. 159
  corde <-
    (sin(dlat / 2)) ^ 2 + cos(LatA) * cos(LatB) * ((sin(dlon / 2)) ^ 2)
  if (corde > 1) {
    corde <- 1
  }
  Angle <- 2 * asin(corde ^ 0.5)
  return(Angle)
}

###################################################################
S_AngleErrorGPS <- function(mErrorGPS, Distance) {
  # ' mErrorGPS [m]
  # ' distance [m]
  # ' AngleErrorGPS [deg]
  
  Error <- atan(mErrorGPS * 0.707 * 2 / Distance) * Rad2Deg
  return(Error)
}

###################################################################
S_BearingPoints <- function(LatA, LongA, LatB, LongB) {
  # ' LatA [deg]
  # ' LongA [deg]
  # ' LatB [deg]
  # ' LongB [deg]
  # ' BearingPoints [deg]
  
  LatAi <- LatA * Deg2Rad
  LatBi <- LatB * Deg2Rad
  LongAi <- LongA * Deg2Rad
  LongBi <- LongB * Deg2Rad
  Distance <- S_DistanceAngle(LatAi, LongAi, LatBi, LongBi)
  # ???, webpage is not available anymore on the web...
  #mbased on spherical law of cosines
  BearingPoints <-
    asin((sin(LatBi) - sin(LatAi) * cos(Distance)) / cos(LatAi) / sin(Distance))
  BearingPoints <- abs(BearingPoints * Rad2Deg)
  Deltalong <- LongBi - LongAi
  Deltalat <- LatBi - LatAi
  if (Deltalong > 0) {
    if (Deltalat > 0) {
      Bearing <-
        90 - BearingPoints
    } else {
      Bearing <- 90 + BearingPoints
    }
  }
  if (Deltalong <= 0) {
    if (Deltalat > 0) {
      Bearing <-
        270 + BearingPoints
    } else {
      Bearing <- 270 - BearingPoints
    }
  }
  return (Bearing)
}
