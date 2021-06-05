#!/usr/bin/env python3

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.interpolate
from datetime import datetime

import os
import sys


class MakeImage( object ):
	def __init__( self, sourceFile ):
		super( MakeImage, self ).__init__()
		self.sourceFile = sourceFile
		sourceFolder, sourceName = os.path.split( sourceFile )
		sourcePrefix = os.path.splitext( sourceName )[0]
		imageFolder = os.path.abspath( os.path.join( sourceFolder, '..', 'Images' ) )
		if '30MinMeans' in sourceName:
			self.outputFile = os.path.join( imageFolder, '30MinMeans', sourcePrefix + '.png' )
		else:
			self.outputFile = os.path.join( imageFolder, sourcePrefix + '.png' )

	def getFullName( self, dataRoot, fileName ):
		fullName = os.path.abspath( os.path.join( dataRoot, '..', 'ArrayData', fileName ) )
		return fullName

	def alreadyCompleted( self, dataRoot, fileName ):
		response = False
		folderName = os.path.abspath( os.path.join( dataRoot, '..', 'ArrayData' ) )
		if not os.path.isdir( folderName ):
			os.mkdir( folderName )
		fullName = self.getFullName( dataRoot, fileName )
		if os.path.exists( fullName ):
			response = True
		return response 

	def _forceAspect( self, ax, aspect=1 ):
		im = ax.get_images()
		extent = im[0].get_extent()
		ax.set_aspect( abs( ( extent[1] - extent[0] ) / ( extent[3] - extent[2] ) ) / aspect )
		
	def get_sunspotNumber( self, sourcePrefix ):
		SunspotFile = pd.read_csv( '../SN_d_tot_V2.0.csv', names = ['year', 'month', 'day', 'decimal year', 'SNvalue', 'SNerror', 'Nb observations', 'x'] )
		SunspotFile["month"] = SunspotFile.month.map( "{:02}".format )
		SunspotFile["day"] = SunspotFile.day.map( "{:02}".format )
		SunspotFile['date'] = SunspotFile['year'].map( str ) + '-' + SunspotFile['month'].map( str ) + '-' + SunspotFile['day'].map( str )
		SunspotDict = dict( zip( SunspotFile.date, SunspotFile.SNvalue ) )
		s = SunspotDict[sourcePrefix] / 250
		return s
	
	def get_scaledDayofYear( self, sourcePrefix ):
		utcDateItem = datetime.strptime( sourcePrefix, '%Y-%m-%d' )
		dayOfYear = utcDateItem.timetuple().tm_yday
		year = datetime.strptime( str( utcDateItem.year ) + '-12-31', '%Y-%m-%d' ).timetuple().tm_yday
		if dayOfYear < year / 4:
			d = 1 - ( ( dayOfYear - 1 ) / 90 )
		elif year / 4 < dayOfYear <= year / 2:
			d = -( ( dayOfYear - 91 ) / 92 )
		elif year / 2 < dayOfYear < year / 3:
			d = -( ( 274 - dayOfYear ) / 91 )
		else:
			d = ( dayOfYear - 274 ) / 92
		return d
	
	def saveImage( self ):
		if os.path.isfile( self.outputFile ):
			print( 'MakeImage::saveImage file:{} already exists - skipping.'.format( self.outputFile ) )
		else:
			df = pd.read_csv( self.sourceFile, sep=',', header=0 )
			val = 1.0 - df[' corr_k'].values
			lon = df[' lon'].values
			lat = df['lat'].values

			val = np.nan_to_num( val )

			# add a 1.0 off on the side somewhere?

			print( 'lat min:{} max:{}'.format( lat.min(), lat.max() ) )
			print( 'lon min:{} max:{}'.format( lon.min(), lon.max() ) )
			print( 'val min:{} max:{}'.format( val.min(), val.max() ) )

			cartCoord = list( zip( lon, lat ) )
			print( 'cartCoord:{}'.format( cartCoord ) )

			X = np.linspace( min( lon ), max( lon ) )
			Y = np.linspace( min( lat ), max( lat ) )
			X, Y = np.meshgrid( X, Y )

			interp = scipy.interpolate.LinearNDInterpolator( cartCoord, val, fill_value=0, )
			Z0 = interp( X, Y )
			
			sourceFolder, sourceName = os.path.split( self.sourceFile )
			sourcePrefix = os.path.splitext( sourceName )[0]
			if not self.alreadyCompleted( sourceFolder, sourceName ):
				s = self.get_sunspotNumber( sourcePrefix )
				d = self.get_scaledDayofYear( sourcePrefix )
				Z00 = [[[Z0[i][j],s,d] for j in range( len( Z0[i] ))] for i in range( len( Z0 ))]
				outputFile = os.path.abspath( os.path.join( sourceFolder, '..', 'ArrayData', sourcePrefix ) )
				np.save( outputFile, Z00 )
			else:
				print( 'date already completed:{}'.format( sourcePrefix ) ) 			
			
			
			
			Z0[len( X ) - 2, 0] = 1.0
			fig = plt.figure()
			ax = fig.add_subplot( 111 )
			plt.xlabel("Longitude")
			plt.ylabel("Latitude")
			extent = [lon.min(), lon.max(), lat.min(), lat.max() ]
			ax.imshow( Z0, extent=extent, origin='lower' )
			self._forceAspect( ax, aspect=0.75 )
			fig.savefig( self.outputFile )

def main():
	response = 1
	print( 'argv:{}'.format( str( sys.argv ) ) )
	sourceFile = os.path.join( '/', 'Users', 'jaywang', 'Desktop', 'GNS_Internship', 'Workspace', 'TEC', 'Correlation', '2018-10-25.csv' )
	obj = MakeImage( sourceFile )
	obj.saveImage()
	return response

if __name__ == '__main__':
	response = main()
	print( '\nDONE - returning {}'.format( response ) )
	sys.exit( response )

