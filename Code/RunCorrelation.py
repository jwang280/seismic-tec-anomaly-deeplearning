#!/usr/bin/env python3

import os
import glob

class RunCorrelation( object ):
	CORR_FOLDER = 'Correlation'

	def __init__( self, dataRoot ):
		super( RunCorrelation, self ).__init__()
		self.dataRoot = dataRoot
# 		print( 'RunCorrelation::__init__ dataRoot:{}'.format( dataRoot ))

	def runCorr( self ):
# 		print( 'RunCorrelation::runCorr '.format(  ))
		return self.__start( 'OneDayCorrRand', '.csv' )

	def run30MinCorr( self ):
# 		print( 'RunCorrelation::run30MinCorr '.format(  ))
		return self.__start( 'OneDayCorr30MinMeansRand', '-30minmeans.csv' )

	def __start( self, programName, outputName ):
# 		print( '\n\nRunCorrelation::__start \n$$$ programName:{} \n$$$ outputName:{} (suffix) \n$$$ dataRoot:{}'.format( programName, outputName, self.dataRoot ))
		message = None
		for name in sorted( glob.glob( os.path.join( self.dataRoot, "????-??-??" ) ) , reverse=True ):
# 			print( 'RunCorrelation::__start checking folder name:{}'.format( name ))
			date = name.split( os.sep )[-1]
			nameSansDate = os.path.split( name )[0] + os.sep
# 			print( 'RunCorrelation::__start sep:{} nameSansDate:{} date:{}'.format( os.sep, nameSansDate, date ))
			if not self.alreadyCompleted( date, outputName ):
# 				location = name.split( os.sep )[-2]
				datePieces = date.split( '-' )
# 				./OneDayCorrRand -i /tmp/TEC/ -y 2018 -m 02 -d 15 -o /tmp/TEC/Correlation/
				outputFolder = os.path.join( nameSansDate, RunCorrelation.CORR_FOLDER ) + os.sep
				command = './{} -i {} -y {} -m {} -d {} -o {}'.format( programName, nameSansDate, *datePieces, outputFolder )
# 				print( 'RunCorrelation::__start \n$$$ command:{} \n$$$ outputFolder:{}'.format( command, outputFolder ))
				unused_systemResult = os.system( command )
# #				if 0 != systemResult:
# 				print( 'XXXXX--> RunCorrelation::__start systemResult:{}'.format( systemResult ) )
			else:
				print( 'date already completed:{}'.format( date ) )
		return message

	def getFullName( self, date, outputName ):
		fileName = date + outputName # '.corr'-30minmeans
		fullName = os.path.join( self.dataRoot, RunCorrelation.CORR_FOLDER, fileName )
		return fullName

	def alreadyCompleted( self, date, outputName ):
		# TODO: check for a 0-sized file - indicating it didn't run successfully
# 		print( 'RunCorrelation::alreadyCompleted date:{} outputName:{}'.format( date, outputName ))
		response = False
# 		fileName = date + outputName # '.corr'-30minmeans

		folderName = os.path.join( self.dataRoot, RunCorrelation.CORR_FOLDER )
# 		print( 'XXXXX--> RunCorrelation::alreadyCompleted folderName:{} - create if needed'.format( folderName ))
		if not os.path.isdir( folderName ):
			os.mkdir( folderName )

		fullName = self.getFullName( date, outputName ) # os.path.join( self.dataRoot, 'Correlation', fileName )
		if os.path.exists( fullName ):
			response = True
# 		print( 'XXXXX--> RunCorrelation::alreadyCompleted\nfullName:{}\ncheck response:{}'.format( fullName, response ) )
		return response

# def runCorrelation( self ) -> None:
# 	workingFolder = self.configuration[ TEC_NewZealand.CF_DATA_ROOT ]
# 	print( 'TEC_NewZealand::runCorrelation workingFolder:{}'.format( workingFolder ) )
# 	runCorrelation = RunCorrelation.RunCorrelation( workingFolder )
# 	runCorrelation.runCorr()
# 	runCorrelation.run30MinCorr()

def main():
	dataRoot = '/Fred/Flintstone' # /tmp/TEC' # '/seis/prj/tec/data' # /scratch/TEC/GPSTK/DATA/NewZealand'
	app = RunCorrelation( dataRoot )
	app.runCorr()
	app.run30MinCorr()

if __name__ == "__main__":
	main()
