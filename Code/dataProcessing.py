#!/usr/bin/env python3

import os
import sys
import shutil
import datetime
# and jupyter, scipy, sklearn, pandas, pandas-datareader, matplotlib, pillow, requests, h5py, nb_conda
# python3 -m pip install --upgrade --user pip

from TECfetchData import TECfetchData
from RunCorrelation import RunCorrelation
from MakeImage import MakeImage
from dateutil import parser
from AddSunspot import AddSunspot

class dataProcessing( object ):
	DATA_ROOT = '../' #'/seis/prj/tec/data' # /media/mjollnir/1TGreen/TEC/data' # /tmp/TEC' # /media/mjollnir/1TGreen/TEC'

	def __init__( self, stepIndex, dataRoot=None ):
		super( dataProcessing, self ).__init__()
		print( 'dataProcessing::__init__'.format() )
		if dataRoot is not None:
			dataProcessing.DATA_ROOT = dataRoot
		self.tecObject = None
		self.stepIndex = stepIndex # [-1, 0, 1 ]
		previousCompletedForwardDate = self.getLatestFolder()
		self.workDate = self.getDateFromDelta( datetime.datetime.strptime( previousCompletedForwardDate, '%Y-%m-%d' ), self.stepIndex )
		self.currentWorkDate = str( self.workDate ).split( ' ' )[ 0 ]
# 		print( 'dataProcessing::__init__ workDate:{} type:{}'.format( self.currentWorkDate, type( self.currentWorkDate ) ) )
		self.message = None

	def fileCleanup( self ) -> None:
# 		print( 'dataProcessing::fileCleanup workDate:{}'.format( self.currentWorkDate ) )
		if self.tecObject is None:
			if self.currentWorkDate is not None:
				self.tecObject = TECfetchData( utcDate=self.currentWorkDate, workingFolder=dataProcessing.DATA_ROOT )
		if self.tecObject is not None:
			self.tecObject.removeIntermediateFiles()

	def getDateFromDelta( self, date, delta ) -> int:
# 		print( 'dataProcessing::getDateFromDelta'.format() )
		return date + datetime.timedelta( days=delta )

	def getNextWorkDate( self ):
# 		print( 'dataProcessing::getNextWorkDate'.format() )
		now = datetime.datetime.now()
		dt = self.getDateFromDelta( now, -1 )
		response = '{}-{}-{}'.format( dt.year, dt.month, dt.day )
# 		print( 'getNextWorkDate response:{} type:{}'.format( response, type( response ) ) )
		return response

	def getLatestFolder( self ):
# 		print( 'dataProcessing::getLatestFolder'.format() ) # yes, there are easier ways! - sorted()[-1] (selective)
		folders = sorted( [ o for o in os.listdir( dataProcessing.DATA_ROOT ) if os.path.isdir( os.path.join( dataProcessing.DATA_ROOT, o ) )] ) # , reverse=True )
# 		print( 'folders:{}'.format( folders ) )
		selectedFolder = None
		for checkFolder in folders:
			checkFolderSplit = checkFolder.split( '-' )
			if 3 == len( checkFolderSplit ):
				if selectedFolder is None:
					selectedFolder = checkFolder
				else:
# 					print( 'getLatestFolder going:{} checkFolder:{} selectedFolder:{}'.format( self.stepIndex, checkFolder, selectedFolder ))
					lastFolderSplit = selectedFolder.split( '-' )
					if self.stepIndex < 0:
						if lastFolderSplit[0] > checkFolderSplit[0]: selectedFolder = checkFolder
						elif lastFolderSplit[0] == checkFolderSplit[0]:
							if lastFolderSplit[1] > checkFolderSplit[1]: selectedFolder = checkFolder
							elif lastFolderSplit[1] == checkFolderSplit[1]:
								if lastFolderSplit[2] > checkFolderSplit[2]: selectedFolder = checkFolder
					else:
						if lastFolderSplit[0] < checkFolderSplit[0]: selectedFolder = checkFolder
						elif lastFolderSplit[0] == checkFolderSplit[0]:
							if lastFolderSplit[1] < checkFolderSplit[1]: selectedFolder = checkFolder
							elif lastFolderSplit[1] == checkFolderSplit[1]:	
								if lastFolderSplit[2] < checkFolderSplit[2]: selectedFolder = checkFolder
		if selectedFolder is None:
			selectedFolder = self.getNextWorkDate()
		return selectedFolder

	def correctForPresent( self ):
		response = 1 # wait default
		now = datetime.datetime.now()
		if self.workDate is None: 
			return response
		delta = now - self.workDate
		if delta.days <= 2 and 1 == self.stepIndex: # are we close to the present and going forward?
# 			print( '/n/nI would normally remove the directory:{}'.format( self.currentWorkDate ) )
			self.purgeFolderIfEmpty( self.currentWorkDate )
			response = 120 # wait long time
		return response

	def runForward( self ) -> bool:
		print( 'dataProcessing::runForward forwardDate:{}'.format( self.currentWorkDate ) )
		self.tecObject = TECfetchData( utcDate=self.currentWorkDate, workingFolder=dataProcessing.DATA_ROOT )
		try:
			if self.tecObject.download(): # GeoNet and UCSD
				#pass
				if self.tecObject.preprocess(): # runs RinSum and ResCor
					if self.tecObject.ionoBias(): # runs IonoBias
						if self.tecObject.tecMaps():
							#print( '!!!!! FAILED dataProcessing::runForward would normally run self.fileCleanup()'.format() ) 
							#self.fileCleanup()
							return True
						else:
							self.message = '!!!!! FAILED to run tecMaps for :{}\n      - {}'.format( self.currentWorkDate, 
																 self.tecObject.message )
					else: self.message = '!!!!! FAILED to run ionoBias for :{}\n      - {}'.format( self.currentWorkDate, self.tecObject.message )
				else: self.message = '!!!!! FAILED to preprocess data for :{}\n      - {}'.format( self.currentWorkDate, self.tecObject.message )
			else: self.message = '!!!!! FAILED to download data for :{}\n      - {}'.format( self.currentWorkDate, self.tecObject.message )
		except Exception as ex:
			print( '!!!!! EXCEPTION in runForward: {}'.format( ex ) )
		if self.message is not None:
			print( 'self.message:{}'.format( self.message ) )
		return self.correctForPresent()

	def runCorrelation( self ):
# 		print( 'dataProcessing::runCorrelation workingFolder:{}'.format( self.currentWorkDate ) )
		runCorrelation = RunCorrelation( dataProcessing.DATA_ROOT )
		self.message = runCorrelation.runCorr()
# 		message2 = runCorrelation.run30MinCorr()
# 		if message1 is not None: self.message = message1 + '\n' + message2
# 		else: self.message = message2
		return 0

	def purgeFolderIfEmpty( self, checkFolder ):
		dateFolder = os.path.join( dataProcessing.DATA_ROOT, checkFolder )
		tecDataFolder = os.path.join( dateFolder, TECfetchData.outFolder )
# 		processedFolder = os.path.join( dateFolder, TECfetchData.processedFolder )
		alldataFolder = os.path.join( dateFolder, TECfetchData.alldataFolder )

		if os.path.isdir( dateFolder ):
			if os.path.isdir( tecDataFolder ):
				tecDataFolderLength = len( os.listdir( tecDataFolder ) )
# 				print( 'purgeFolderIfEmpty checking dateFolder:{} tecDataFolderLength:{}'.format( dateFolder, tecDataFolderLength ) )
				if 1000 < tecDataFolderLength: # got some or all of the data
# 					if os.path.isdir( processedFolder ): shutil.rmtree( processedFolder )
					if os.path.isdir( alldataFolder ): 
						shutil.rmtree( alldataFolder )
					print( 'dataProcessing::purgeFolders skipping:{} folder has data:{}'.format( dateFolder, tecDataFolderLength ) )
				else:
					print( 'dataProcessing::purgeFolders removing:{}'.format( dateFolder ) )
					# is there data in alldata? might want to keep it
# 					if os.path.isdir( processedFolder ): shutil.rmtree( processedFolder )
# 					if os.path.isdir( alldataFolder ): shutil.rmtree( alldataFolder )
# 					if os.path.isdir( alldataFolder ):
# 						if 0 < len( os.listdir( alldataFolder ) ):
# 							pass # keep at present for debug purposes - why wasn't the data processed?
# 						else:
# 							shutil.rmtree( dateFolder )
# 					else: # don't have any data
					shutil.rmtree( dateFolder )
			else:
				shutil.rmtree( dateFolder )

# 			if os.path.isdir( tecDataFolder ):
# 				if len( os.listdir( tecDataFolder ) ) < 2880: # Correlation routines require all 2880 files, but some have fewer amounts... thinking
# 					print( 'dataProcessing::purgeFolderIfEmpty removing:{} for insufficient:{} len:{}'.format( checkFolder, tecDataFolder, len( os.listdir( tecDataFolder ) ) ) )
# 				else:
# 					print( 'dataProcessing::purgeFolderIfEmpty skipping:{}'.format( tecDataFolder ) )
# 			else:
# 				if 1 == self.stepIndex:
# 					shutil.rmtree( dateFolder )
# 					print( 'dataProcessing::purgeFolderIfEmpty removing:{} for no:{}'.format( checkFolder, tecDataFolder ) )

	def purgeFolders( self ):
# 		print( 'dataProcessing::purgeFolders'.format() )
		folders = sorted( [ o for o in os.listdir( dataProcessing.DATA_ROOT ) if os.path.isdir( os.path.join( dataProcessing.DATA_ROOT, o ) )], reverse=True )
# 		print( 'folders:{}'.format( folders ) )
		for checkFolder in folders:
# 			print( 'dataProcessing::purgeFolders checking:{}'.format( checkFolder ) )
			checkFolderSplit = checkFolder.split( '-' )
			if 3 == len( checkFolderSplit ):
				self.purgeFolderIfEmpty( checkFolder )
		return 1

	def getMissingFolders( self ):
		response = []
# 		print( 'dataProcessing::getMissingFolder'.format() )
		folders = sorted( [ o for o in os.listdir( dataProcessing.DATA_ROOT ) if os.path.isdir( os.path.join( dataProcessing.DATA_ROOT, o ) )], reverse=True )
# 		print( 'folders:{}'.format( folders ) )
# 		selectedFolder = None
		nextFolder = None
		for checkFolder in folders:
			checkFolderSplit = checkFolder.split( '-' )
			if 3 == len( checkFolderSplit ):
				if nextFolder is None:
					nextFolder = checkFolder # possibly easier ways, but can only look at the form xxxx-xx-xx
				else:
# 					print( 'checkFolder:{}'.format( checkFolder ) )
					nextDateFolder = self.getDateFromDelta( datetime.datetime.strptime( checkFolder, '%Y-%m-%d' ), -1 )
	# workDate = self.getDateFromDelta( datetime.datetime.strptime( previousCompletedForwardDate, '%Y-%m-%d' ), self.stepIndex )
					nextDateFolder = str( nextDateFolder ).split( ' ' )[ 0 ]
	# 				nextDateFolder =  nextDateFolder.split( ' ' )[0]
# 					print( 'checking:{} nextDateFolder:{}'.format( checkFolder, nextDateFolder ))
					nextFolder = os.path.join( dataProcessing.DATA_ROOT, nextDateFolder )
					if not os.path.isdir( nextFolder ):
						response.append( os.path.split( nextFolder )[1] )
# 						break
		print( 'dataProcessing::getMissingFolder response:{}'.format( response ) )
		return list( reversed( response ) )

	def dateToDay( self, v ):
		return int( parser.parse( v ).strftime( '%s' ) ) // 86400

	def dayToDate( self, n ):
		return datetime.datetime.utcfromtimestamp( n * 86400 ).isoformat()[:10]

	def fillInMissing( self ):
		startFolders = [ o for o in os.listdir( dataProcessing.DATA_ROOT ) if os.path.isdir( os.path.join( dataProcessing.DATA_ROOT, o ) )]
		folders = []
		for checkFolder in startFolders:
			splitFolder = checkFolder.split( '-' )
			if 3 == len( splitFolder ):
				folders.append( checkFolder )
		folders = sorted( folders, reverse=True )
# 		print( 'folders:{}'.format( folders ) )
		folder = folders[0]
		lastFolder = folders[-1]
		startFolderNumber = self.dateToDay( folder )
		lastFolderNumber = self.dateToDay( lastFolder )
		for folderNumber in range( startFolderNumber, lastFolderNumber, -1 ):
			checkFolder = self.dayToDate( folderNumber )
			dateFolder = os.path.join( dataProcessing.DATA_ROOT, checkFolder )
			workThisFolder = False
			if os.path.isdir( dateFolder ):
				tecDataFolder = os.path.join( dateFolder, TECfetchData.outFolder )
# 				processedFolder = os.path.join( dateFolder, TECfetchData.processedFolder )
# 				alldataFolder = os.path.join( dateFolder, TECfetchData.alldataFolder )
# 				processedDir = os.path.join( dataProcessing.DATA_ROOT, checkFolder,
				if os.path.exists( tecDataFolder ):
					tecDataFolderLength = len( os.listdir( tecDataFolder ) )
# 				print( 'purgeFolderIfEmpty checking dateFolder:{} tecDataFolderLength:{}'.format( dateFolder, tecDataFolderLength ) )
					if tecDataFolderLength < 2880: # got some or all of the data
						workThisFolder = True
# 					filesInProcessed = len( [name for name in os.listdir( processedFolder ) if os.path.isfile( os.path.join( processedFolder, name ) )] )
# 					if 0 == filesInProcessed: workThisFolder = True
			else: workThisFolder = True
			if workThisFolder:
				self.currentWorkDate = checkFolder
				self.runForward()

	def makeTecMaps( self ):
# 		print( 'dataProcessing::makeTecMaps '.format() )
		# get all names in the Correlation folder
		correlationFolder = os.path.join( dataProcessing.DATA_ROOT, 'Correlation' )
		testFiles = sorted( [ o for o in os.listdir( correlationFolder ) if os.path.isfile( os.path.join( correlationFolder, o ) )], reverse=True )
		for filename in testFiles:
			if filename.endswith( '.csv' ):
				fullFilename = os.path.join( correlationFolder, filename )
# 				print( 'dataProcessing::makeTecMaps filename:{}'.format( fullFilename ) )
				mi = MakeImage( fullFilename )
				mi.saveImage()
		return 1
	
	def RunAddSunspot( self ):
		correlationFolder = os.path.join( dataProcessing.DATA_ROOT, 'Correlation' )
		testFiles = sorted( [ o for o in os.listdir( correlationFolder ) if os.path.isfile( os.path.join( correlationFolder, o ) )], reverse=True )
		for filename in testFiles:
			if filename.endswith( '.csv' ):
				fullFilename = os.path.join( correlationFolder, filename )
				RunAddSunspot = AddSunspot( fullFilename )
				self.message = RunAddSunspot.addSunspot()
		return 0	

# 	def sendEmail( self ):
# 		print( 'dataProcessing::sendEmail'.format() )
# 		if self.message is not None:
# 			print( '$$$$\tsending email' )
# 			recipientList = [ 'rchuso@gmail.com' ]
# 			subject = 'TEC failure automated report'
# 			es = SendEmail( account='fred.flintstone@gmx.com' )
# 			es.sendMessage( recipientList, subject, self.message )

def getPidFilePath( stepIndex ):
	response = os.path.join( dataProcessing.DATA_ROOT, 'NewZealandPid' + str( stepIndex ) )
	print( 'main::getPidFilePath response:{}'.format( response ) )
	return response

def writePidFile( stepIndex ):
	pidFile = getPidFilePath( stepIndex )
	os.makedirs( os.path.split( pidFile )[0], exist_ok=True )
	pid = str( os.getpid() )
	print( 'main::writePidFile pidFile:{} pid:{}'.format( pidFile, pid ) )
	with open( pidFile, 'w' ) as fOut:
		fOut.write( pid )

def removePidFile( stepIndex ):
	print( 'main::removePidFile '.format() )
	os.unlink( getPidFilePath( stepIndex ) )

def isAlreadyRunning( stepIndex ):
	print( 'main::isAlreadyRunning '.format() )
	response = False
	pidfile = getPidFilePath( stepIndex )
	if os.path.isfile( pidfile ): response = True
	print( 'main::isAlreadyRunning response:{}'.format( response ) )
	return response

def sendEmail( message=None ):
	print( 'main::sendEmail '.format() )
	#recipientList = [ 'jwa280@uclive.ac.nz' ]
	#subject = 'TEC problem detected'
	#if message is None: message = 'Confirm the program is running again next hour.'
	#es = SendEmail( account='fred.flintstone@gmx.com' )
	#es.sendMessage( recipientList, subject, message )

def gotThisFlag( flag ):
	response = False
	if flag in sys.argv: response = True
	if response: print( 'main::gotThisFlag found {}'.format( flag ) )
	return response

def gotHelpFlag(): return gotThisFlag( '-h' )
def gotPurgeFlag(): return gotThisFlag( '-p' )
def gotFillInFlag(): return gotThisFlag( '-f' )
def gotDirReverseFlag(): return gotThisFlag( '-r' )
def gotMakeImagesFlag(): return gotThisFlag( '-i' )
def gotCorrelationFlag(): return gotThisFlag( '-c' )
def gotAddSunspotFlag(): return gotThisFlag( '-s' )

def main():
	response = 1
	print( 'argv:{}'.format( str( sys.argv ) ) )

	if gotHelpFlag():
		print( 'Options:'.format() )
		print( '\t-h: show this help and exit'.format() )
		print( '\t-p purge existing folders that are incomplete'.format() )
		print( '\t-f fill in empty folders'.format() )
		print( '\t-i create images'.format() )
		print( '\t-c run correlations'.format() )
		print( '\t<<None>> traverse forward'.format() )
		print( '\t-r traverse reverse'.format() )
	elif gotPurgeFlag():
		print( 'main gotPurgeFlag'.format() )
		app = dataProcessing( 0 )
		response = app.purgeFolders()
	elif gotMakeImagesFlag():
		print( 'main gotMakeImagesFlag'.format() )
		app = dataProcessing( 2 )
		response = app.makeTecMaps()
	elif gotCorrelationFlag():
		print( 'main gotCorrelationFlag'.format() )
		app = dataProcessing( 0 )
		response = app.runCorrelation()
	elif gotAddSunspotFlag():
		print( 'main gotAddSunspotFlag'.format() )
		app = dataProcessing( 0 )
		response = app.RunAddSunspot()
	elif gotFillInFlag():
		print( 'main gotFillInFlag'.format() )
		app = dataProcessing( 0 )
		response = app.fillInMissing()
	else:
		if gotDirReverseFlag(): stepIndex = -1
		else: stepIndex = 1
		print( 'main::stepIndex:{}'.format( stepIndex ) )
		if isAlreadyRunning( stepIndex ): sendEmail( 'the application "{}" appears to be already running: {}'.format( stepIndex, sys.argv ) )
		else:
			print( 'main '.format() )
			app = dataProcessing( stepIndex )
			writePidFile( stepIndex )
			try: response = app.runForward()
			except: print( 'caught exception --- ignoring it.'.format() )
			finally:
				removePidFile( stepIndex )
# 				if 1 != stepIndex: sendEmail( 'stepIndex:{} response:{} running:"{}"'.format( stepIndex, response, sys.argv ) )
	return response

if __name__ == '__main__':
	response = main()
	print( '\nDONE - returning {}'.format( response ) )
	sys.exit( response )
