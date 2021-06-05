#!/usr/bin/env python3

import matplotlib
matplotlib.use( 'Agg' )

import os
import glob
import sys
from datetime import datetime
from ftplib import FTP
import shutil

class TECfetchData( object ):
    alldataFolder = 'alldata'
    outFolder = 'TEC_Data'
    logFiles = 'logFiles'
    
    def __init__( self, utcDate, workingFolder='TEC' ):
        super( TECfetchData, self ).__init__()
        self.workingFolder = workingFolder
        self.utcDateStr = utcDate
        self.utcDateItem = datetime.strptime( utcDate, '%Y-%m-%d' )
        self.setupRootFolders()
        self.setupBinPrefix()
        self.newSamplingInterval = 30
        self.message = None
	
    def setFolder( self, path ):
        if not os.path.exists( path ):
            os.makedirs( path, exist_ok=True )

    def setWorkingFolderLogFiles( self ):
        self.workingFolderLogFiles = os.path.join( self.workingFolder, self.logFiles )
        self.setFolder( self.workingFolderLogFiles )

    def setWorkingFolderDate( self ):
        self.workingFolderDate = os.path.join( self.workingFolder, self.utcDateStr )
        self.setFolder( self.workingFolderDate )

    def setWorkingFolderDateAlldata( self ):
        self.workingFolderDateAlldata = os.path.join( self.workingFolderDate, self.alldataFolder )
        self.setFolder( self.workingFolderDateAlldata )

    def setWorkingFolderDateOut( self ):
        self.workingFolderDateOut = os.path.join( self.workingFolderDate, self.outFolder )
        self.setFolder( self.workingFolderDateOut )

    def setupRootFolders( self ):
        self.setWorkingFolderDate()
        self.setWorkingFolderLogFiles()
        self.setWorkingFolderDateAlldata()
        self.setWorkingFolderDateOut()
        self.setNavFileName()

    #def createOutputFolder( self, dateString ):
        #dataFolder = os.path.join( '..', 'data' )
        #if not os.path.isdir( dataFolder ):
            #os.makedirs( dataFolder )
        #dateFolder = os.path.join( dataFolder, dateString )
        #if not os.path.isdir( dateFolder ):
            #os.makedirs( dateFolder )
        #return dateFolder
	
    def setupBinPrefix( self ):
        cwd = os.path.dirname( os.path.realpath( sys.argv[0] ) )
        self.binPrefix = os.path.abspath( os.path.join( cwd, '..', 'bin' ) ) 
	
    def download( self ):
        response = False
        print( 'TECfetchData::download Gathering data'.format() )
        if self.fetchStoreGeoNetData():
            if self.fetchStoreNavData():
                if self.unpackZippedData():
                    if self.shortRinexToLongRinex():
                        #if self.preprocess():
                            #if self.ionoBias():
                                #if self.tecMaps():
                                    response = True
                                #else : print( 'TECfetchData::download FAILED in tecMaps' )
                            #else : print( 'TECfetchData::download FAILED in ionoBias' )
                        #else : print( 'TECfetchData::download FAILED in preprocess' )
                    else: print( 'TECfetchData::download FAILED in shortRinexToLongRinex'.format() )
                else: print( 'TECfetchData::download FAILED in unpackZippedData'.format() )
            else: print( 'TECfetchData::download FAILED in downloadNavData'.format() )
        else: print( 'TECfetchData::download FAILED in downloadDataFromGeoNet'.format() )
        return response
    
    def dayOfYear( self, dateItem ):
        return dateItem.timetuple().tm_yday    
    
    def fetchStoreGeoNetData( self, ftpSite='ftp.geonet.org.nz', basePath='/gnss/rinex/' ):
        response = True
        julianDay = self.dayOfYear( self.utcDateItem )
        year = self.utcDateItem.year
        ftp = FTP( ftpSite )
        ftp.login()  
        pathStr = os.path.join( basePath + str( year ), str( julianDay ) )
        try:
            ftp.cwd( pathStr )
            listing = []
            ftp.retrlines( 'LIST', listing.append )
            list_d = [ i for i in listing if i.endswith( 'o.gz' ) ]
            for item in list_d:
                baseFilename = item.split()[-1]
                localFilename = os.path.join( self.workingFolderDateAlldata, baseFilename )
                with open( localFilename, 'wb' ) as fIn:
                    ftp.retrbinary( 'RETR ' + baseFilename, fIn.write, 8 * 1024 )
            response = True
        except FileNotFoundError as ex:
            self.message = '***** EXCEPTION:{}'.format( ex )
            print( 'TECfetchData::fetchStoreGeoNetData {} Exception'.format( self.message ) )
            response = False
        finally:
            ftp.quit()
        return response
    
    def fetchStoreNavData( self ):
        response = True
        julianDay = self.dayOfYear( self.utcDateItem )
        year = self.utcDateItem.year
        pathStr = '/pub/rinex/' + str( year ) + '/' + str( julianDay )
        garnerUcsd = 'garner.ucsd.edu'
        try:
            ftp = FTP( garnerUcsd )
            ftp.login()  
            ftp.cwd( pathStr )
            filename = os.path.basename( self.navFileName ) + '.Z'
            localFilename = os.path.join( self.workingFolderDateAlldata, filename )
            try:
                with open( localFilename, 'wb' ) as lf:
                    ftp.retrbinary( 'RETR ' + filename, lf.write, 8 * 1024 )
                    print( 'TEC_Class::downloadNavData grabbing localFilename:{} size:{}'.format( localFilename, os.path.getsize( localFilename ) ) )
            except FileNotFoundError as ex:
                self.message = '***** EXCEPTION:{}'.format( ex )
                print( 'TECfetchData::fetchStoreNavData {} Exception'.format( self.message ) )
            except TimeoutError as ex:
                self.message = '***** EXCEPTION:{}'.format( ex )
                print( 'TECfetchData::fetchStoreNavData {} Exception'.format( self.message ) )                    
        finally:
            try:
                ftp.quit()
            except BrokenPipeError as ex:
                self.message = '***** EXCEPTION:{}'.format( ex )
                print( 'TECfetchData::fetchStoreNavData quit {} Exception'.format( self.message ) )
        return response
            
    def runSystemCommand( self, command, checkResponse=True ):
        systemResponse = 0
#       print( 'TECfetchData::runSystemCommand command:{}'.format( command ) )
        commandResult = os.system( command ) # the RinDum command returns an error condition, even if it worked. I checked it.
        if checkResponse:
            systemResponse = os.WEXITSTATUS( commandResult )
        if 0 != systemResponse:
            print( '\nTECfetchData::runSystemCommand commandResult:{}\ncommand:\n{}\n'.format( systemResponse, command ) )
        return systemResponse
            
    def unpackZippedData( self ):
        response = False
        zipFilesWildCard= self.workingFolderDateAlldata + '/*.???.*'
        for compressedFileName in glob.glob( zipFilesWildCard ):
            command = 'gunzip -f -k ' + compressedFileName
            if 0 == self.runSystemCommand( command ):
                response = True
            else:
                print( 'TECfetchData::unpackZippedData FAILED to uncompress:{}'.format( compressedFileName ) )
                command = 'ls -l ' + compressedFileName
                unused_commandResult = self.runSystemCommand( command, False )
        return response 
	
    def setNavFileName( self ):
        print( 'TECfetchData::setNavFileName'.format() )
        julianDay = self.dayOfYear( self.utcDateItem )
        dayStr = '%03d' % julianDay
        year = self.utcDateItem.year
        self.navFileName = os.path.join( self.workingFolderDateAlldata, 'auto' + str( dayStr ) + '0.' + str( year )[-2:] + 'n' )
        print( 'TECfetchData::setNavFileName navFileName:{} exists:{} (probably not yet)'.format( self.navFileName, os.path.isfile( self.navFileName ) ) )   
    
    def shortRinexToLongRinex( self ): 
        response = True
        compressedRinex = self.workingFolderDateAlldata + '/*.??d'
        for compressedFileName in glob.glob( compressedRinex ):
            command = os.path.join( self.binPrefix, 'CRX2RNX' ) + ' -f ' + compressedFileName
            if 0 == self.runSystemCommand( command ):
                response = True
            else:
                print( 'TECfetchData::shortRinexToLongRinex failed to run CRX2RNX'.format() )
                response = True
        return response
    
    def makeRinsumCommand( self, fileName ):
        """
        extract summary information from RINEX file
        """
        command = None
        sumFile = fileName + '.sum'
        inFile = os.path.join( self.workingFolderDateAlldata, fileName )
        outFile = os.path.join( self.workingFolderDateAlldata, sumFile )
        print( 'TECfetchData::makeRinsumCommand sumFile:{} inFile:{} outFile:{}'.format( sumFile, inFile, outFile ) )
        if os.path.isfile( inFile ):
            command = os.path.join( self.binPrefix, 'RinSum' ) + ' --obs ' + inFile + ' > ' + outFile
        else:
            print( 'TECfetchData::makeRinsumCommand missing input file:{}'.format( inFile ) )
        return command, sumFile
    
    def getPositionFromSumFile( self, sumFile ):
        """
        get position of the GPS station from the RINEX summary file produced by RinSum
        """
        sumPath = os.path.join( self.workingFolderDateAlldata, sumFile )
        for line in open( sumPath ):
            if line.find( 'Position' ) >= 0:
                position = map( float, line.split( ':' )[1][2:-3].split( ',' ) )
                return list( position )
        print( '\nTECfetchData::getPositionFromSumFile failed to find position in:{}'.format( sumFile ) )
        return None 
    
    def makeResCorCommand( self, fileName, position ):
        """
        Prgm RinEdit will open and read a single Rinex observation file, apply editing commands
        using the RinexEditor package, compute any of several residuals and corrections and
        register extended Rinex observation types for them, and then write the edited data,
        along with the new extended observation types, to an output Rinex observation file.
        Here we are resampling to a new sampling interval and preparing for IonoBias
        """
        command = None
        rcFile = None
        inFile = os.path.join( self.workingFolderDateAlldata, fileName )
        if os.path.isfile( inFile ):
            if os.path.isfile( self.navFileName ):
                rcFile = os.path.basename( fileName ) + '.RC'
                outFile = os.path.join( self.workingFolderDateAlldata, rcFile )
                command = self.binPrefix + '/ResCor'
                command += ' -IF' + inFile + ' -OF' + outFile
                command += ' -TN' + str( int( float( self.newSamplingInterval ) ) ) + ' -HDf -HDm' + fileName[0:4] + ' --Cforce --IonoHt 350'
                command += ' -DOL1 -DOL2 -DOD1 -DOD2 -DOS1 -DOS2'
                command += ' -AOEL -AOAZ -AOLA -AOLO -AOSR'                
                command += ' --RxXYZ %.4f,%.4f,%.4f' % ( position[0], position[1], position[2] )
                command += ' --nav ' + self.navFileName
            else:
                print( 'TECfetchData::makeResCorCommand nav file is not available:{}'.format( self.navFileName ) )
        else:
            print( 'TECfetchData::makeResCorCommand file is not available:{}'.format( inFile ) )
        return command, rcFile     
    
    def getRinsumOutputFile( self, fileName ):
        sumFile = fileName + '.sum'
        return os.path.join( self.workingFolderDateAlldata, sumFile )
	
    def checkRinSumResult( self, fileName ):
        response = False
        with open( self.getRinsumOutputFile( fileName ), 'r' ) as fIn:
            lines = fIn.read().splitlines()
            last_line = lines[-1]
            if 'RinSum timing' in last_line: 
                response = True
        return response
    
    def preprocess( self ):
        print( 'TECfetchData::preprocess'.format() )
        response = True
        uncompressedWildcard = os.path.join( self.workingFolderDateAlldata, '*.??o' )
        for fullFileName in sorted( glob.glob( uncompressedWildcard ) ):
            print( 'TECfetchData::preprocess working with fullFileName:{}'.format( fullFileName ) )
            fileName = os.path.basename( fullFileName )
            command, sumFile = self.makeRinsumCommand( fileName )
            print( 'TECfetchData::preprocess working command:\n{}\nsumFile:{}'.format( command, sumFile ) )
            self.runSystemCommand( command, False )
            if self.checkRinSumResult( fileName ):
                print( 'TECfetchData::preprocess section:3'.format() )
                position = self.getPositionFromSumFile( sumFile )
                if position is not None:
                    print( 'TECfetchData::preprocess position:{}'.format( position ) )
                    command, rcFile = self.makeResCorCommand( fileName, position )
                    if command is not None:
                        print( 'TECfetchData::preprocess running command:\n{}\nrcFile:{}'.format( command, rcFile ) )
                        if 0 == self.runSystemCommand( command, False ):
                            #command, unused_sumPath = self.makeRinsumCommand( rcFile )
                            #print( 'TECfetchData::preprocess section:5 using command:{}'.format( command ) )
                            #if command is not None:
                                #print( 'TECfetchData::preprocess this command:{}'.format( command ) )
                                #if 0 != self.runSystemCommand( command, False ):
                                    response = True
                                    #print( 'TECfetchData::preprocess FAILED'.format() )
                            #else: print( 'TECfetchData::preprocess command is None'.format() )
                        else: print( 'TECfetchData::preprocess command FAILED:{}'.format( command ) )
                    else: print( 'TECfetchData::preprocess command was None'.format() )
                else: print( 'TECfetchData::preprocess position was None'.format() )
        return response

    def createIonoBiasInputFile( self, allDataPath, listFilesStr ):
        ionoBiasInputFile = 'bias_' + self.utcDateStr + '.inp'
        navFileName = os.path.basename( self.navFileName )
        # satbiasFileName = 'satBias_' + self.utcDateStr
        satbiasFileName = 'satBias_' + self.utcDateStr
        # aTesFileName = 'aTestFil_' + self.utcDateStr
        aTestFileName= 'aTestFile_' + self.utcDateStr
        # logFile = os.path.join( self.workingFolderLogFiles, 'bias_log_' + self.utcDateStr )
        logFile = os.path.join( self.workingFolderLogFiles, 'bias_log_' + self.utcDateStr )
        if os.path.isfile( listFilesStr ):
            with open( ionoBiasInputFile, 'w' ) as f:
                f.write( '--verbose\n' )
                f.write( '--debug\n' )
                logFileStr = '--log ' + logFile + '\n'
                f.write( logFileStr )
                inputFilesStr = '--input @' + listFilesStr + '\n'
                f.write( inputFilesStr )
                navDirStr = '--navdir ' + allDataPath + '\n'
                f.write( navDirStr )
                navFileStr = '--nav ' + navFileName + '\n'
                f.write( navFileStr )
                aTestFileStr = '--datafile ' + aTestFileName + '\n'
                f.write( aTestFileStr )
                f.write( '--MinPoints 24\n' )
                f.write( '--MinTimeSpan 12\n' )
                f.write( '--MinElevation 6\n' )
                f.write( '--MinLatitude -47\n' )
                f.write( '--MaxLatitude -32\n' )
                f.write( '--MinLongitude 165\n' )
                f.write( '--MaxLongitude 180\n' )
                f.write( '--TimeSector night\n' )
                f.write( '--TerminOffset 20\n' )
                f.write( '--IonoHeight 350\n' )
                f.write( '#--NoSatBiases\n' )
                biasOutStr = '--biasout ' + satbiasFileName + '\n'
                f.write( biasOutStr )
        else:
            print( 'TECfetchData::createIonoBiasInputFile missing listFilesStr:{}'.format( listFilesStr ) )
            ionoBiasInputFile = None
        return ionoBiasInputFile
    
    def ionoBias( self ):
        """
        Prgm IonoBias will open and read several preprocessed Rinex obs files
        (containing obs types EL,LA,LO,SR or SS) and use the data to estimate
        satellite and receiver biases and to compute a simple ionospheric model
        using least squares and the slant TEC values.
        """
        response = False
        listFilesStr = 'list_' + self.utcDateStr + '.files'        
        year = self.utcDateItem.year
        RC_Files = self.workingFolderDateAlldata + '/*.' + str( year )[-2:] + 'o.RC'
        command = 'ls ' + RC_Files + ' > ' + listFilesStr
        if 0 == self.runSystemCommand( command ):
            ionoBiasInputFile = self.createIonoBiasInputFile( self.workingFolderDateAlldata, listFilesStr )
            if ionoBiasInputFile is not None:
                command = os.path.join( self.binPrefix, 'IonoBias' ) + ' -f' + ionoBiasInputFile
                if 0 == self.runSystemCommand( command ): 
                    response = True
                else: print( 'TECfetchData::ionoBias FAILED command:{}'.format( command ) )
            else: print( 'TECfetchData::ionoBias ionoBiasInputFile is None'.format() )
        else: print( 'TECfetchData::ionoBias FAILED os.system command:{}'.format( command ) )
        return response
    
    def createTecMapsInputFile( self, allDataPath, outdataPath, listFilesStr ):
        tecMapInputFile = 'maps_' + self.utcDateStr + '.inp'
        logFile = os.path.join( self.workingFolderLogFiles, 'maps_log_' + self.utcDateStr )
        navFileName = os.path.basename( self.navFileName )
        satbiasFileName = 'satBias_' + self.utcDateStr
        if os.path.isfile( listFilesStr ):
            with open( tecMapInputFile, 'w' ) as f:
                f.write( ' # input file for VTECMap \n' )
                f.write( '--verbose\n' )
                f.write( ' #\n' )
                f.write( '# input\n' )
                inputFilesStr = '--input ' + listFilesStr + '@\n'
                f.write( inputFilesStr )
                navDirStr = '--navdir ' + allDataPath + '\n'
                f.write( navDirStr )
                navFileStr = '--nav ' + navFileName + '\n'
                f.write( navFileStr )
                f.write( '# ref station\n' )
                f.write( '--RxLLH -41.0,173.0,0.0,refs\n' )
                f.write( '# sat+rx biases\n' )
                satBiasStr = '--Biases ' + satbiasFileName + '\n'
                f.write( satBiasStr )
                f.write( '#\n' )
                f.write( '# Processing\n' )
                f.write( '--UniformSpacing\n' )
                f.write( '#--UniformGrid\n' )
                f.write( '--ElevThresh 6.0\n' )
                f.write( '--MinAcqTime 0\n' )
                f.write( '--NumLat 30\n' )
                f.write( '--BeginLat -47\n' )
                f.write( '--DeltaLat .5\n' )
                f.write( '--NumLon 15\n' )
                f.write( '--BeginLon 165\n' )
                f.write( '--DeltaLon 1\n' )
                f.write( '#  VTEC map will be created by default\n' )
                f.write( '#--MUFmap  these maps have not been verified yet...\n' )
                f.write( '#--F0F2map\n' )
                f.write( '#\n' )
                f.write( '# output\n' )
                logFileStr = '--log ' + logFile + '\n'
                f.write( logFileStr )
                f.write( '--OutputGrid\n' )
                f.write( '--GnuplotOutput\n' )
                baseNameStr = '--BaseName ' + outdataPath + '/igs\n'
                f.write( baseNameStr )
        else:
            print( '\nTECfetchData::createTecMapsInputFile missing listFilesStr:{}'.format( listFilesStr ) )
            tecMapInputFile = None
        return tecMapInputFile      

    def tecMaps( self ):
        response = False
        listFilesStr = 'list_' + self.utcDateStr + '.files'
        tecMapInputFile = self.createTecMapsInputFile( self.workingFolderDateAlldata, self.workingFolderDateOut, listFilesStr )
        year = self.utcDateItem.year
        RC_Files = os.path.join( self.workingFolderDateAlldata, '*.' + str( year )[-2:] + 'o.RC' )
        command = 'ls ' + RC_Files + ' > ' + listFilesStr
        if 0 == self.runSystemCommand( command ):
            command = os.path.join( self.binPrefix, 'TECMaps' ) + ' -f' + tecMapInputFile
            commandResult = self.runSystemCommand( command )
            if 0 == commandResult:
                response = True
            else:
                print( '***** TECfetchData::tecMaps FAILED to run command:{}'.format( command ) )
        else:
            print( '***** TECfetchData::tecMaps FAILED to run command:{}'.format( command ) )
            return response

    
        
def main():
    print( '\nThis program is not run - you probably meant to run "dataProcessing.py"' )
	
if __name__ == '__main__':
    main()
