      implicit none
      character*45 infile,outfile,filenm,psfilenm,joinstr
      character*40 header,makeheader
      character*9 label,units
      character*3 suffix
      character*15 cptfile,cptname
      character*1 a(45)
      integer lq,findlq,i
      real xmin,xmax
      call getarg(1,infile)
      call getarg(2,outfile)
      write(6,*) infile,outfile
      open(unit=10,file=infile,status='old')
      open(unit=11,file=outfile)
 1    read(10,*,end=99,err=1)filenm,xmin,xmax
      read(filenm,21)(a(i),i=1,45)
 21   format(45a1)
      if (a(5).eq.'/')then
         label="Mean_TEC"
         units="TECU"
      else if (a(6).eq."A")then
         label="IndexA"
         units="Anomaly"
      else if(a(6).eq."B")then
         label="IndexB"
         units="Anomaly"      
      else if(a(6).eq."C")then
         label="IndexC"
         units="Anomaly"      
      else if(a(6).eq."D")then
         label="IndexD"
         units="Anomaly"
      else
         write(6,*)"filenm with unexpected name format", filenm
         stop
      endif
      suffix=".ps"
      lq=findlq(filenm)
      psfilenm=joinstr(a,suffix,lq)
      cptfile=cptname(xmin,xmax)
      header=makeheader(a,lq)
      write(11,23)"sh tecplot.sh ",filenm,header,label,units,cptfile,
     1     " > ",psfilenm
 23   format(a14,1x,a45,a12,1x,a9,a9,1x,a15,a3,a45)
      go to 1
 99   continue
      close(10)
      close(11)
      stop
      end

      integer function findlq(filenm)
      character*45 filenm
      character*1 a(45)
      integer i
      read(filenm,21)(a(i),i=1,45)
 21   format(45a1)
      do i=1,45
         if (a(i).eq.' ')then
            findlq=i-1
            go to 99
         endif
      enddo
 99   return
      end

      character*45 function joinstr(a,suffix,lq)
      character*1 a(45)
      character*3 suffix
      integer lq
      read(suffix,21)(a(i),i=(lq+1),(lq+3))
 21   format(45a1)
      write(joinstr,22)(a(i),i=1,(lq+3))
 22   format(45a1)
      return
      end
      
      character*45 function makeheader(a,lq)
      character*1 a(45),b(12)
      integer lq
      b(12)=a(lq-1)
      do i=1,11
         b(i)=a(lq-16+i)
      enddo
      write(makeheader,22)(b(i),i=1,12)
 22   format(12a1)
      return
      end

      character*15 function cptname(ymin,ymax)
      real ymin,ymax
      integer nmin,nmax
      character*2 nstrng(13),str1,str2
      data nstrng/'-3','-2','-1','-0','+1','+2','+3','+4','+5',
     1     '+6','+7','+8','+9'/
      nmin=int(ymin)
      nmax=int(ymax)+1
      if (nmax.eq.nmin)nmax=nmax+1
      do i=-3,9
         if (nmin.eq.i)str1=nstrng(i+4)
         if (nmax.eq.i)str2=nstrng(i+4)
      enddo
      write(cptname,21)'colours',str1,str2,'.cpt'
 21   format(a7,2a2,a4)
      return
      end
      
      
      
      
      
      
