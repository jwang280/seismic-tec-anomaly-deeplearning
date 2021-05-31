c     Compile with: gfortran -g -fbacktrace -ffpe-trap=zero,overflow,underflow ProcessOneDayCorrRand.f -o ProcessOneDayCorrRand

      PROGRAM ProcessOneDayCorrRand
      implicit none
      character*4 numberstrng
      character*10 cdate,datestrng,cyear,cmonth,cday
      character*20 eqname
      integer date(3),lg(10),i,j,k,k2,fcount,jmax,jmean,nd(450)
      character*40 elements(10),elemnts2(10)
      character*256 paste,filein,fileout,maxtecfile,meantecfile
      real lat(450),long(450),tec(2880,450),meantec(450),corr(450)
      real xwiggle(2880,450),xbarwiggle(450),sig1(450),sig2(450)
      real distkm,xbar(450)
      logical fileex,unitop
      data date/2011,4,2/
      call getarg(1,eqname)
      call getarg(2,cyear)
      call getarg(3,cmonth)
      call getarg(4,cday)
      read(cyear,*)date(1)
      read(cmonth,*)date(2)
      read(cday,*)date(3)
      cdate=datestrng(date)
      elements(1)='/seis/prj/' ! "/scratch/TEC/"
      elements(2)='tec/' ! "GPSTK/DATA/"
      elements(3)='data' ! eqname
      elements(4)="/"
      elements(5)=cdate
      elements(6)="/TEC_Data/"
      elements(7)="igs."
      elemnts2(1)= "/seis/prj/tec/data/" ! "/scratch/TEC/GPSTK/DATA/" ! "/seis/prj/csep/"
      elemnts2(2)= '' ! eqname ! "/seis/prj/csep/"
      elemnts2(3)="/Correlation/"
      elemnts2(4)=cdate
      elemnts2(5)="."
      elemnts2(6)="corr"
      fileout=paste(elemnts2,6)
      inquire(file=fileout,opened=unitop)
      if(unitop)close(11)
      write(12,*)"filout",fileout
      open(11,file=fileout)
      fcount=0
      do k=1,450
         meantec(k)=0.
      enddo
      do j=1,2880
         elements(8)=numberstrng(j)
         filein=paste(elements,8)
         write(12,*)filein
         inquire(file=filein,exist=fileex)
         if (fileex)then
            write(12,*)"Input file exists"
            fcount=fcount+1
            inquire(unit=10,opened=unitop)
            if(unitop)close(10)
            open(10,file=filein,status='old')
            do k=1,450
!                print*, 'Reading stuff',k
                read(10,*,err=101,end=102)lat(k),long(k),tec(j,k)
c     calculate correlation matrix for this day
             meantec(k)=meantec(k)+tec(j,k)/2880
            enddo
 101        CONTINUE
         else
            go to 102 ! CYCLE
         endif
 102  enddo
c     calculate local correlation index
      write(12,*)"flag1"
      do k=1,450
         nd(k)=0.
         xbar(k)=0.
         do j=1,2880
            xbar(k)=xbar(k)+tec(j,k)/2880
            xwiggle(j,k)=0.
         enddo
         write(12,*)"flag1a",k,j
         do k2=1,450
            if ((k2.ne.k).and.(distkm(lat(k),long(k),lat(k2),
     1           long(k2)).lt.250)) then
               nd(k)=nd(k)+1
               do j=1,2880
                  xwiggle(j,k)=xwiggle(j,k)+tec(j,k2)
               enddo
            write(12,*)"flag1b",k,j,k2
            endif
         enddo
      enddo
      write(12,*)"flag1c",k,j,k2
      do k=1,450
         xbarwiggle(k)=0.0
         do j=1,2880
            xwiggle(j,k)=xwiggle(j,k)/nd(k)
            xbarwiggle(k)=xbarwiggle(k)+xwiggle(j,k)/2880
         enddo
      enddo
      write(12,*)"flag2"
      do k=1,450
         sig1(k)=0.
         sig2(k)=0.
         do j=1,2880
            sig1(k)=sig1(k)+(tec(j,k)-xbar(k))**2/2879
            sig2(k)=sig2(k)+(xwiggle(j,k)-xbarwiggle(k))**2/2879
         enddo
         sig1(k)=sqrt(sig1(k))
         sig2(k)=sqrt(sig2(k))
      enddo
      write(12,*)"flag3"
      do k=1,450
         corr(k)=0.0
         do j=1,2880
            corr(k)=corr(k)+(tec(j,k)-xbar(k))*(xwiggle(j,k)
     1           -xbarwiggle(k))/(2879*sig1(k)*sig2(k))
         enddo
      enddo
      write(12,*)"flag4"
      write(6,*)"fcount",fcount
      do k=1,450
         write(11,*) lat(k),long(k),corr(k),nd(k)
         if (corr(k).lt.0.99) write(6,*) cdate,lat(k),long(k),corr(k)
      enddo
      write(12,*)"flag5"
 199  continue
 99   continue
      close(10)
      close(11)
      stop
      end


      character*256 function paste(elements,n)
      implicit none
      character*40 elements(10)
      character*1 elemat(40,10)
      integer lg(10),i,j,n
c      write(6,*)(lg(i),i=1,n)
      do i=1,10
         lg(i)=40
         do j=1,40
            elemat(j,i)=" "
         enddo
         read(elements(i),21)(elemat(j,i),j=1,lg(i))
 21      format(40a1)
         do j=1,40
            if (elemat(j,i).eq.' ')lg(i)=lg(i)-1
         enddo
      enddo
!      print *, ((elemat(j,i),j=1,lg(i)),i=1,n)
      write(paste,22)((elemat(j,i),j=1,lg(i)),i=1,n) ! should be n
!      print *, 'Paste:[',paste,']'
 22   format(256a1)
      return
      end

      character*10 function datestrng(date)
      implicit none
      character*4 cyear
      character*2 cmonth,cday
      character*1 dash
      integer date(3)
      write(cyear,21)date(1)
 21   format(i4)
 22   format(i2)
 23   format("0",i1)
      data dash/"-"/
      if (date(2).gt.9)then
         write(cmonth,22) date(2)
      else
         write(cmonth,23) date(2)
      endif
      if (date(3).gt.9)then
         write(cday,22) date(3)
      else
         write(cday,23) date(3)
      endif
      write(datestrng,24)cyear,dash,cmonth,dash,cday
 24   format(a4,a1,a2,a1,a2)
      return
      end

      character*4 function numberstrng(n)
      integer n
      if (n.gt.999)then
         write(numberstrng,21)n
      else if (n.gt.99)then
         write(numberstrng,22)n
      else if (n.gt.9)then
         write(numberstrng,23)n
      else
         write(numberstrng,24)n
      endif
 21   format(i4)
 22   format("0",i3)
 23   format("00",i2)
 24   format("000",i1)
      return
      end

      real function distkm(lat1,long1,lat2,long2)
      implicit none
      real lat1,long1,lat2,long2,diskm2
      real theta1,theta2,phi1,phi2,pi,cosdis,erad
      pi=3.14159
      erad=111.*180/pi
      theta1=lat1*pi/180.
      theta2=lat2*pi/180.
      phi1=long1*pi/180.
      phi2=long2*pi/180.
      cosdis=sin(theta1)*sin(theta2)+cos(theta1)*cos(theta2)*
     1cos(phi1-phi2)
      if ((cosdis.gt.1.0).or.(cosdis.lt.-1.0))then
         distkm=diskm2(lat1,long1,lat2,long2)
         return
      endif
      distkm=erad*acos(cosdis)
      return
      end

      real function diskm2(lat1,long1,lat2,long2)
      real lat1,long1,lat2,long2,degkm,dx,dy
      degkm=111.0
      dx=(lat1-lat2)*degkm
      dy=(long1-long2)*degkm*cos(3.14159*(lat1+lat2)/360.)
      diskm2=sqrt(dx**2+dy**2)
      return
      end

