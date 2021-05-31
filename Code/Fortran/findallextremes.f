      implicit none
      character*48 infile,outfile,textin,dummy1,dummy2,dummy3
      real x,y,z,zmin,zmax
      call getarg(1,infile)
      call getarg(2,outfile)
      open(10,file=infile,status='old')
      open(11,file=outfile)
 2    read(10,*,end=199)dummy1,dummy2,dummy3,textin
      open(12,file=textin,status='old')
      zmin=z
      zmax=z
 1    read(12,*,end=99)x,y,z
      if(z.lt.zmin)zmin=z
      if(z.gt.zmax)zmax=z
      go to 1
 99   continue
      write(11,*)"'",textin,"'",zmin,zmax
      close(12)
      go to 2
 199  continue
      close(10)
      close(11)
      stop
      end
