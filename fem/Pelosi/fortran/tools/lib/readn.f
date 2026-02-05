C  ====================================================================
C  
      Subroutine ReadNodalMesh  (finname,MAXNODE,MAXNXE,MAXELE,
     &                           XY,ELE,NLAB,ELAB,NNODE,NELE)
C      
C  ====================================================================
C     READS A GEOMETRY FILE for what concerns the connectivity matrix
C     and the node coordinates, skipping the edge informations
C     QUICK_FEM (C) 1997 Pelosi - Coccioli - Selleri
C  ====================================================================
C     [IN]
      character*256 finname            ! File name
      integer       MAXNODE,MAXNXE,MAXELE

C     [OUT]      
      real    XY(2,MAXNODE)            ! Node Co-ordinates
      integer NLAB(MAXNODE)            ! Node Labels
      integer NNODE                    ! Node Number
      integer ELE(0:MAXNXE,MAXELE)     ! Connection matrix
      integer ELAB(MAXELE)             ! Element Labels
      integer NELE                     ! Element Number
C     [LOCAL]      
      character*256 dump               
      logical toolarge
      integer i,ii,j,nspig

C     ---------- Open File      
      open (UNIT=1,STATUS='OLD',ACCESS='SEQUENTIAL', ERR=6000, 
     &      FILE=finname, FORM='FORMATTED')           

C     ---------- Reading Header
      read (1,'(a80)',err=6001) dump
      read (1,'(a11,I6)',err=6001) dump,nele
      read (1,'(a8,I6)',err=6001) dump,nnode
      read (1,'(a8,I6)',err=6001) dump,nspig
      
      toolarge = .FALSE.
      if (MAXNODE.lt.NNODE) then
        print * ,'****ERROR****'
        print * ,'Parameter MAXNODE must be at least ',NNODE
        toolarge = .TRUE.
      endif

      if (MAXELE.lt.NELE) then
        print * ,'****ERROR****'
        print * ,'Parameter MAXELE must be at least ',NELE
        toolarge = .TRUE.
      endif
      if (toolarge) stop

C     ---------- Reading the Connection Matrix & Element labels
      read (1,'(a80)',err=6002) dump
      do i=1,nele        
        read (1,'(11I6)',err=6002) ii, ELE(0,ii), 
     &       (ELE(j,ii),j=1,ELE(0,ii)),elab(ii)
      enddo                                          

C     ---------- Reading the Node co-cordinates & Labels
      read (1,'(a80)',err=6003) dump
      do i=1,nnode
        read  (1,'(I6,1X,f13.6,1X,f13.6,1X,I6)',err=6003) 
     &         ii, xy(1,ii), xy(2,ii), nlab(ii)
      enddo                                              
      goto 6010
6000  write(*,*) '****ERROR 001****'
      write(*,*) 'In ReadNodeMesh: Cannot open: ', finname
      stop
6001  write(*,*) '****ERROR 002****'
      write(*,*) 'In ReadNodeMesh: Error reading header in: ', finname
      stop
6002  write(*,*) '****ERROR 003****'
      write(*,*) 'In ReadNodeMesh: Error reading elements in: ', finname
      stop
6003  write(*,*) '****ERROR 004****'
      write(*,*) 'In ReadNodeMesh: Error reading nodes in: ', finname
      STOP
6010  close(1)
      return
      end
        
