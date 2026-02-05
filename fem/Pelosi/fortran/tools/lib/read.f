C  =====================================================================
C
      Subroutine ReadMesh  (finname,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                      XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                      NNODE,NELE,NSPIG)
C
C  =====================================================================
C     Sets an entry in a band storage mode matrix
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      character*256 finname
      integer MAXNODE,MAXNXE,MAXELE,MAXSPIG
C     [OUT]      
      real    XY(2,MAXNODE)
      integer ELE(0:MAXNXE,MAXELE),NLAB(MAXNODE),ELAB(MAXELE)             
      integer SPIG(0:MAXNXE,MAXELE),SLAB(MAXSPIG),NSPIG
      integer NNODE,NELE
C     [LOCAL]      
      character*256 dump
      logical       toolarge
      integer       i,ii,j

C     -------- Open File      
      open (UNIT=1,STATUS='OLD',ACCESS='SEQUENTIAL', ERR=3000, 
     &      FILE=finname, FORM='FORMATTED')           
C     -------- Writing Data
      read (1,'(a80)',err=3001) dump
      read (1,'(a11,I6)',err=3001) dump,nele
      read (1,'(a8,I6)',err=3001) dump,nnode
      read (1,'(a8,I6)',err=3001) dump,nspig
      read (1,'(a80)',err=3002) dump

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
      
      if (MAXSPIG.lt.NSPIG) then
        print * ,'****ERROR****'
        print * ,'Parameter MAXSPIG must be at least ',NSPIG
        toolarge = .TRUE.
      endif
      if (toolarge) stop

      do i=1,nele        
        read (1,'(11I6)',err=3002) ii, ELE(0,ii), 
     &       (ELE(j,ii),j=1,ELE(0,ii)),elab(ii)
      enddo
      read (1,'(a80)',err=3003) dump
      do i=1,nnode
        read  (1,'(I6,1X,f13.6,1X,f13.6,1X,I6)',err=3003) 
     &         ii, xy(1,ii), xy(2,ii), nlab(ii)
      enddo                                              
      if (nspig.gt.0) then
        read (1,'(a80)',err=3004) dump
        do i=1,nele
          read (1,'(10I6)',err=3004) ii,SPIG(0,ii),
     &         (SPIG(j,ii),j=1,SPIG(0,ii))
        enddo                                              
        read (1,'(a80)',err=3005) dump
        do i=1,nspig
          read (1,'(2I6)',err=3005) ii,SLAB(ii)
        enddo                                              
      endif    
      goto 3010
3000  write(*,*) '****ERROR****'
      write(*,*) 'Cannot open file ', finname
      stop
3001  write(*,*) '****ERROR****'
      write(*,*) 'Error writing header of file ', finname
      stop
3002  write(*,*) '****ERROR****'
      write(*,*) 'Error writing Elements Block in file ', finname
      stop
3003  write(*,*) '****ERROR****'
      write(*,*) 'Error writing Nodes Block in file ', finname
      STOP
3004  write(*,*) '****ERROR****'
      write(*,*) 'Error reading Edges Block in file ', finname
      STOP
3005  write(*,*) '****ERROR****'
      write(*,*) 'Error reading Edge Labels Block in file ', finname
      STOP
3010  close(1)
      return
      end
        
