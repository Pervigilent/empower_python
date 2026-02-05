C  =====================================================================
C
C     SIMPLE CALL TO THE FEM TO POSTSCRIPT PROCEDURE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C
C  =====================================================================
      program do_f2ps
      integer    MAXNODE, MAXNXE, MAXELE,
     &           MAXSPIG, MAXBLCK, NPTXBLK,
     &           MAXPT, MAXEXS
      parameter (MAXNODE = 3000,    ! Maximum node number
     &           MAXNXE  = 8,       ! Maximum nuber of nodes in each element
     &           MAXELE  = 4000,    ! Maximum number of elements
     &           MAXSPIG = 4000,    ! Maximum number of edges
     &           MAXBLCK = 100,     ! Maximum number of blocks
     &           NPTXBLK = 8,       ! Maximum number of points per block
     &           MAXPT   = 300,     ! Maximum number of Points
     &           MAXEXS  = 50)      ! Maximum element side on block side                                  

C     ---------- Primary mesh variables    
      real    XY(2,MAXNODE)         ! Coordinates of nodes
      integer ELE(0:MAXNXE,MAXELE)  ! Connettivity matrix
      integer SPIG(0:MAXNXE,MAXELE) ! Edges Matrix
      integer NLAB(MAXNODE)         ! Node Labels
      integer ELAB(MAXELE)          ! Element Labels   
      integer SLAB(MAXSPIG)         ! Edge Labels
      integer NNODE                 ! Number of Nodes
      integer NELE                  ! Number of Elements                                           
      integer NSPIG                 ! Number of Edges
      character*256 fname           ! Input/Output File Name
      logical flags(6)              ! What is to be shown?
      character*1 dummy             ! Answers
      integer i
      
C     ---------- Inputs
      write (*,'(1x,a)') 'Input file ?  : '
      read (*,*) fname 
      call ReadMesh (fname,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                NNODE,NELE,NSPIG)              
                                                         
      do i=1,6
        flags(i)=.false.
      enddo                                                         
      write (*,'(1x,a)') 'Show Element Numbers ? [y/N] : '
      read (*,*) dummy                       
      if (dummy.eq.'y' .or. dummy.eq.'Y') flags(1)=.TRUE.
      write (*,'(1x,a)') 'Show Element Labels ?  [y/N] : '
      read (*,*) dummy                       
      if (dummy.eq.'y' .or. dummy.eq.'Y') flags(2)=.TRUE.
      write (*,'(1x,a)') 'Show Node Numbers ?    [y/N] : '
      read (*,*) dummy                       
      if (dummy.eq.'y' .or. dummy.eq.'Y') flags(3)=.TRUE.
      write (*,'(1x,a)') 'Show Node Labels ?     [y/N] : '
      read (*,*) dummy                       
      if (dummy.eq.'y' .or. dummy.eq.'Y') flags(4)=.TRUE.
      write (*,'(1x,a)') 'Show Edge Numbers ?    [y/N] : '
      read (*,*) dummy                       
      if (dummy.eq.'y' .or. dummy.eq.'Y') flags(5)=.TRUE.
      write (*,'(1x,a)') 'Show Edge Labels ?     [y/N] : '
      read (*,*) dummy                       
      if (dummy.eq.'y' .or. dummy.eq.'Y') flags(6)=.TRUE.
      
C     TESTS 
      write (*,'(1x,a)') 'Output file ? : '
      read (*,*) fname
      call WritePS (fname,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                NNODE,NELE,FLAGS)

      end                                    
