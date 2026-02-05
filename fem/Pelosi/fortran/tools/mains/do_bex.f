C  =====================================================================
C
C     SIMPLE CALL TO THE BAND EXTIMATION PROCEDURE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C
C  =====================================================================
      program do_bex
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
      real    XY(2,MAXNODE)         ! Coordiantes of nodes
      integer ELE(0:MAXNXE,MAXELE)  ! Connettivity matrix
      integer SPIG(0:MAXNXE,MAXELE) ! Edges Matrix
      integer NLAB(MAXNODE)         ! Node Labels
      integer ELAB(MAXELE)          ! Element Labels   
      integer SLAB(MAXSPIG)         ! Edge Labels
      integer NNODE                 ! Number of Nodes
      integer NELE                  ! Number of Elements                                           
      integer NSPIG                 ! Number of Edges
      character*256 fname           ! Output File

C     ---------- External Functions
      integer nBANDEX

C     ---------- Inputs
      write (*,'(1x,a)') 'Input file ?  : '
      read (*,*) fname 
      call ReadMesh (fname,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                NNODE,NELE,NSPIG)              

C     TESTS 
      write (*,*) 'Band for NODES:', nBANDEX (MAXELE,MAXNXE,ELE,NELE)
      write (*,*) 'Band for EDGES:', nBANDEX (MAXELE,MAXNXE,SPIG,NELE)
      end                                    
