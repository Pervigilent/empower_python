C  =====================================================================
C
C     SIMPLE CALL TO THE DELUNAYZATION PROCEDURE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C
C  =====================================================================
      program do_del
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
      integer NeleType              ! Element Type       
                                    ! 3 - Triangular, first order      
                                    ! 6 - Triangular, second order      
                                    ! 4 - Qadrilateral, first order      
                                    ! 8 - Quadrilateral, second order      
      character*256 fnamein         ! Input File Name
      character*256 fnameout        ! Output File Name
C     ---------- Inputs
      write (*,'(1x,a)') 'Input file ?  : '
      read (*,*) fnamein 
      write (*,'(1x,a)') 'Output file ? : '
      read (*,*) fnameout
      call ReadMesh (fnamein,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                NNODE,NELE,NSPIG)              

C     TESTS 
      neletype=ELE(0,1)
      
      call Delaunay  (MAXNODE,MAXNXE,MAXELE,
     &                XY,ELE,SPIG,ELAB,NELE)
      
      call WriteMesh (fnameout,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                NNODE,NELE,NSPIG)              

      end                                    
