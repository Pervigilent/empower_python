C  =====================================================================
C
C     SIMPLE CALL TO THE FEM TO NUMBER OPTIMIZATION PROCEDURE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C
C  =====================================================================
      program do_ren
      integer    MAXNODE, MAXNXE, MAXELE,
     &           MAXSPIG, MAXBLCK, NPTXBLK,
     &           MAXPT, MAXEXS
      parameter (MAXNODE = 3000,    ! Maximum node number
     &           MAXNXE  = 8,       ! Maximum nuber of nodes in each element
     &           MAXELE  = 4000,    ! Maximum number of elements
     &           MAXSPIG = 6000,    ! Maximum number of edges
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
      integer NPERMUT(MAXNODE)      ! Node Permutations
      integer EPERMUT(MAXELE)       ! Element Permutations
      integer SPERMUT(MAXSPIG)      ! Edge Permutations
      character*256 fnamein         ! Input File Name 
      character*256 fnameout        ! Output File Name 
 
C     ---------- Auxiliary variables 
	real    Period                ! Period of the structure 
	integer Thre                  ! Threshold for Adjacency 
	integer EQN(MAXNODE)          ! Equivalent Nodes 
	integer i 
      character*1 dummy             ! Answers 

C     ---------- Initialization
      Thre   = 1
      Period = 0.
      do i =1,MAXNODE
        EQN(i) = i
      enddo
C     ---------- Inputs
      write (*,'(1x,a)') 'Input file ?     : '
      read (*,*) fnamein 
      write (*,'(1x,a)') 'Output file ?    : '
      read (*,*) fnameout
      write (*,'(1x,a)') 'Node or edge Optimization? [N/e] : '
      read (*,*) dummy
	if (dummy.eq.'e' .or. dummy.eq.'E') THRE = 2
      write (*,'(1x,a)') 'Periodic ? [y/N] : '
      read (*,*) dummy
      if (dummy.eq.'y' .or. dummy.eq.'Y') then 
        write (*,'(1x,a)') 'Period along x?  : '
        read (*,*) Period
      endif 
 
      call ReadMesh (fnamein,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                NNODE,NELE,NSPIG)              
 
	if (Period .gt. 0) then
        write(*,*)'Finding Equivalent Nodes'
        call FindEqN (XY,MAXNODE,NNODE,Period,EQN)
	endif 
 
C     TESTS 
      call RENUMBER(MAXELE,MAXNXE,MAXNODE,MAXSPIG,
     &              XY,ELE,SPIG,NNODE,NELE,NSPIG,ELAB,NLAB,SLAB,
     &              THRE,EQN,NPERMUT,EPERMUT,SPERMUT)
      
      call WriteMesh (fnameout,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                NNODE,NELE,NSPIG)              

      end                                    
