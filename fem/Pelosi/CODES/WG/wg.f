C  =====================================================================
C
      program WG
C
C  =====================================================================
C     COMPUTES THE PROPAGATION CONSTANTS, EVENTUALLY COMPLEX, OF
C     AN INHOMOGENEOUS CLOSED WAVEGUID
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      implicit none
C     [PARAMETERS]
      integer    MAXNODE, MAXNXE, MAXELE,
     &           MAXSPIG, MAXMAT, KU, KL, KD
     
      parameter (MAXNODE = 200,     ! Maximum node number
     &           MAXNXE  = 8,       ! Maximum nuber of nodes in each element
     &           MAXELE  = 450,     ! Maximum number of elements
     &           MAXSPIG = 480,     ! Maximum number of edges
     &           MAXMAT  = 10,      ! Maximum number of materials
     &           KU      = 85,      ! Upper diagonals
     &           KL      = 85,      ! Lower diagonals
     &           KD      = 0)       ! Symmetric                            

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

      integer MLAB(MAXMAT)          ! Material Labels
      real    EPS(MAXMAT)           ! Material Epsilons
      integer NEM                   ! Number of different materials
      
      integer peclab                ! Pec nodes and edges label
      INTEGER nd40, nl40            ! Number of Nodes and Edges on pec              
      INTEGER nnn                   ! Degrees of freedom
      
C     ---------- System variables
c             Matrix A, frequency Independent and  dependent
      real    a_fi(MAXNODE+MAXSPIG, MAXNODE+MAXSPIG)   
      real    a_fd(MAXNODE+MAXSPIG, MAXNODE+MAXSPIG)   
c             Matrix B, frequency Independent and  dependent
      real    b_fd(MAXNODE+MAXSPIG, MAXNODE+MAXSPIG)   
      real    b_fi(MAXNODE+MAXSPIG, MAXNODE+MAXSPIG) 
 
      real    alfar(MAXNODE+MAXSPIG)! Eigenvalues, real numerator    
      real    alfai(MAXNODE+MAXSPIG)! Eigenvalues, imaginary numerator    
      real    beta(MAXNODE+MAXSPIG) ! Eigenvalues, denominator
      INTEGER IERR, info            ! Controls on eigenvalue solver
       
      CHARACTER*256 finname         ! Mesh file name   
      CHARACTER*256 fnameaux        ! Electromagnetic File name      
      CHARACTER*256 fnameout        ! Output file name

      INTEGER i, j, k               ! Loop variable            
      REAl    finiz,ffina,f         ! Starting, ending and actual freq.
      INTEGER nstep                 ! Frequency steps
      INTEGER nmode                 ! Number of modes for output
      REAL    k02                   ! Wavenumber, squared, in void
      REAL    aux,scl,scla          ! Auxiliary
      REAL    lenm                  ! Length of all edges

C     ---------- Inputs
      write (*,'(1x,a)') 'Mesh file ?     : '
      read(*, *) finname

      write (*,'(1x,a)') 'Electromagnetic file ?    : '
      read (*,*) fnameaux

      write (*,'(1x,a)') 'Output file ?    : '
      read (*,*) fnameout
      
c     ---------- Mesh Reading
      call ReadMesh  (finname,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                NNODE,NELE,NSPIG)

c     ---------- Find suitable scaling factor
      do i=1,NELE
        lenm =lenm +(sqrt((xy(1,ele(1,i))-xy(1,ele(2,i)))**2 + 
     &                    (xy(2,ele(1,i))-xy(2,ele(2,i)))**2) +
     &               sqrt((xy(1,ele(2,i))-xy(1,ele(3,i)))**2 + 
     &                    (xy(2,ele(2,i))-xy(2,ele(3,i)))**2) +
     &               sqrt((xy(1,ele(3,i))-xy(1,ele(1,i)))**2 + 
     &                    (xy(2,ele(3,i))-xy(2,ele(1,i)))**2))/3.
      enddo
      lenm = lenm / NELE
      scla = lenm

c     ---------- Electromagnetic data reading
      open (1,file = fnameaux,status='OLD')

      read(1, *) finiz, ffina, nstep
      read(1, *) nmode
      read(1, *) nem
      read(1, *) (MLAB(i),i=1,nem)
      read(1, *) (EPS(i),i=1,nem)
      read(1, *) PECLAB
      
      close(1)

C     ---------- Normalize to scaled wavelength
 
      scl = 1000.*(0.2998/finiz)/scla
      write(*,*) scla,scl

      do i=1,NNODE
        xy(1,i)=xy(1,i)/scl
        xy(2,i)=xy(2,i)/scl
      enddo


c     ---------- Computation of fixed nodes and edges
      nl40 = 0
      do i = 1,Nspig
         if (SLAB(i) .eq. PECLAB) nl40 = nl40 + 1
      enddo
      nd40 = 0
      do i = 1,NNODE
         if (NLAB(i) .eq. PECLAB) nd40 = nd40 + 1
      enddo
      
      print *, 'Nodes  = ',nnode
      print *, '       Of which on p.e.c. = ',nd40
      print *, 'Edges  = ',nspig
      print *, '       Of which on p.e.c. = ',nl40
      nnn = NNODE + NSPIG - nl40 - nd40

      print *, 'Degrees of freedom = ',nnn
      
c     ---------- Building the frequency independent matrices
      call globm
     &     (MAXNODE, MAXNXE, MAXELE, MAXSPIG, KU, KL, KD, MAXMAT,
     &      ELE, ELAB, SPIG, XY, NNODE, NELE, NSPIG, 
     &      a_fi, b_fi)

c     ---------- Frequency cycle on a scaled frequency)
      do i = 1,nstep
	  
	 if (nstep .eq. 1) then
	    f = finiz
         else
            aux = ffina - finiz
            f = ((REAL(i)-1.) / (REAL(nstep)-1.) * aux + finiz)
         endif
	
         k02 = (2*3.141592*f/(scla*finiz))**2.

c     ----------- Augmenting A and B with frequency dependent terms
c                 And system solution
         do k = 1,NNODE+NSPIG
            do j = 1,NNODE+NSPIG
               a_fd(k, j) = a_fi(k, j)
               b_fd(k, j) = b_fi(k, j)
            enddo
         enddo
	 call dimmat
     &     (MAXNODE, MAXNXE, MAXELE, MAXSPIG, MAXMAT, KU, KL, KD,
     &      ELE, SPIG, XY, NNODE, NELE, NSPIG, MLAB, EPS, NEM,
     &      a_fd, b_fd, k02, nnn, ierr, peclab, ELAB, NLAB, SLAB,
     &      alfai, alfar, beta)

	 write(*, *) 'Iteration ',i,' concluded (IERR=',IERR,')'

c     ---------- Results Writing
         call write_res
     &        (fnameout, MAXNODE, MAXSPIG,
     &        i, NNODE, nd40, nl40, nnn,
     &        alfai, alfar, beta, f, scl , nmode)

      end do

200   continue

      write(*, *)
      if (info .eq. 1) then
	  write(*, *)'Execution aborted'
      else
	  write(*, *)'Iterations          : ',nstep
	  write(*, *)'Unknowns            : ',nnn
	  write(*, *)'Execution concluded succesfully'
	  write(*, *)
      end if

      end















