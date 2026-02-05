C  =====================================================================
C
      PROGRAM Cyl
C
C  =====================================================================
C     2-D scattering from infinite cylinders extending in the z-direction
C     excited by a plane wave
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      IMPLICIT NONE
      integer MAXNODE,MAXELE,MAXNXE,MAXBAND,MAXDIEL,KU,KL,KD
                                               
      parameter (
     &  MAXNODE      = 1200,     !Maximum number of nodes
     &  MAXELE       = 1400,     !Maximum number of elements
     &  MAXNXE       = 8,        !Maximum number of nodes per element
     &  MAXBAND      = 85,       !Maximum band
     &  MAXDIEL      = 5,        !Maximum number of different dielectrics
     &  KU           = 85,       !Upper band
     &  KL           = 85,       !Lower band
     &  KD           = 0)        !Symmetric band
 
c     FEM Mesh and related variables
      integer       ele(0:8,MAXELE)    !Connectiviti matrix
      real          xy(2,MAXNODE)      !Node co-ordinates
      integer       nlab(MAXNODE)      !Node labels
      integer       elab(MAXELE)       !Element labels
      integer       nnode              !Node number
      integer       nele               !Element number
      integer       ABCTYPE            !Type of ABC
                                       ! 1 = Analitic 1st order
                                       ! 2 = PMA
      integer       LABC               !Label of nodes on Absorbing 
                                       !boundary, if TYPEABC=1
      integer       LPMAX              !Labels of elements in PMA, X dir. 
      integer       LPMAY              !Labels of elements in PMA, Y dir. 
      integer       LPMAXY             !Labels of elements in PMA, corners
      complex       APMA               !PMA coefficient a=b=1/c 
      integer       LPEC               !Label of nodes on PEC 
      integer       LPMC               !Labels of nodes on PMC 
      integer       Ndie               !Number of dielectrics
      complex       Eps(MAXDIEL)       !Permittivities
      complex       Mu(MAXDIEL)        !Permeabilities
      integer       Material(MAXDIEL)  !Material labels

C     Solving system variables
      character*1   UPLO 
      complex       A(3*MAXBAND+1,MAXNODE) !Left hand side matrix
      complex       B(MAXNODE)             !Right Hand side vector
      complex       AR,BR                  !ABC coefficients
      
C     File names and auxiliary variables 
      character*256 fgeoname           !Geometry file
      character*256 felename           !Electromagnetic file
      character*256 foutname           !Output file Name 
      character*256 fname              !Output file Name 
      character*1   Polarization       !Flag for field polarization
      integer       i,j,ie 
      integer       ipvt(MAXNODE),INFO,iband 
      integer       LDIR,DIRC 
      real          r,phi 
      complex       Coeffp(MAXDIEL),Coeffq(MAXDIEL)
 
C     Functions 
      integer       nbandex 
      complex       einc 
 
      real PI,K0 
      parameter (pi = 3.141592653589793238, 
     &           K0 = 2*pi                 ) 
       
 
c     Reading Mesh file
      write (*,*) '+--- Begin '
      write (*,'(1x,a)') '+--- Mesh file NAME ? '
      read  (*,*) fgeoname
      write (*,*) '+--- Reading Mesh File '
      call ReadNodalMesh  (fgeoname,MAXNODE,MAXNXE,MAXELE,
     &                     XY,ELE,NLAB,ELAB,NNODE,NELE)

c     Reading Input Parameters 
      write (*,'(1x,a)') '+--- Electromagnetic file NAME ? '
      read  (*,*) felename
      write (*,*) '+--- Reading Electromagnetic File '
      open (unit=1,status='old',
     &      form='formatted',file=felename)
      read(1,*) phi                       ! Incidence direction [deg]
      read(1,*) ABCTYPE                   ! TYPE of ABC
      read(1,*) LABC                      ! Label of nodes on ABC if 
      read(1,*) LPMAX,LPMAy,LPMAXY        ! Label of elements in PMA
      read(1,*) APMA                      ! PMA coefficient
      read(1,*) LPEC                      ! Label of nodes on PEC 
      read(1,*) LPMC                      ! Label of nodes on PMC
      read(1,*) Ndie                      ! No. of dielectrics
      read(1,*)(Eps(i),i=1,Ndie)          ! Permittivity of materials
      read(1,*)(Mu(i),i=1,Ndie)           ! Permeability of materials
      read(1,*)(Material(i),i=1,Ndie)     ! Material Labels
       read(1,'(a1)') polarization         ! Inc. Field polarization (E/H) 
      close(1) 
 
C     Output file 
      write (*,'(1x,a)') '+--- Output file NAME ? (no extension) '
      read  (*,*) foutname
      
C     Minor settings       
      phi = phi * pi/180. 
      if (polarization .eq. 'h') polarization = 'H' 
      if (polarization .eq. 'e') polarization = 'E' 
      if ((polarization.ne. 'E') .and. (polarization.ne. 'H')) then 
        write(*,*) '***ERROR*** Unrecognized polarization' 
        stop 
      endif   
      if ( polarization .eq. 'E') then 
        LDIR = LPEC 
        DIRC = -1 
        do i=1,Ndie 
          Coeffp(i) = 1/Mu(Material(i)) 
          Coeffq(i) = Eps(Material(i)) 
        enddo 
      else if ( polarization .eq. 'H') then 
        LDIR = LPMC 
        DIRC = - 1 
        do i=1,Ndie 
          Coeffp(i) = 1/Eps(Material(i)) 
          Coeffq(i) = Mu(Material(i)) 
        enddo 
      endif 
      UPLO = 'T' 
 
C     Verifies Bandwidth
      iband = nbandex(MAXELE,MAXNXE,ELE,NELE)
      if (iband.gt.maxband) then
        write(*,*) '*** ERROR *** MAXBAND should be at least to ',iband
        stop
      endif    
        
C     Set the coefficients AR and BR for the ABC
      R = 0.
      DO I = 1,NNODE
        IF (Xy(1,I) .GT. R) R = Xy(1,I)
      ENDDO
      BR = 1./(2.*((0.,1.)*K0+1./R))
      AR = (0.,1.)*K0 + 1./(2.*R) - BR/(4.*R**2)

C     Set to zero the global matrix and the right hand side
      do i=1,3*maxband+1
        do j=1,nnode
          a(i,j) = (0.,0.)
        enddo
      enddo
      do i=1,nnode  
        B(I) = (0.,0.)
      ENDDO

C     Assemble the global matrix
      write (*,*) '+--- Assembling FEM Matrix with analitic ABC' 
      if (ABCTYPE.eq.1) then
        CALL EMBEDA (UPLO,MAXNODE,MAXNXE,MAXELE,MAXDIEL,MAXBAND,
     &               ELE,xy,NLAB,ELAB,LABC,LDIR,DIRC, 
     &               COEFFP,COEFFQ,ie,nele,nnode,A,B,ku,kl,kd,
     &               AR,BR,R,phi) 
      else
        CALL EMBEDB (UPLO,MAXNODE,MAXNXE,MAXELE,MAXDIEL,MAXBAND,
     &               ELE,xy,NLAB,ELAB,LPMAX,LPMAY,LPMAXY,APMA,LDIR,
     &               DIRC,COEFFP,COEFFQ,nele,nnode,A,B,ku,
     &               kl,kd,phi,Ndie) 
      endif        

C     Solving the system of linear equations 
      write (*,*) '+--- Computing LU Factorization of FEM Matrix ' 
      call CGBTRF(NNODE,NNODE,kl,ku,A,3*MAXBAND+1,IPVT,INFO)
      IF(INFO .NE. 0) THEN
        WRITE(*,*) '***ERROR*** in LU factorization, INFO =', INFO
        STOP
      ENDIF

      write (*,*) '+--- Back Substitution ' 
      call CGBTRS('N', NNODE, KL, KU, 1, A,
     &            3*MAXBAND+1, IPVT, B, NNODE, INFO)
      IF(INFO .NE. 0) THEN
        WRITE(*,*) '***ERROR*** in solving, INFO =', INFO
        STOP
      ENDIF      

C     Store Results
      write(*,*) '+--- Storing diffracted field'
      fname = foutname
      fname(INDEX(foutname,' '):)='.bcs'
      CALL writecres (fname,B,nnode,1,'M')
      do i=1,nnode
        B(i) = B(i)+einc(xy(1,i),xy(2,i),phi)
      enddo
      fname = foutname
      fname(INDEX(foutname,' '):)='.tot'
      write(*,*) '+--- Storing total field' 
      CALL writecres (fname,B,nnode,1,'M') 

      END

 









