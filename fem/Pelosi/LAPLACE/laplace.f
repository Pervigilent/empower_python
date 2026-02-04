C ======================================================================
      PROGRAM LAPLACE
C ======================================================================
C     SOLVES THE ELECTROSTATIC PROBLEM THROUGH AN APPLICATION OF THE
C     FINITE ELEMENT METHOD. THE PROBLEM CAN CONTAIN AT MOST 
C       - MAXPOT  DIFFERENT FIXED POTENTIAL 
C       - MAXDIE  DIFFERENT UNIFORM DIELECTRICS
C
C     THE FEM IMPLEMENTATION CAN CONTAIN AT MOST
C       - MAXNODE NODES
C       - MAXELE  ELEMENTS, EACH CONTAINING AT MOST MAXNXE NODES
C    
C     THE OUTPUT ARE:
C       - THE ELECTROSTATIC ENERGY
C       - THE CAPACITY of the structure. this latter is valid
C         only for problems with two fixed potentials
C
C     THIS PROGRAM CAN TREAT ONLY FIRST ORDER TRIANGULAR ELEMENTS
C ======================================================================
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C ======================================================================
C     ---------- PARAMETERS
      parameter (MAXNODE = 300,        ! Maximum No. of Nodes
     &           MAXNXE  = 3,          ! Maximum No. of Nodes in Element
     &           MAXELE  = 300,        ! Maximum No. of Elements
     &           MAXPOT  = 10,         ! Maximum No. of Fixed Potentials
     &           MAXDIE  = 10 )        ! Maximum No. of Dielectrics

C     ---------- Primary mesh variables    
      real    XY(2,MAXNODE)            ! Node Co-ordiantes of 
      integer ELE(0:MAXNXE,MAXELE)     ! Connettivity matrix
      integer NLAB(MAXNODE)            ! Node Labels
      integer ELAB(MAXELE)             ! Element Labels   
      integer NNODE                    ! Number of Nodes
      integer NELE                     ! Number of Elements                                           
     
C     ---------- Electric variables
      real    FP(MAXPOT)               ! The fixed potentials 
      real    DE(MAXDIE)               ! The relative Permittivities
      
C     ---------- System variables
      real    SG(MAXNODE,MAXNODE)      ! Global system matrix
      real    B(MAXNODE)               ! right hand side
      real    X(MAXNODE)               ! Unknown vector
      
C     ---------- File Names
      CHARACTER*256      FILEFEM       ! Geometry file (The mesh)
      CHARACTER*256      FILEELE       ! Electrical data file (fixed 
                                       !        potentials and relative 
                                       !        permittivities)
      CHARACTER*256      FILERES       ! Output file with the results 

C     ---------- Other variables                    
      INTEGER I,J,IE                   ! indices
      real    SE(3,3)                  ! Elemental Matrix

C     ---------- Results
      real    Energy                   ! Stored energy
      real    Capacity                 ! Calculated capacity
      
C     -----------------------------------------------------------------
C     ---------- File names Input      
      write (*,*) ' Geometry file name         [INPUT] ? '
      READ (*,*)  FILEFEM
      write (*,*) ' Electrical data file name  [INPUT] ? '
      READ (*,*)  FILEELE
      write(*,*)  ' Results file name         [OUTPUT] ? '
      READ (*,*)  FILERES
      
C     ---------- Read Input data from FILEFEM
      write (*,*) '1 - READING MESH DATA '
      call ReadNodalMesh (filefem,MAXNODE,MAXNXE,MAXELE,
     &                    XY,ELE,NLAB,ELAB,NNODE,NELE)              

C     ---------- Read Input data from FILEELE
      write (*,*) '2 - READING ELECTRIC DATA'
      call ReadElect (fileele,MAXPOT,MAXDIE,FP,DE)

C     ---------- Matrix Initialization BACK TO MKS
      DO I = 1, nNODe
        xy(1,i)=xy(1,i)/1000.
        xy(2,i)=xy(2,i)/1000.
      enddo

C     ---------- Matrix Initialization
      DO I = 1, nNODe
        DO J = 1, nNODe
          SG(I,J) = 0.
        ENDDO
        B(I) = 0.
      ENDDO

C     ---------- Global Matrix & Right hand side Assembly
      WRITE (*,*) '3 - ASSEMBLING GLOBAL MATRIX'
      DO IE = 1, nELE                                   
C       ---------- Create the Elemental Matrix SE
        CALL SETRI(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,SE)  
C       ---------- Embeds SE in the global matrix SG and 
C                  Augment the right hand side vector B        
        CALL EMBED(MAXNODE,MAXNXE,MAXELE,MAXPOT,MAXDIE,
     &             ELE,NLAB,ELAB,FP,DE,IE,SE,SG,B)               
      ENDDO

C     ---------- Solve the linear system        
      WRITE (*,*) '4 - SOLVING THE SYSTEM'
      call solve (NNODE,SG,MAXNODE,B,X)

C     ---------- Computes electrostatic energy
      energy=0.
      do ie=1,nele 
        CALL SETRI(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,SE) 
        if (elab(ie).ge.1) then
          epsilon=de(elab(ie))
        else
          epsilon=1.
        endif    
        do j=1,3
          do k=1,3                                      
            energy=energy+se(j,k)*x(ele(j,ie))*x(ele(k,ie))*epsilon
          enddo
        enddo
      enddo
      energy=energy*(8.85e-12)/ 2      
      capacity=4.*energy*(fp(2)-fp(1))**2
      write (*,*) '5 - RESULTS:'
      write (*,*) '      Energy In the computational box: ', energy
      write (*,*) '      Energy In the complete problem : ', energy*2.
      write (*,*) '      Capacity of the system         : ', capacity
            
C     ---------- STORE RESULTS
      WRITE (*,*) '5 - WRITING RESULTS'
      CALL OUTPUT (FILEres,X,nnode)
      
      END

C  =====================================================================
C
      SUBROUTINE SETRI(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,SE)
C
C  =====================================================================
C     CONSTRUCTS THE ELEMENTAL MATRIX SE FOR A FIRST ORDER
C     TRIANGULAR ELEMENT. DUMMY ARGUMENTS ARE COMMENTED IN
C     THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer MAXNODE,MAXNXE,MAXELE,ELE(0:MAXNXE,MAXELE)
      real    XY(2,MAXNODE)         
C     [OUT]
      real    SE(3,3)
C     [LOCAL]
      INTEGER I1,I2,I3              ! Local node numbers
      integer I,J                   ! indices
      REAL    AREA                  ! Element area
      real    DX(3),DY(3)           ! differences between 
                                    ! Element vertices co-ordinates

C     ---------- SET UP INDICES FOR TRIANGLE
      I1 = ELE(1,IE)
      I2 = ELE(2,IE)
      I3 = ELE(3,IE)

C     ---------- DETERMINE ELEMENT AREA
      AREA = ( (Xy(1,I2) - Xy(1,I1)) * (xY(2,I3) - xY(2,I1)) -
     &         (Xy(1,I3) - Xy(1,I1)) * (xY(2,I2) - xY(2,I1))  ) / 2.

C     ---------- COMPUTE ENTRIES OF THE LOCAL MATRIX SE
      DX(1) =  Xy(1,I3) - Xy(1,I2)
      DX(2) =  Xy(1,I1) - Xy(1,I3)
      DX(3) =  Xy(1,I2) - Xy(1,I1)
      DY(1) =  xY(2,I2) - xY(2,I3)
      DY(2) =  xY(2,I3) - xY(2,I1)
      DY(3) =  xY(2,I1) - xY(2,I2)

      DO I = 1,3
        DO J = 1,I
          SE(I,J) = (DY(I)*DY(J)+DX(I)*DX(J)) / (4. * AREA)
          SE(J,I) = SE(I,J)
        ENDDO
      ENDDO

      RETURN
      END


C  =====================================================================
C
      subroutine EMBED(MAXNODE,MAXNXE,MAXELE,MAXPOT,MAXDIE,
     &                 ELE,NLAB,ELAB,FP,DE,IE,SE,SG,B)    
C
C  =====================================================================
C     EMBEDS LOCAL MATRIX SL OF THE ELEMENT IE INTO THE GLOBAL MATRIX
C     SG OR INTO THE RIGHT HAND SIDE, AS APPROPRIATE.
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]      
      integer MAXNODE,MAXNXE,MAXELE,MAXPOT,MAXDIE,ELE(0:MAXNXE,MAXELE)
      integer NLAB(MAXNODE),ELAB(MAXELE)
      real    FP(MAXPOT),DE(MAXDIE)
      real    se(3,3)                       
      INTEGER IE
C     [IN/OUT]      
      real    SG(MAXNODE,MAXNODE),B(MAXNODE)
C     [LOCAL]
      INTEGER I,J                      ! indices
      integer IROW,ICOL                ! idexes to entries in SG
      real    epsilon                  ! actual permittivity
      logical flag                     

c     ---------- Finds the relative permittivity of the element      
      epsilon =1.
      if (elab(ie).gt.1) epsilon = de(elab(ie))
      
c     ---------- Embeds the matrix
      DO I = 1,3
        IROW = ELE(I,IE)
C     ---------- HAS THE ROW NODE A FIXED POTENTIAL?
        IF (NLAB(IROW).ge.1) then
          SG(IROW,IROW) = 1.
          B(IROW)       = fp(nlab(irow))
        ELSE
C     ---------- OTHERWISE DO THE 3 COLUMNS
          DO J = 1,3
            ICOL = ELE(J,IE)
C     ---------- HAS THE COLUMN NODE A FIXED POTENTIAL?
            flag = .false.
            IF (NLAB(ICOL).ge.1) then
              B(IROW) = B(IROW) - Se(I,J)*epsilon * fp(nlab(icol))
            ELSE
C     ---------- IF NO, AUGMENT THE GLOBAL MATRIX
              SG(IROW,ICOL) = SG(IROW,ICOL) + Se(I,J)*epsilon
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      RETURN
      END

C  =====================================================================
C
      SUBROUTINE OUTPUT (FILEres,x,nnode)
C
C  =====================================================================
C     Writes an output file containing the nodal values
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      CHARACTER*256 FILEres
      real    x(nNODE)                
      integer nnode
C     [LOCAL]
      INTEGER I

      OPEN(UNIT=1,STATUS='UNKNOWN',FORM='FORMATTED',
     &                         FILE=FILEres)

C     ---------- Write an header
      WRITE(1,'(a7,I5)') 'type = ',1
      WRITE(1,'(a9,I5)') 'number = ',nnode

c     ---------- write nodal values
      DO I = 1,nnode
        WRITE(1,'(I5,1x,f13.5)')I,x(I)
      ENDDO
      close (1)

      RETURN
      END

C  =====================================================================
C
      subroutine ReadElect (fileele,MAXPOT,MAXDIE,FP,DE)
C
C  =====================================================================
C     Reads an input file containig electrical data
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      character*256 fileele
      integer maxpot,maxdie
      real    fp(maxpot),de(maxdie)
C     [LOCAL]
      integer i,j      
      OPEN(UNIT=1,STATUS='OLD',FORM='FORMATTED',
     &                         FILE=FILEele)

      read (1,*) npot
      do i=1,npot
        read (1,*) j,fp(j)
      enddo
      read (1,*) ndie
      do i=1,ndie
        read (1,*) j,de(j)
      enddo
      return
      end
      
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

C  =====================================================================
C
      subroutine solve (NNODE,SG,MAXNODE,B,X)
C
C  =====================================================================
C     Very simple and straightforward gauss algorithm with pivoting
C     should be substituted with more accurate routines from netlib
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real    SG(MAXNODE,MAXNODE),B(MAXNODE)
      integer NNODE                         
C     [OUT]
      real    X(MAXNODE)      
C     [LOCAL]      
      integer i,j,k,imax,jmax,ipiv(300)! indices and pivoting
      real coef,sum,tmp                ! Temporary variables
                                        
      do i=1,nnode-1
        ipiv(i)=i
      enddo
                                                
      do i=1,nnode-1
        imax=i
        jmax=i  
C     ---------- Row pivoting        
        do j=i,nnode
          if (abs(sg(j,i)).gt.abs(sg(imax,i))) imax=j
        enddo
        if (imax.ne.i) then
          do j=i,nnode
            tmp=sg(i,j)
            sg(i,j)=sg(imax,j)
            sg(imax,j)=tmp
          enddo    
          tmp=b(i)
          b(i)=b(imax)
          b(imax)=tmp
        endif                
C     ---------- Column pivoting        
        do j=i,nnode
          if (abs(sg(i,j)).gt.abs(sg(i,jmax))) jmax=j
        enddo
        if (jmax.ne.i) then
          do j=i,nnode
            tmp=sg(j,i)
            sg(j,i)=sg(j,jmax)
            sg(j,jmax)=tmp
          enddo    
          ipiv(i)=jmax
          ipiv(jmax)=i
        endif                
C     ---------- Subtraction from lower rows        
        if (abs(SG(i,i)).lt.1.e-6) write (*,*) 'i=',i
        do j=i+1,nnode
          coef = SG(j,i)/SG(i,i)
          SG(j,i)=0.
          if (abs(coef).gt.1.e-6) then
            do k=i+1,nnode
              sg(j,k)=sg(j,k)-coef*sg(i,k)
            enddo
          endif
          b(j)=b(j)-coef*b(i)
        enddo
      enddo
C     ---------- Substitution       
      x(nnode)=b(nnode)/sg(nnode,nnode)
      do i=nnode-1,1,-1
        sum=b(i)        
        do j=i+1,nnode
          sum=sum-x(j)*sg(i,j)
        enddo
        x(i)=sum/sg(i,i)
      enddo
C     ---------- Backward pivoting
      do i=1,nnode
        if (ipiv(i).ne.i) then
          do j=i+1,nnode
            if (ipiv(j).eq.i) then
              ipiv(j)=ipiv(i)
              ipiv(i)=i
              tmp=x(j)
              x(j)=x(i)
              x(i)=tmp
            endif
          enddo
        endif
      enddo        
      return
      end    
            
