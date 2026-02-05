C  =====================================================================
C
      subroutine ELEENMAT (MAXNODE,MAXNXE,MAXELE,
     &                     XY,ELE,IE,GG,LDG)
C  =====================================================================
C     CONSTRUCTS THE ELEMENTAL MATRICES FOR MIXED ELEMENTS
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [PARAMETERS]
      integer LDE                 ! Maximum number of nodes in array EXY
      parameter (LDE = 8)         ! Should be equal to MAXNXE
      INTEGER   ldim
      parameter (LDIM = 3)        ! leading dimension della matrice
      
C     [IN]
      integer MAXNODE             ! Maximum number of nodes
      integer MAXNXE              ! Maximum number of nodes per element
      integer MAXELE              ! Maximum number of elements
      integer IE                  ! Element to compute
      integer ELE(0:MAXNXE,MAXELE)! Connectivity matrix
      real    XY(2,MAXNODE)       ! Node Co-ordinates matrix  
      integer LDG                 ! GG matrix dimension
      
C     [OUT]
      real    GG(LDG,LDG)         ! G element matrix

C     [LOCAL]
      real    EXY(LDE,2)          ! Element nodes co-ordinates
      INTEGER I
      REAL    Co(3)               ! Coefficients
      REAL    b(3),c(3)           ! Coefficients
      real    Ar                  ! Area

C     [EXTERNAL FUNCTIONS]
      Integer     GD(3,3)         ! Integer Matrices
      
      data GD /  0,  1, -1,
     &           0, -2,  2, 
     &           0,  1, -1 /

      do i=1,ELE(0,IE)
        EXY(i,1)=xy(1,ele(i,ie))
        EXY(i,2)=xy(2,ele(i,ie))                                             
      enddo

      b(1)  =   EXY(2,2)-EXY(3,2) 
      b(2)  =   EXY(3,2)-EXY(1,2) 
      b(3)  =   EXY(1,2)-EXY(2,2) 

      c(1)  =   EXY(3,1)-EXY(2,1) 
      c(2)  =   EXY(1,1)-EXY(3,1) 
      c(3)  =   EXY(2,1)-EXY(1,1) 

      Co(1) =  sqrt((c(3))**2+(b(3))**2)/
     &             (EXY(1,1)*b(1)+EXY(2,1)*b(2)+EXY(3,1)*b(3))**2 
      Co(2) =  sqrt((c(1))**2+(b(1))**2)/
     &             (EXY(1,1)*b(1)+EXY(2,1)*b(2)+EXY(3,1)*b(3))**2  
      Co(3) =  sqrt((c(2))**2+(b(2))**2)/
     &             (EXY(1,1)*b(1)+EXY(2,1)*b(2)+EXY(3,1)*b(3))**2  

      Ar = (c(3)*b(2)-b(3)*c(2))

      gg(1,1) = Co(1)*(b(1)*(b(1)-b(2))+c(1)*(c(1)-c(2)))*Ar/6.
      gg(1,2) = -Co(1)*(b(2)*(b(2)-b(1))+c(2)*(c(2)-c(1)))*Ar/6.
      gg(1,3) = Co(1)*(-EXY(1,2)*b(2)-EXY(2,2)*b(1)-EXY(3,2)*b(3)+
     &                  EXY(1,1)*c(2)+EXY(2,1)*c(1)+EXY(3,1)*c(3))*Ar/6.

      gg(2,1) = Co(2)*(-EXY(1,2)*b(1)-EXY(2,2)*b(3)-EXY(3,2)*b(2)+
     &                  EXY(1,1)*c(1)+EXY(2,1)*c(3)+EXY(3,1)*c(2))*Ar/6.
      gg(2,2) = Co(2)*(b(2)*(b(2)-b(3))+c(2)*(c(2)-c(3)))*Ar/6.
      gg(2,3) = -Co(2)*(b(3)*(b(3)-b(2))+c(3)*(c(3)-c(2)))*Ar/6.

      gg(3,1) = -Co(3)*(b(1)*(b(1)-b(3))+c(1)*(c(1)-c(3)))*Ar/6. 
      gg(3,2) = Co(3)*(-EXY(1,2)*b(3)-EXY(2,2)*b(2)-EXY(3,2)*b(1)+
     &                  EXY(1,1)*c(3)+EXY(2,1)*c(2)+EXY(3,1)*c(1))*Ar/6.
      gg(3,3) = Co(3)*(b(3)*(b(3)-b(1))+c(3)*(c(3)-c(1)))*Ar/6. 

      return
      end





















