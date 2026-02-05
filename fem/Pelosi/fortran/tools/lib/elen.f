C  =====================================================================
C
      SUBROUTINE ELENMAT(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,SE,LDS,TE,LDT)
C
C  =====================================================================
C     CONSTRUCTS THE ELEMENTAL MATRICES FOR NODAL ELEMENTS
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [PARAMETERS]
      integer LDE                 ! Maximum number of nodes in array EXY
      parameter (LDE = 8)         ! Should be equal to MAXNXE
      
C     [IN]
      integer MAXNODE             ! Maximum number of nodes
      integer MAXNXE              ! Maximum number of nodes per element
      integer MAXELE              ! Maximum number of elements
      integer IE                  ! Element to compute
      integer ELE(0:MAXNXE,MAXELE)! Connectivity matrix
      real    XY(2,MAXNODE)       ! Node Co-ordinates matrix  
      integer LDS,LDT             ! SE and TE matrices dimensions
C     [OUT]
      real    SE(LDS,LDS)         ! S element matrix
      real    TE(LDT,LDT)         ! T element matrix
C     [LOCAL]
      real    EXY(LDE,2)          ! Element nodes co-ordinates
      integer i                   ! an index
      
      do i=1,ELE(0,IE)
        EXY(i,1)=xy(1,ele(i,ie))
        EXY(i,2)=xy(2,ele(i,ie))                                            
      enddo
                                           
      if (ELE(0,IE).eq.3) then
C     ---------- First order Triangle
        call ELENT1(Exy,LDE,SE,LDS,TE,LDT)
        
      elseif (ELE(0,IE).eq.6) then
C     ---------- Second order Triangle      
        call ELENT2(Exy,LDE,SE,LDS,TE,LDT)

      elseif (ELE(0,IE).eq.4) then
C     ---------- First order Quadrilater
        call ELENQ1(Exy,LDE,SE,LDS,TE,LDT)

      elseif (ELE(0,IE).eq.8) then
C     ---------- Second order Quadrilater
        call ELENQ2(Exy,LDE,SE,LDS,TE,LDT)
      endif      
      
      RETURN
      END
   
C  =====================================================================
C
      SUBROUTINE ELENT1(Exy,LDE,SE,LDS,TE,LDT)
C
C  =====================================================================
C     CONSTRUCTS THE LOCAL MATRICES SL AND TL FOR A FIRST ORDER
C     TRIANGULAR ELEMENT.
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer LDE,LDS,LDT         ! SE,TE Matrices dimensions
      real    EXY(LDE,2)          ! Nodes co-ordinates    
C     [OUT]
      real    SE(LDS,LDS)         ! S element matrix
      real    TE(LDT,LDT)         ! T element matrix
C     [LOCAL]      
      INTEGER I,J
      REAL    AREA                ! Element Area
      REAL    DX(3),DY(3)         ! Differences between node

      if (LDS.lt.3 .or. ldt.lt.3) then
        write (*,*) '*** ERROR in calling ELENT1, LDS or LDT too small'
        stop
      endif
        
      AREA = ((EXY(2,1)-EXY(1,1))*(EXY(3,2)-EXY(1,2)) - 
     &        (EXY(3,1)-EXY(1,1))*(EXY(2,2)-EXY(1,2)))/2.

C     COMPUTE ENTRIES OF THE LOCAL MATRICES SE and TE
      DX(1) =  EXY(3,1) - EXY(2,1)
      DX(2) =  EXY(1,1) - EXY(3,1)
      DX(3) =  EXY(2,1) - EXY(1,1)
      DY(1) =  EXY(2,2) - EXY(3,2)
      DY(2) =  EXY(3,2) - EXY(1,2)
      DY(3) =  EXY(1,2) - EXY(2,2)

      DO I = 1,3
        DO J = 1,I
          SE(I,J) = (DY(I)*DY(J)+DX(I)*DX(J)) / (4. * AREA)
          SE(J,I) = SE(I,J)
          TE(I,J) = AREA / 12.
          TE(J,I) = TE(I,J)
        ENDDO
        TE(I,I) = 2. * TE(I,I)
      ENDDO

      RETURN
      END

C  =====================================================================
C
      SUBROUTINE ELENT2(Exy,LDE,SE,LDS,TE,LDT)
C
C  =====================================================================
C     CONSTRUCTS THE LOCAL MATRICES SL AND TL FOR A SECOND ORDER
C     TRIANGULAR ELEMENT.
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer LDE,LDS,LDT         ! SE,TE Matrices dimensions
      real    EXY(LDE,2)          ! Nodes co-ordinates    
C     [OUT]
      real    SE(LDS,LDS)         ! S element matrix
      real    TE(LDT,LDT)         ! T element matrix
C     [LOCAL]      
      INTEGER     I,J             ! Indice
      REAL        AREA            ! Element Area
      REAL        Co(3)           ! Cotangents
      Integer     TD(6,6),SD(6,6) ! Integer Matrices
      
      data TD / 6,  0,  0, -1, -4, -1,
     &          0, 32, 16,  0, 16, -4,
     &          0, 16, 32, -4, 16,  0,
     &         -1,  0, -4,  6,  0, -1,
     &         -4, 16, 15,  0, 32,  0,
     &         -1, -4,  0, -1,  0,  6 /
     
      data SD / 0,  0,  0,  0,  0,  0,
     &          0,  8, -8,  0,  0,  0,
     &          0, -8,  8,  0,  0,  0,
     &          0,  0,  0,  3, -4,  1,
     &          0,  0,  0, -4,  8, -4,
     &          0,  0,  0,  1, -4,  3 /
     
C     [EXTERNAL FUNCTIONS]      
      integer     IR6

      if (LDS.lt.6 .or. ldt.lt.6) then
        write (*,*) '*** ERROR in calling ELENT2, LDS or LDT too small'
        stop
      endif
      
      AREA = ((EXY(4,1)-EXY(1,1))*(EXY(6,2)-EXY(1,2)) - 
     &        (EXY(6,1)-EXY(1,1))*(EXY(4,2)-EXY(1,2)))/2.

C     COMPUTE ENTRIES OF THE LOCAL MATRICES SE and TE
      Co(1) =  ((EXY(4,1)-EXY(1,1))*(EXY(6,1)-EXY(1,1)) + 
     &          (EXY(4,2)-EXY(1,2))*(EXY(6,2)-EXY(1,2)))/(2.*Area)
      Co(2) =  ((EXY(6,1)-EXY(4,1))*(EXY(1,1)-EXY(4,1)) + 
     &          (EXY(6,2)-EXY(4,2))*(EXY(1,2)-EXY(4,2)))/(2.*Area)
      Co(3) =  ((EXY(1,1)-EXY(6,1))*(EXY(4,1)-EXY(6,1)) + 
     &          (EXY(1,2)-EXY(6,2))*(EXY(4,2)-EXY(6,2)))/(2.*Area)
      
      DO I = 1,6
        DO J = 1,I
          TE(I,J) = AREA*REAL(TD(I,J))/REAL(180)
          TE(J,I) = TE(I,J)
          SE(I,J) = (Co(1)*REAL(SD(IR6(i,0),IR6(j,0)))+
     &               Co(2)*REAL(SD(IR6(i,1),IR6(j,1)))+   
     &               Co(3)*REAL(SD(IR6(i,2),IR6(j,2))))/real(6)
          SE(J,I) = SE(I,J)
        ENDDO
      ENDDO

      RETURN
      END

C  =====================================================================
C
      INTEGER FUNCTION IR6(i,n)
C
C  =====================================================================
C     Perform index permutation for second order triangular elements
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]      
      integer i               ! Index
      integer n               ! Number of rotations
C     [LOCAL]
      integer itwist(6)       ! Permutations      
      integer j,it            ! Auxiliary integers
      DATA itwist /6, 3, 5, 1, 2, 4/
      
      it = i
      do j=1,n,1
        it = itwist(it)
      enddo
      IR6 = it
      return
      end       
      
C  =====================================================================
C
      SUBROUTINE ELENQ1(EXY,LDE,SE,LDS,TE,LDT)
C
C  =====================================================================
C     CONSTRUCTS THE LOCAL MATRICES SL AND TL FOR A FIRST ORDER
C     ISOPARAMETRIC QUADRILATERAL ELEMENT.
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer LDE,LDS,LDT         ! SE,TE Matrices dimensions
      real    EXY(LDE,2)          ! Nodes co-ordinates    
C     [OUT]
      real    SE(LDS,LDS)         ! S element matrix
      real    TE(LDT,LDT)         ! T element matrix
C     [LOCAL]                                            
      real    u,v                 ! element co-ordinates                     
      REAL    Shape(4)            ! Shape functions
      REAL    ShapeDU(4)          ! Its derivates with respect to u
      real    ShapeDV(4)          ! Its derivates with respect to u
      real    TJTI(2,2)           ! Inverse of transposed Jacobian
      real    DETJ                ! Determinant of Jacobian    
      integer i,j,m,n             ! Indices
      real    WQ(5), SQ(5)        ! Gauss quadrature
      real    SUM
      
      DATA WQ/0.2369269, 0.4786287, 0.5688889, 0.4786287, 0.2369269/
      DATA SQ/-0.9061799, -0.5384693, 0.0000000, 0.5384693, 0.9061799/
                                        
      if (LDS.lt.4 .or. ldt.lt.4) then
        write (*,*) '*** ERROR in calling ELENQ1, LDS or LDT too small'
        stop
      endif

      DO I=1,4
        DO J=1,4
          SE(I,J) = 0.
          TE(I,J) = 0.
        enddo
      enddo  

C     Perform a 5x5 points gauss quadrature
      DO M = 1,5
        DO N = 1,5
          U = SQ(M)
          V = SQ(N)
          CALL ISO4(EXY,LDE,U,V,Shape,ShapeDU,ShapeDV,TJTI,DETJ)
C         COMPUTE SE, TE CONTRIBUTIONS
          DO I = 1,4
            DO J = 1,I
              TE(I,J) = TE(I,J) + WQ(M)*WQ(N)*Shape(I)*Shape(J)*DETJ
              SUM = ShapeDU(I)*TJTI(1,1)*ShapeDU(J)+
     &              ShapeDU(I)*TJTI(1,2)*ShapeDV(J)+
     &              ShapeDV(I)*TJTI(2,1)*ShapeDU(J)+
     &              ShapeDV(I)*TJTI(2,2)*ShapeDV(J)
              SE(I,J) = SE(I,J) + WQ(M)*WQ(N)*SUM*DETJ
            enddo
          enddo
        enddo
      enddo

C     MAKE SE, TE SYMMETRIC!
      DO I = 1,8
        DO J = 1,I
          TE(J,I) = TE(I,J)
          SE(J,I) = SE(I,J)
        enddo
      enddo
      
      return
      end                                  
      
C  =====================================================================
C
      SUBROUTINE ISO4(EXY,LDE,U,V,Shape,ShapeDU,ShapeDV,TJTI,DETJ)
C  =====================================================================
C     DETERMINES  GEOMETRIC  PARAMETERS AND  DERIVATIVES OF AN
C     ISOPARAMETRIC ELEMENT WITH 4 NODES. 
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer LDE
      real    EXY(LDE,2),u,v
C     [OUT]
      REAL    Shape(4), ShapeDU(4), ShapeDV(4), TJTI(2,2)
      real    detj
C     [LOCAL]      
      REAL    TJ(2,2), TJI(2,2)    ! Jacobian and its inverse
      integer k,ks,is,js           ! some indexes
                  
C     INTERPOLATION FUNCTIONS FOR 8-NODED ELEMENT
      Shape(1) =   0.25*(u-1.0)*(v-1.0)
      Shape(2) = - 0.25*(u+1.0)*(v-1.0)
      Shape(3) =   0.25*(u+1.0)*(v+1.0)
      Shape(4) = - 0.25*(u-1.0)*(v+1.0)
      
C     FUNCTION DERIVATIVES WITH RESPECT TO U
      ShapeDU(1) =   0.25*(v-1.0)
      ShapeDU(2) = - 0.25*(v-1.0)
      ShapeDU(3) =   0.25*(v+1.0)
      ShapeDU(4) = - 0.25*(v+1.0)
      
C     FUNCTION DERIVATIVES WITH RESPECT TO V
      ShapeDV(1) =   0.25*(u-1.0)
      ShapeDV(2) = - 0.25*(u+1.0)
      ShapeDV(3) =   0.25*(u+1.0)
      ShapeDV(4) = - 0.25*(u-1.0)
         
C     COMPUTE THE JACOBIAN
      TJ(1,1) = 0.
      TJ(1,2) = 0.
      TJ(2,1) = 0.
      TJ(2,2) = 0.
      DO K = 1,4
         TJ(1,1) = TJ(1,1) + EXY(k,1)*ShapeDU(K)
         TJ(1,2) = TJ(1,2) + EXY(k,2)*ShapeDU(K)
         TJ(2,1) = TJ(2,1) + EXY(k,1)*ShapeDV(K)
         TJ(2,2) = TJ(2,2) + EXY(k,2)*ShapeDV(K)
      enddo

C     JACOBIAN DETERMINANT, INVERSE, TRANSPOSE-INVERSE
      DETJ = TJ(1,1)*TJ(2,2) - TJ(1,2)*TJ(2,1)
      TJI(1,1) =  TJ(2,2) / DETJ
      TJI(1,2) = -TJ(1,2) / DETJ
      TJI(2,1) = -TJ(2,1) / DETJ
      TJI(2,2) =  TJ(1,1) / DETJ
      DO IS = 1,2
        DO JS = 1,2
          TJTI(IS,JS) = 0.
          DO KS = 1,2
            TJTI(IS,JS) = TJTI(IS,JS) + TJI(KS,IS) * TJI(KS,JS)
          enddo
        enddo
      ENDDO
C
      RETURN
      END

C  =====================================================================
C
      SUBROUTINE ELENQ2(Exy,LDE,SE,LDS,TE,LDT) 
C
C  =====================================================================
C     CONSTRUCTS THE LOCAL MATRICES SL AND TL FOR A SECOND ORDER
C     ISOPARAMETRIC QUADRILATERAL ELEMENT.
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer LDE,LDS,LDT         ! SE,TE Matrices dimensions
      real    EXY(LDE,2)          ! Nodes co-ordinates    
C     [OUT]
      real    SE(LDS,LDS)         ! S element matrix
      real    TE(LDT,LDT)         ! T element matrix
C     [LOCAL]                       
      real    u,v                 ! element co-ordinates                     
      REAL    Shape(8)            ! Shape functions
      REAL    ShapeDU(8)          ! Its derivates with respect to u
      real    ShapeDV(8)          ! Its derivates with respect to u
      real    TJTI(2,2)           ! Inverse of transposed Jacobian
      real    DETJ                ! Determinant of Jacobian
      integer i,j,m,n             ! Indices
      real    WQ(5), SQ(5)        ! Gauss quadrature
      real    SUM
      
      DATA WQ/0.2369269, 0.4786287, 0.5688889, 0.4786287, 0.2369269/
      DATA SQ/-0.9061799, -0.5384693, 0.0000000, 0.5384693, 0.9061799/
      
      if (LDS.lt.8 .or. ldt.lt.8) then
        write (*,*) '*** ERROR in calling ELENQ2, LDS or LDT too small'
        stop
      endif
                                              
      DO I=1,8
        DO J=1,8
          SE(I,J) = 0.
          TE(I,J) = 0.
        enddo
      enddo  

C     Perform a 5x5 points gauss quadrature
      DO M = 1,5
        DO N = 1,5
          U = SQ(M)
          V = SQ(N)
          CALL ISO8(EXY,LDE,U,V,Shape,ShapeDU,ShapeDV,TJTI,DETJ)
C         COMPUTE SE, TE CONTRIBUTIONS
          DO I = 1,8
            DO J = 1,I
              TE(I,J) = TE(I,J) + WQ(M)*WQ(N)*Shape(I)*Shape(J)*DETJ
              SUM = ShapeDU(I)*TJTI(1,1)*ShapeDU(J)+
     &              ShapeDU(I)*TJTI(1,2)*ShapeDV(J)+
     &              ShapeDV(I)*TJTI(2,1)*ShapeDU(J)+
     &              ShapeDV(I)*TJTI(2,2)*ShapeDV(J)
              SE(I,J) = SE(I,J) + WQ(M)*WQ(N)*SUM*DETJ
            enddo
          enddo
        enddo
      enddo

C     MAKE SE, TE SYMMETRIC!
      DO I = 1,8
        DO J = 1,I
          TE(J,I) = TE(I,J)
          SE(J,I) = SE(I,J)
        enddo
      enddo
                  
      return
      end                                        
      
C  =====================================================================
C
      SUBROUTINE ISO8(EXY,LDE,U,V,Shape,ShapeDU,ShapeDV,TJTI,DETJ)
C  =====================================================================
C     DETERMINES  GEOMETRIC  PARAMETERS AND  DERIVATIVES OF AN
C     ISOPARAMETRIC ELEMENT WITH 4 or 8 NODES
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer LDE
      real    EXY(LDE,2),u,v
C     [OUT]
      REAL    Shape(8), ShapeDU(8), ShapeDV(8), TJTI(2,2)
      real    detj
C     [LOCAL]      
      REAL    TJ(2,2), TJI(2,2)    ! Jacobian and its inverse
      integer k,ks,is,js           ! some indexes
                  
C     INTERPOLATION FUNCTIONS FOR 8-NODED ELEMENT
      Shape(1) = 0.25*(1.0-u)*(1.0-v)*(-u-v-1.0)
      Shape(2) = 0.5*(1.0-u*u)*(1.0-v)
      Shape(3) = 0.25*(1.0+u)*(1.0-v)*(u-v-1.0)
      Shape(4) = 0.5*(1.0-v*v)*(1.0+u)  
      Shape(5) = 0.25*(1.0+u)*(1.0+v)*(u+v-1.0)
      Shape(6) = 0.5*(1.0-u*u)*(1.0+v)
      Shape(7) = 0.25*(1.0-u)*(1.0+v)*(-u+v-1.0)
      Shape(8) = 0.5*(1.0-v*v)*(1.0-u)

C     FUNCTION DERIVATIVES WITH RESPECT TO U
      ShapeDU(1) = 0.25*(v+1.0)*(2.0*u+v)
      ShapeDU(2) = (v+1.0)*(-u)
      ShapeDU(3) = 0.25*(1.0+v)*(2.0*u-v)
      ShapeDU(4) = 0.5*(v*v-1.0)   
      ShapeDU(5) = 0.25*(1.0-v)*(2.0*u+v)
      ShapeDU(6) = (v-1.0)*u
      ShapeDU(7) = 0.25*(1.0-v)*(2.0*u-v)
      ShapeDU(8) = 0.5*(1.0-v*v)

C     FUNCTION DERIVATIVES WITH RESPECT TO V
      ShapeDV(1) = 0.25*(u+1.0)*(2.0*v+u)
      ShapeDV(2) = 0.5*(1-u*u)
      ShapeDV(3) = 0.25*(1.0-u)*(2.0*v-u)
      ShapeDV(4) = (u-1.0)*v
      ShapeDV(5) = 0.25*(1.0-u)*(2.0*v+u)
      ShapeDV(6) = 0.5*(u*u-1.0)
      ShapeDV(7) = 0.25*(1.0+u)*(2.0*v-u)
      ShapeDV(8) = (u+1.0)*(-v)
         
C     COMPUTE THE JACOBIAN
      TJ(1,1) = 0.
      TJ(1,2) = 0.
      TJ(2,1) = 0.
      TJ(2,2) = 0.
      DO K = 1,8
         TJ(1,1) = TJ(1,1) + EXY(k,1)*ShapeDU(K)
         TJ(1,2) = TJ(1,2) + EXY(k,2)*ShapeDU(K)
         TJ(2,1) = TJ(2,1) + EXY(k,1)*ShapeDV(K)
         TJ(2,2) = TJ(2,2) + EXY(k,2)*ShapeDV(K)
      enddo

C     JACOBIAN DETERMINANT, INVERSE, TRANSPOSE-INVERSE
      DETJ = TJ(1,1)*TJ(2,2) - TJ(1,2)*TJ(2,1)
      TJI(1,1) =  TJ(2,2) / DETJ
      TJI(1,2) = -TJ(1,2) / DETJ
      TJI(2,1) = -TJ(2,1) / DETJ
      TJI(2,2) =  TJ(1,1) / DETJ
      DO IS = 1,2
        DO JS = 1,2
          TJTI(IS,JS) = 0.
          DO KS = 1,2
            TJTI(IS,JS) = TJTI(IS,JS) + TJI(KS,IS) * TJI(KS,JS)
          enddo
        enddo
      ENDDO
C
      RETURN
      END
     
