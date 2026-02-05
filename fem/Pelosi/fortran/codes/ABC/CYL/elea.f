C  =====================================================================
C
      SUBROUTINE ELEAMAT(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,
     &                   SXE,SYE,LDS,TE,LDT)
C
C  =====================================================================
C     CONSTRUCTS THE ELEMENTAL MATRICES FOR NODAL ELEMENTS IN 
C     ANISOTROPIC MEDIA, TRIANGULAR AND FIRST ORDER.
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
      real    SXE(LDS,LDS)        ! Sx element matrix
      real    SYE(LDS,LDS)        ! Sy element matrix
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
        call ELEAT1(Exy,LDE,SXE,SYE,LDS,TE,LDT)
      endif      
      
      RETURN
      END
   
C  =====================================================================
C
      SUBROUTINE ELEAT1(Exy,LDE,SXE,SYE,LDS,TE,LDT)
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
      real    SXE(LDS,LDS)        ! Sx element matrix
      real    SYE(LDS,LDS)        ! Sy element matrix
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
        DO J = 1,3
          SXE(I,J) = DY(I)*DY(j)/(4.*AREA)
          SYE(I,J) = DX(I)*DX(J)/(4.*AREA)
        ENDDO
      ENDDO
      DO I = 1,3
        DO J = 1,I
          TE(I,J) = AREA / 12.
          TE(J,I) = TE(I,J)
        ENDDO
        TE(I,I) = 2. * TE(I,I)
      ENDDO

      RETURN
      END

