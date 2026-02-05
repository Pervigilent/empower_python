C  =====================================================================
C
      SUBROUTINE ELEEMAT(MAXNODE,MAXNXE,MAXELE, 
     &                   XY,ELE,IE,SE,LDS,TE,LDT)
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
C     ---------- First order Triangular Edge Elements
        call ELEET1(Exy,LDE,SE,LDS,TE,LDT)
      endif
            
      RETURN
      END
   
C  =====================================================================
C
      SUBROUTINE ELEET1(Exy,LDE,EE,LDEE,FE,LDFE)
C
C  =====================================================================
C     CONSTRUCTS THE LOCAL MATRICES SL AND TL FOR A FIRST ORDER
C     TRIANGULAR EDGE ELEMENT.
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer LDE,LDEE,LDFE       ! EE,FE Matrices dimensions
      real    EXY(LDE,2)          ! Nodes co-ordinates    
C     [OUT]
      real    EE(LDEE,LDEE)       ! E element matrix
      real    FE(LDFE,LDFE)       ! F element matrix
C     [LOCAL]      
      INTEGER I,J
      REAL    AREA                ! Element Area
      REAL    length(3)           ! Differences between node
C     [EXTERNAL FUNCTIONS]
      REAL    EFFE

      
      AREA = ((EXY(2,1)-EXY(1,1))*(EXY(3,2)-EXY(1,2)) - 
     &        (EXY(3,1)-EXY(1,1))*(EXY(2,2)-EXY(1,2)))/2.
                         
      length(1)=sqrt( (Exy(1,1)-Exy(2,1))**2 + (Exy(1,2)-Exy(2,2))**2 )
      length(2)=sqrt( (Exy(2,1)-Exy(3,1))**2 + (Exy(2,2)-Exy(3,2))**2 )
      length(3)=sqrt( (Exy(3,1)-Exy(1,1))**2 + (Exy(3,2)-Exy(1,2))**2 )
      
C     ---------- Matrix E      
      do i=1,3
        do j=1,i
          EE(i,j) = length(i)*length(j)/area
          EE(j,i) = EE(i,j)
        enddo
      enddo
      
C     ---------- Matrix F
      FE(1,1) = length(1)*length(1)*
     &          (effe(Exy,2,2)-effe(Exy,1,2)+effe(Exy,1,1))/(24*area)
      FE(2,1) = length(1)*length(2)*
     &          (effe(Exy,2,3)-effe(Exy,2,2)-2*effe(Exy,1,3)+
     &          effe(Exy,1,1))/(48*area)
      FE(3,1) = length(1)*length(3)*
     &          (effe(Exy,2,1)-2*effe(Exy,2,3)-effe(Exy,1,1)+
     &          effe(Exy,1,3))/(48*area)
      FE(1,2) = FE(2,1)
      FE(2,2) = length(2)*length(2)*
     &          (effe(Exy,3,3)-effe(Exy,2,3)+effe(Exy,2,2))/(24*area)
      FE(3,2) = length(2)*length(3)*
     &          (effe(Exy,3,1)-Effe(Exy,3,3)-2*effe(Exy,2,1)+
     &          effe(Exy,2,3))/(48*area)
      FE(1,3) = FE(3,1)
      FE(2,3) = FE(3,2)
      FE(3,3) = length(3)*length(3)*
     &          (effe(Exy,1,1)-effe(Exy,1,3)+effe(Exy,3,3))/(24*area)

      RETURN
      END

C  =====================================================================
C
      real function effe(Exy,i,j)
C
C  =====================================================================
C     CONSTRUCTS THE LOCAL MATRICES SL AND TL FOR A FIRST ORDER
C     TRIANGULAR EDGE ELEMENT.
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real    EXY(3,2)          ! Nodes co-ordinates    
      integer i,j               ! iNDEXES
      
c     [LOCAL]
      integer     ip,id,jp,jd         !#  Nodi in questione
      
c     CODICE                 
      ip=mod(i+1,3)+1
      id=mod(i,3)+1
      jp=mod(j+1,3)+1
      jd=mod(j,3)+1
      
      EFFE=(Exy(id,1)-Exy(ip,1))*(Exy(jd,1)-Exy(jp,1)) + 
     &     (Exy(id,2)-Exy(ip,2))*(Exy(jd,2)-Exy(jp,2))
      
      return
      end

     
