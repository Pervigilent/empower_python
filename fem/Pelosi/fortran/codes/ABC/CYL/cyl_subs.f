C  =====================================================================
C
      subroutine EMBEDA (UPLO,MAXNODE,MAXNXE,MAXELE,MAXDIEL,MAXBAND,
     &                   ELE,xy,NLAB,ELAB,LABC,LDIR,DIRC,
     &                   Coeffp,Coeffq,ie,nele,nnode,A,B,ku,kl,kd,
     &                   AR,BR,rho,phi)
C
C  =====================================================================
C     EMBEDS LOCAL MATRIX SL OF THE ELEMENT IE INTO THE GLOBAL MATRIX
C     A OR INTO THE RIGHT HAND SIDE, AS APPROPRIATE. THIS EXPLOITS
C     ANALYTIC ABC.
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]      
      IMPLICIT NONE
      CHARACTER*1 UPLO     !'U' Stores only Upper Triangle 
                           !'L' Stores only Lower Triangle 
                           !'T' Stores all matrix 
      integer MAXNODE,MAXNXE,MAXELE,MAXDIEL,MAXBAND,ELE(0:MAXNXE,MAXELE)
      integer NLAB(MAXNODE),ELAB(MAXELE),NELE,NNODE 
      integer LABC         ! Label of nodes on absorbing boundary 
      integer LDIR         ! Label of nodes on Dirichlet boundary 
      integer DIRC         ! Coefficient of scattered field 
                           ! at Dirichlet boundary (Uscat=DIRC*Uinc) 
      complex Coeffp(MAXDIEL),Coeffq(MAXDIEL),AR,BR
      integer IE,ku,kl,kd
      real    xy(2,MAXNODE),SE(8,8),TE(8,8),rho,phi 

C     [IN/OUT]      
      complex A(3*MAXBAND+1,MAXNODE)     ! Left hand side matrix
      complex B(MAXNODE)                 ! Right Hand side vector

C     [LOCAL]
      integer I,J,K,KP1                  ! indices
      integer IROW,ICOL                  ! indexes to entries global FEM Matrix
      complex caux,Ek,Ekp1
      real    LK,PI,K0,K02
 
C     [EXTERNAL FUNCTIONS] 
      complex EINC
      complex cbget
 
      integer AK(3,3,3),BK(3,3,3)
      DATA AK / 2, 1, 0, 1, 2, 0, 0, 0, 0,
     &          0, 0, 0, 0, 2, 1, 0, 1, 2,
     &          2, 0, 1, 0, 0, 0, 1, 0, 2 /

      DATA BK / 1, -1,  0, -1,  1,  0,  0,  0,  0,
     &          0,  0,  0,  0,  1, -1,  0, -1,  1,
     &          1,  0, -1,  0,  0,  0, -1,  0,  1 /
      
      parameter (PI = 3.141592653589793238) 
      K0 = 2. * PI 
      K02= K0 * K0

      do ie=1,NELE

          call ELENMAT(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,SE,8,TE,8)  

C     HAS THE ELEMENT AN EDGE ON THE FICTITIOUS BOUNDARY?
        K = 0
        IF (NLAB(ELE(1,IE)).EQ.LABC.AND.NLAB(ELE(2,IE)).EQ.LABC) THEN
          K  = 1
          LK = SQRT( ( Xy(1,ELE(1,IE)) - Xy(1,ELE(2,IE)) )**2 +
     &              ( xY(2,ELE(1,IE)) - xY(2,ELE(2,IE)) )**2 )
        ELSEIF (NLAB(ELE(2,IE)).EQ.LABC.AND.NLAB(ELE(3,IE)).EQ.LABC)THEN
           K = 2
           LK = SQRT( ( Xy(1,ELE(2,IE)) - Xy(1,ELE(3,IE)) )**2 +
     &            ( xY(2,ELE(2,IE)) - xY(2,ELE(3,IE)) )**2 )
        ELSEIF (NLAB(ELE(3,IE)).EQ.LABC.AND.NLAB(ELE(1,IE)).EQ.LABC)THEN
          K = 3
          LK = SQRT( ( Xy(1,ELE(1,IE)) - Xy(1,ELE(3,IE)) )**2 +
     &            ( xY(2,ELE(1,IE)) - xY(2,ELE(3,IE)) )**2 )
        ENDIF
                                    
        DO I = 1,3
          IROW = ELE(I,IE)
          IF (NLAB(IROW) .EQ. LDIR) THEN 
C     ---------- ENFORCE DIRICHLET BOUNDARY CONDITIONS 
            call CBPUT((1.,0.),
     &                 A,IROW,IROW,ku,kl,kd,3*MAXBAND+1,NNODE,'U') 
            B(IROW) = DIRC * EINC(Xy(1,IROW),xY(2,IROW),PHI) 
 
          ELSE 
            DO J = 1,3
              ICOL = ELE(J,IE)
C     ----------AUGMENT THE GLOBAL MATRIX A 
              caux = CBGET(A,irow,icol,ku,kl,kd,3*MAXBAND+1,
     &                     NNODE,'U')
              caux = caux + Coeffp(ELAB(IE)) * SE(i,j) - 
     &                      k02 * COEFFQ(ELAB(IE)) * TE(i,j) 
              call CBPUT(caux,
     &                 A,IROW,ICOL,ku,kl,kd,3*MAXBAND+1,NNODE,'U')          
C     ----------AUGMENT THE RIGHT HAND SIDE
              B(IROW) = B(IROW) - (coeffp(ELAB(IE)) * SE(I,J) - 
     &                             K02*COEFFQ(ELAB(IE)) * TE(I,J))
     &                           * EINC(Xy(1,ICOL),xY(2,ICOL),PHI)
            ENDDO
          ENDIF
        ENDDO

C     HAS THE ELEMENT AN EDGE ON THE FICTITIOUS BOUNDARY?
        IF ( K .GT. 0 ) THEN
C     ----------IF YES, AUGMENT THE GLOBAL MATRIX A WITH THE ABC
          DO I = 1,3
            IROW = ELE(I,IE)
            DO J = 1,3
              ICOL = ELE(J,IE)
              caux = CBGET(A,irow,icol,ku,kl,kd,3*MAXBAND+1,
     &                     NNODE,'U')
              caux = caux + AR*LK*AK(I,J,K)/6.+BR/LK*BK(I,J,K) 
              call CBPUT(caux,
     &                 A,IROW,ICOL,ku,kl,kd,3*MAXBAND+1,NNODE,'U')          
            ENDDO
          ENDDO
C     ----------AUGMENT THE RIGHT HAND SIDE
          kp1=k+1
          if (kp1.gt.3) kp1=kp1-3
          call  EIPRIME(xy(1,ele(k,ie)),xy(2,ele(k,ie)),
     &                  xy(1,ele(kp1,ie)),xy(2,ele(kp1,ie)),
     &                  rho,phi,Ek,Ekp1)
          B(ele(k,ie))   = B(ele(k,ie))   + Ek
          B(ele(kp1,ie)) = B(ele(kp1,ie)) + Ekp1

        endif
      ENDdo

      RETURN
      END

C  =====================================================================
C
      subroutine EMBEDB (UPLO,MAXNODE,MAXNXE,MAXELE,MAXDIEL,MAXBAND,
     &                   ELE,xy,NLAB,ELAB,LPMAX,LPMAY,LPMAXY,APMA,LDIR,
     &                   DIRC,COEFFP,COEFFQ,nele,nnode,A,B,ku,
     &                   kl,kd,phi,Ndie) 
C
C  =====================================================================
C     EMBEDS LOCAL MATRIX SL OF THE ELEMENT IE INTO THE GLOBAL MATRIX
C     A OR INTO THE RIGHT HAND SIDE, AS APPROPRIATE. THIS EXPLOITS
C     PMA TYPE ABC.
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]      
      IMPLICIT NONE
      CHARACTER*1 UPLO     !'U' Stores only Upper Triangle 
                           !'L' Stores only Lower Triangle 
                           !'T' Stores all matrix 
      integer MAXNODE,MAXNXE,MAXELE,MAXDIEL,MAXBAND,ELE(0:MAXNXE,MAXELE)
      integer NLAB(MAXNODE),ELAB(MAXELE) 
      integer LPMAX,LPMAY,LPMAXY ! Label of elements on the PMA 
      integer LDIR         ! Label of nodes on Dirichlet boundary 
      integer DIRC         ! Coefficient of scattered field 
                           ! at Dirichlet boundary (Uscat=DIRC*Uinc) 
      complex CoeffP(MAXDIEL),CoeffQ(MAXDIEL),APMA,AA,BB,CC
      integer Nele,NNode,Ndie,ku,kl,kd,ie
      real    xy(2,MAXNODE),SXE(8,8),SYE(8,8),TE(8,8),phi 

C     [IN/OUT]      
      complex A(3*MAXBAND+1,MAXNODE)     ! Left hand side matrix
      complex B(MAXNODE)                 ! Right Hand side vector

C     [LOCAL]
      integer I,J                        ! indices
      integer IROW,ICOL                  ! indexes to entries global FEM Matrix
      complex caux
      real    PI,K0,K02
 
C     [EXTERNAL FUNCTIONS] 
      complex CBGET
      complex EINC
       
      parameter (PI = 3.141592653589793238) 
      K0 = 2. * PI 
      K02= K0 * K0 
                             
      do ie=1,nele

        call ELEAMAT(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,SXE,SYE,8,TE,8)  
         
        DO I = 1,3
          IROW = ELE(I,IE)
          IF (NLAB(IROW) .EQ. LDIR) THEN 
C     ---------- ENFORCE DIRICHLET BOUNDARY CONDITIONS 
            call CBPUT((1.,0.),
     &                 A,IROW,IROW,ku,kl,kd,3*MAXBAND+1,NNODE,'U') 
            if (ELAB(ie).ne.LPMAX.and.ELAB(ie).ne.LPMAY.and.
     &          ELAB(ie).ne.LPMAXY) then
              B(IROW) = DIRC * EINC(Xy(1,IROW),xY(2,IROW),PHI)
            endif 
          ELSE 
            DO J = 1,3
              ICOL = ELE(J,IE)
              IF (ELAB(IE).le.NDIE) THEN
C     ----------AUGMENT THE GLOBAL MATRIX A 
                caux = CBGET(A,irow,icol,ku,kl,kd,3*MAXBAND+1,
     &                       NNODE,'U')
                caux = caux + Coeffp(ELAB(IE)) * (SXE(i,j) + SYE(i,j))  
     &                        - k02 * COEFFQ(ELAB(IE)) * TE(i,j) 
                call CBPUT(caux,
     &                   A,IROW,ICOL,ku,kl,kd,3*MAXBAND+1,NNODE,'U')          
C     ----------AUGMENT THE RIGHT HAND SIDE
                B(IROW) = B(IROW) - (coeffp(ELAB(IE))*
     &                        (SXE(i,j) + SYE(i,j)) - K02*
     &                        COEFFQ(ELAB(IE)) * 
     &                        TE(I,J)) * EINC(Xy(1,ICOL),xY(2,ICOL),PHI)

C     ----------AUGMENT THE GLOBAL MATRIX A
              else
                if(ELAB(IE).EQ.LPMAX) then
                  AA=APMA
                  BB=1./APMA
                  CC=APMA
                else if(ELAB(IE).EQ.LPMAY) then  
                  AA=1./APMA
                  BB=APMA
                  CC=APMA
                else  
                  AA=1.
                  BB=1.
                  CC=APMA*APMA
                endif
                caux = CBGET(A,irow,icol,ku,kl,kd,3*MAXBAND+1,
     &                       NNODE,'U')
                caux = caux + 
     &                          SXE(i,j)/BB +
     &                          SYE(i,j)/AA +
     &                          - k02 * CC * TE(i,j) 
                call CBPUT(caux,
     &                   A,IROW,ICOL,ku,kl,kd,3*MAXBAND+1,NNODE,'U')          

           
                if (nlab(ele(i,ie)).eq.0) then
                  B(IROW) = B(IROW) - 
     &                          (SXE(i,j) + SYE(i,j) - K02* TE(I,J))
     &                          * EINC(Xy(1,ICOL),xY(2,ICOL),PHI)
                endif
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      enddo

      RETURN
      END

C  =============================================================
C
      complex FUNCTION EINC(X,Y,PHI)
C
C  =============================================================
C     COMPUTE THE INCIDENT FIELD IN THE POINT WITH COORDINATE
C     X AND Y.
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================

      implicit none

C     [IN]      
      REAL  X,Y,PHI

C     [LOCAL]
      REAL        PI,K0

      parameter (PI = 3.141592653589793238)
      K0 = 2. * PI

      EINC = CEXP( (0.,1.)*K0* ( X*COS(PHI)+Y*SIN(PHI) ) )

      RETURN
      END

C  =============================================================
C
      subroutine EIPRIME(x1,y1,x2,y2,
     &                   rho,phi,Ek,Ekp1)
C
C  =============================================================
C     COMPUTE THE LINEAR INTEGRAL OF THE RHO DERIVATIVE OF THE
C     INCIDENT FIELD FROM THE POINT WITH COORDINATE
C     X1,Y1 TO THE POINT OF COORDINATES X2,Y2.
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================

      implicit none

C     [IN]      
      REAL     X1,Y1,X2,Y2,RHO,PHI

C     [OUT]
      complex  Ek, Ekp1

C     [LOCAL]
      real     phik,deltaphi
      REAL     PI,K0
      integer  i

C     [Order 7 gaussian integration on [0:1] data]
      real w(7)
      real x(7)

      data x /0.0254460438, 0.1292344072, 0.2970774243, 0.5000000000,
     &        0.7029225757, 0.8707655928, 0.9745539562 /
      data w /0.0647424831, 0.1398526957, 0.1909150253, 0.2089795918,
     &        0.1909150253, 0.1398526957, 0.0647424831 /
      
      parameter (PI = 3.141592653589793238)
      K0 = 2. * PI
      
      phik = atan2(Y1,X1)
      deltaphi=atan2(Y2,X2)-phik
      
      if(deltaphi.gt.PI) deltaphi = 2*PI - deltaphi
      if(deltaphi.lt.-PI) deltaphi = 2*PI + deltaphi

      Ek   = (0.,0.)
      Ekp1 = (0.,0.)
      do i=1,7
        Ek   = Ek + 
     &         w(i) * (1-x(i)) * cos (phik - phi + x(i)*deltaphi) * 
     &         exp((0.,1.)*K0*rho*cos(phik - phi + x(i)*deltaphi))
        Ekp1 = Ekp1 +
     &         w(i) * x(i) * cos (phik - phi + x(i)*deltaphi) * 
     &         exp((0.,1.)*K0*rho*cos(phik - phi + x(i)*deltaphi))
      enddo

      Ek   = (0.,1.) * k0 * rho * deltaphi * Ek
      Ekp1 = (0.,1.) * k0 * rho * deltaphi * Ekp1

      return
      end






