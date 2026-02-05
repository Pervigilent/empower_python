C  =====================================================================
c
      SUBROUTINE BUILD_FEMa(MAXNODE,MAXNXE,MAXELE,MAXBAND,MAXNXP,
     &                      ELE,xy,NLAB,ELAB,nele,nnode,Lpec,Lpmc,PO,
     &                      Ndie,Material,Eps,Np,FEM,ku,kl,kd,
     &                      Plane,Vcoeff,vkt2,AR,BR,LABC)
c
C  =====================================================================
c     BUILD the global FEM matrix for a E-Plane or H-Plane
c     (homogeneous) rectangular waveguide horns - Analitic ABC.
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      
      implicit none

C     [IN]
      integer MAXNODE,MAXNXE,MAXELE,MAXBAND,MAXNXP
      integer ELE(0:MAXNXE,MAXELE),NLAB(MAXNODE),ELAB(MAXELE),nele
      integer nnode,Lpec,Lpmc,Np,PO(0:MAXNXP,np) 
      INTEGER ku,kl,kd
      real    xy(2,MAXNODE),SE(8,8),TE(8,8),Eps(Ndie)
      complex vcoeff(0:NDIE), vkt2(0:NDIE),ar,br
      character*1 Plane
      integer Ndie,Material(Ndie),labc 
      
C     [OUT]      
      complex FEM(3*MAXBAND+1,MAXNODE) 
      

C     [LOCAL]
      integer    ie,imat,i,j,k,kk,iauxi,iauxj,iauxk
      real       lk
      complex    caux
      integer AK(3,3,3),BK(3,3,3)

      DATA AK / 2, 1, 0, 1, 2, 0, 0, 0, 0,
     &          0, 0, 0, 0, 2, 1, 0, 1, 2,
     &          2, 0, 1, 0, 0, 0, 1, 0, 2 /

      DATA BK / 1, -1,  0, -1,  1,  0,  0,  0,  0,
     &          0,  0,  0,  0,  1, -1,  0, -1,  1,
     &          1,  0, -1,  0,  0,  0, -1,  0,  1 /
       
C     [FUNCTIONS]      
      complex cbget
      
      DO ie=1,nele
        imat = 0
        if (Plane.eq.'H') then
          do i = 1,Ndie
            if (ELAB(ie).eq.Material(i)) then
              imat = i
            endif
          enddo
        endif

C     HAS THE ELEMENT AN EDGE ON THE FICTITIOUS BOUNDARY?
        KK = 0
        IF (NLAB(ELE(1,IE)).EQ.LABC.AND.NLAB(ELE(2,IE)).EQ.LABC) THEN
          KK  = 1
          LK = SQRT( ( Xy(1,ELE(1,IE)) - Xy(1,ELE(2,IE)) )**2 +
     &               ( xY(2,ELE(1,IE)) - xY(2,ELE(2,IE)) )**2 )
        ELSEIF (NLAB(ELE(2,IE)).EQ.LABC.AND.NLAB(ELE(3,IE)).EQ.LABC)THEN
          KK = 2
          LK = SQRT( ( Xy(1,ELE(2,IE)) - Xy(1,ELE(3,IE)) )**2 +
     &               ( xY(2,ELE(2,IE)) - xY(2,ELE(3,IE)) )**2 )
        ELSEIF (NLAB(ELE(3,IE)).EQ.LABC.AND.NLAB(ELE(1,IE)).EQ.LABC)THEN
          KK = 3
          LK = SQRT( ( Xy(1,ELE(1,IE)) - Xy(1,ELE(3,IE)) )**2 +
     &               ( xY(2,ELE(1,IE)) - xY(2,ELE(3,IE)) )**2 )
        ENDIF

        call ELENMAT(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,SE,8,TE,8)  
      
        do j=1,ele(0,ie)
          iauxj = ele(j,ie)
          iauxi  = NLAB(iauxj)
                                       
C         treat the node normally unless it is on a pec for plane 'E'
C         or pmc for plane 'H'                                                 
          if (.not. 
     &        ((iauxi .eq. LPEC .and. Plane .eq. 'H') .or.
     &         (iauxi .eq. LPMC .and. Plane .eq. 'E')) ) then

            do k=1,ele(0,ie)
              iauxk = ele(k,ie)
              
              caux = CBGET(FEM,iauxj,iauxk,ku,kl,kd,3*MAXBAND+1,
     &                     NNODE,'U')
              caux = caux + vcoeff(imat)*
     &               ( - SE(j,k) + vkt2(imat)*TE(j,k) )

c              caux = caux + vcoeff(imat)*
c     &               ( + SE(j,k) - vkt2(imat)*TE(j,k) )
              call CBPUT(caux,
     &                   FEM,iauxj,iauxk,ku,kl,kd,3*MAXBAND+1,NNODE,'U')
     
            enddo

          else                              
C           Imposes the BC
            call CBPUT((1.,0.),
     &                  FEM,iauxj,iauxj,ku,kl,kd,3*MAXBAND+1,NNODE,'U')
          endif
        enddo
C     HAS THE ELEMENT AN EDGE ON THE FICTITIOUS BOUNDARY?
        IF ( KK .GT. 0 ) THEN
C       ----------IF YES, AUGMENT THE GLOBAL MATRIX A WITH THE ABC
          DO j = 1,3
            iauxj = ELE(j,IE)
            DO k = 1,3
              iauxk = ELE(k,IE)
              caux = CBGET(FEM,iauxj,iauxk,ku,kl,kd,3*MAXBAND+1,
     &                     NNODE,'U')
              caux = caux -  AR*LK*AK(j,k,KK)/6.+BR/LK*BK(j,k,KK) 
c              caux = caux +  AR*LK*AK(j,k,KK)/6.-BR/LK*BK(j,k,KK) 
              call CBPUT(caux,
     &                   FEM,iauxj,iauxk,ku,kl,kd,3*MAXBAND+1,NNODE,'U')
            ENDDO
          ENDDO
        endif
      enddo 

C     Adds the pec conditions at the port sides for H plane
      if (Plane .eq. 'H') then
        do i=1,np
          do j=1,2
            if (j.eq.1) iauxj = po(1,i)
            if (j.eq.2) iauxj = po(po(0,i),i)
            do iauxk=max(1,iauxj-MAXBAND),min(nnode,iauxj+MAXBAND)
              call CBPUT((0.,0.),
     &                   FEM,iauxj,iauxk,ku,kl,kd,3*MAXBAND+1,NNODE,'U')
            enddo                                                       
            call CBPUT((1.,0.),
     &                   FEM,iauxj,iauxj,ku,kl,kd,3*MAXBAND+1,NNODE,'U')            
          enddo
        enddo
      endif
      
      RETURN
      END

C  =====================================================================
c
      SUBROUTINE BUILD_FEMb(MAXNODE,MAXNXE,MAXELE,MAXBAND,MAXNXP,
     &                      ELE,xy,NLAB,ELAB,nele,nnode,Lpec,Lpmc,PO,
     &                      Ndie,Material,Eps,Np,FEM,ku,kl,kd,
     &                      Plane,Vcoeff,vkt2,LPMAX,LPMAY,LPMAXY,
     &                      APMA)
c
C  =====================================================================
c     BUILD the global FEM matrix for a E-Plane or H-Plane
c     (homogeneous) rectangular waveguide horns - PMA.
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      
      implicit none

C     [IN]
      integer MAXNODE,MAXNXE,MAXELE,MAXBAND,MAXNXP
      integer ELE(0:MAXNXE,MAXELE),NLAB(MAXNODE),ELAB(MAXELE),nele
      integer nnode,Lpec,Lpmc,lpmax,lpmay,lpmaxy,Np,PO(0:MAXNXP,np) 
      INTEGER ku,kl,kd
      real    xy(2,MAXNODE),SXE(8,8),SYE(8,8),TE(8,8),Eps(Ndie)
      complex vcoeff(0:NDIE), vkt2(0:NDIE),APMA
      character*1 Plane
      integer Ndie,Material(Ndie) 
      
C     [OUT]      
      complex FEM(3*MAXBAND+1,MAXNODE) 
      

C     [LOCAL]
      integer    ie,imat,i,j,k,iauxi,iauxj,iauxk
      complex    caux,AA,BB,CC
       
C     [FUNCTIONS]      
      complex cbget
      
      DO ie=1,nele
        imat = 0
        if (Plane.eq.'H') then
          do i = 1,Ndie
            if (ELAB(ie).eq.Material(i)) then
              imat = i
            endif
          enddo
        endif

        call ELEAMAT(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,SXE,SYE,8,TE,8)  
      
        do j=1,ele(0,ie)
          iauxj = ele(j,ie)
          iauxi  = NLAB(iauxj)
                                       
C         treat the node normally unless it is on a pec for plane 'E'
C         or pmc for plane 'H'                                                 
          if (.not. 
     &        ((iauxi .eq. LPEC .and. Plane .eq. 'H') .or.
     &         (iauxi .eq. LPMC .and. Plane .eq. 'E')) ) then

            do k=1,ele(0,ie)
              iauxk = ele(k,ie)
              
              caux = CBGET(FEM,iauxj,iauxk,ku,kl,kd,3*MAXBAND+1,
     &                     NNODE,'U')
              if (ELAB(ie) .eq. lpmax .or.
     &            ELAB(ie) .eq. lpmay .or.
     &            ELAB(ie) .eq. lpmaxy ) then
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
                caux = caux + vcoeff(imat)*
     &                 ( - SXE(j,k)/BB - SYE(j,k)/AA + 
     &                   vkt2(0)*CC*TE(j,k) )
              else
                caux = caux + vcoeff(imat)*
     &                 ( - (SXE(j,k)+SYE(j,k)) + vkt2(imat)*TE(j,k) )
              endif

              call CBPUT(caux,
     &                   FEM,iauxj,iauxk,ku,kl,kd,3*MAXBAND+1,NNODE,'U')
     
            enddo

          else                              
C           Imposes the BC
            call CBPUT((1.,0.),
     &                  FEM,iauxj,iauxj,ku,kl,kd,3*MAXBAND+1,NNODE,'U')
          endif
        enddo
      enddo 

C     Adds the pec conditions at the port sides for H plane
      if (Plane .eq. 'H') then
        do i=1,np
          do j=1,2
            if (j.eq.1) iauxj = po(1,i)
            if (j.eq.2) iauxj = po(po(0,i),i)
            do iauxk=max(1,iauxj-MAXBAND),min(nnode,iauxj+MAXBAND)
              call CBPUT((0.,0.),
     &                   FEM,iauxj,iauxk,ku,kl,kd,3*MAXBAND+1,NNODE,'U')
            enddo                                                       
            call CBPUT((1.,0.),
     &                   FEM,iauxj,iauxj,ku,kl,kd,3*MAXBAND+1,NNODE,'U')            
          enddo
        enddo
      endif
      
      RETURN
      END

