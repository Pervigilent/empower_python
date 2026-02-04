C  =====================================================================
c
      SUBROUTINE VETCINIT (A, NR)
c
C  =====================================================================
C     Put a vector to 0
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      implicit     none
C     [IN]
      integer NR
C     [IN/OUT]
      complex A(NR)
C     [LOCAL]
      integer I
      
      do I=1,NR
        a(i)=(0.,0.)
      enddo
      return
      end       
            
C  =====================================================================
c
      SUBROUTINE PORTS(MAXNODE,MAXNXP,NLAB,XY,PO,np,nnode,dim,LP)
c
C  =====================================================================
C     Builds the vector of the nodes on each port
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      implicit     none

C     [IN]
      integer     MAXNODE,MAXNXP,NLAB(MAXNODE),np,nnode,LP(Np)
      real        XY(2,MAXNODE),dim          
      
C     [OUT]
      integer     PO(0:MAXNXP,np) 
      
C     [LOCAL]      
      integer     ip, i, iaux, j

C     Inizializing
      do i=1,np
        PO(0,i)=0
      enddo
      
c     finds nodes on port ip
      do i=1,nnode
        do ip=1,np
          if (NLAB(i) .eq. LP(ip)) then
C           the node IS on port IP
            PO(0,ip)        = PO(0,ip) + 1
            if (PO(0,ip) .gt. MAXNXP) then
C             check for problems  
              write(*,*) '** error **, too many nodes on port ', ip 
              stop
            else
              PO(PO(0,ip),ip) = i
            endif
          endif
        enddo
      enddo
      
c     sort PO so that nodes are adiacent
      do ip = 1,np
        if (abs(xy(1,PO(1,ip))-xy(1,PO(2,ip))) .gt. dim*1.e-3) then
c         (bubble)sort on x          
          do i=1,PO(0,ip)
            do j=i+1,PO(0,ip)
              if ( xy(1,PO(i,ip)) .gt. xy(1,PO(j,ip)) ) then
                iaux  = PO(i,ip)
                PO(i,ip) = PO(j,ip)
                PO(j,ip)  = iaux
              endif
            enddo
          enddo
        else
c         (bubble)sort on y
          do i=1,PO(0,ip)
            do j=i+1,PO(0,ip)
              if ( xy(2,PO(i,ip)) .gt. xy(2,PO(j,ip)) ) then
                iaux  = PO(i,ip)
                PO(i,ip) = PO(j,ip)
                PO(j,ip) = iaux
              endif
            enddo
          enddo
        endif
      enddo

      return
      end

C  =====================================================================
c
      SUBROUTINE BUILD_FEM(MAXNODE,MAXNXE,MAXELE,MAXBAND,MAXNXP,
     &                     ELE,xy,NLAB,ELAB,nele,nnode,Lpec,Lpmc,PO,
     &                     Ndie,Material,Eps,Np,FEM,ku,kl,kd,
     &                     Plane,Vcoeff,vkt2)
c
C  =====================================================================
c     BUILD the global FEM matrix for a E-Plane or H-Plane
c     (homogeneous) rectangular waveguide junction.
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      
      implicit none

C     [IN]
      integer MAXNODE,MAXNXE,MAXELE,MAXBAND,MAXNXP
      integer ELE(0:MAXNXE,MAXELE),NLAB(MAXNODE),ELAB(MAXELE),nele
      integer nnode,Lpec,Lpmc,Np,PO(0:MAXNXP,np) 
      INTEGER ku,kl,kd
      real    xy(2,MAXNODE),SE(8,8),TE(8,8),Eps(Ndie)
      complex vcoeff(0:NDIE), vkt2(0:NDIE)
      character*1 Plane
      integer Ndie,Material(Ndie) 
      
C     [OUT]      
      complex FEM(3*MAXBAND+1,MAXNODE) 
      

C     [LOCAL]
      integer    ie,imat,i,j,k,iauxi,iauxj,iauxk
      complex    caux
       
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

C  =====================================================================
c
      SUBROUTINE BUILD_AC(MAXNODE,MAXNXP,MAXPORTS,MAXMODE,XY,AC,BI,RZ,
     &                    PO,a,k0,k02,zeta0,Np,Nm,Plane)
c
C  =====================================================================
c     BUILD the AC matrix for a E-Plane or H-Plane
c     (homogeneous) rectangular waveguide junction.
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      implicit none
C     [PARAMETERS]
      real PI
      parameter (pi = 3.141592653589793238)
      
C     [IN]
      integer MAXNODE,MAXNXP,MAXPORTS,MAXMODE,Np,Nm
      integer PO(0:MAXNXP,MAXPORTS)                                
      character*1 Plane
      real    a,k0,k02,zeta0,xy(2,MAXNODE)
      
C     [OUT]      
      complex     AC (MAXPORTS*MAXMODE,MAXPORTS*MAXMODE)
      complex     RZ(MAXMODE,MAXPORTS),BI(MAXMODE,MAXPORTS)
      

C     [LOCAL]
      real    dist,x1,x2,y1,y2,abp  
      integer ip,i1,m
      
C     [FUNCTIONS]
      complex naimsqrt,preasqrt      
              
C     [IN LINE FUNCTIONS]
      dist(x1,y1,x2,y2) = sqrt( (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) )
        
      do ip=1,np
        abp = dist(xy(1,PO(1,ip)),        xy(2,PO(1,ip)),
     &             xy(1,PO(PO(0,ip),ip)), xy(2,PO(PO(0,ip),ip)))
        i1 = (ip-1)*nm       
        if (Plane .eq.'E') then
c     LSE10 (or TE10) mode
        Bi(1,ip) = naimsqrt(cmplx(k02-(pi/a)*(pi/a),0.))
        RZ(1,ip) = exp(-(0.,1.)*pi/4.) * sqrt( k0*zeta0*a/(2*pi) ) *
     &                 bi(1,ip) * bi(1,ip)
        AC(i1+1,i1+1) = - RZ(1,ip) * abp
c  ----- LSE1m modes
        do m=1,nm-1
          bi(m+1,ip) = naimsqrt( cmplx( k02 - (pi/a)*(pi/a)
     &                           - (m*pi/abp)*(m*pi/abp),0.) )
          RZ(m+1,ip) = RZ(1,ip)
          AC(i1+m+1,i1+m+1) =  AC(i1+1,i1+1) / 2.
        enddo
      else
        do m=1,nm
          bi (m,ip)  = naimsqrt( cmplx( k02 -
     &                         (m*pi/abp)*(m*pi/abp) ))
          Rz (m,ip)  = preasqrt( k0 * zeta0 / bi(m,ip) )
          AC(i1+m,i1+m) = m * (Pi/2) * RZ(m,ip)
          enddo
       endif
      enddo

      return
      end                         
 
 
C  =====================================================================
c      
      SUBROUTINE BUILD_BC_CD (MAXNODE,MAXNXP,MAXPORTS,MAXMODE,XY,
     &                        BC,CD,BI,RZ,PO,a,Np,Nm,Plane)
c
C  =====================================================================
c     BUILD the BC and CD matrices for a E-Plane or H-Plane
c     (homogeneous) rectangular waveguide junction.
C     DUMMY ARGUMENTS ARE COMMENTED IN THE CALLING MODULE
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      implicit     none
C     [PARAMETERS]
      real PI
      parameter (pi = 3.141592653589793238)

C     [IN]
      integer MAXNODE,MAXNXP,MAXPORTS,MAXMODE,Np,Nm
      integer PO(0:MAXNXP,MAXPORTS)                                
      character*1 Plane
      real    a,xy(2,MAXNODE)                 
      
C     [OUT]      
      complex     BC (MAXPORTS*MAXMODE,MAXPORTS*MAXNXP)
      complex     CD (MAXPORTS*MAXNXP,MAXPORTS*MAXMODE)
      complex     RZ(MAXMODE,MAXPORTS),BI(MAXMODE,MAXPORTS)
      
C     [LOCAL]      
      real    dist,x1,x2,y1,y2,abp,pia,piabp,dx,integ
      integer ip,ie,i1,i2,im,in,j,m,nel,nord
      complex aux
      
C     [FUNCTIONS]
      complex pjn_expa

C     [IN LINE FUNCTIONS]
      dist(x1,y1,x2,y2) = sqrt( (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) )

C     Elements are second order      
      nord = 2                 
      
      in = 0
      if (Plane .eq. 'E') then
        pia = pi / a
        do ip=1,np
          abp = dist( xy(1,PO(1,ip)), xy(2,PO(1,ip)),
     &                xy(1,PO(PO(0,ip),ip)), xy(2,PO(PO(0,ip),ip)) )
        piabp = pi / abp
        nel = (PO(0,ip) - 1) / nord
        im = (ip-1)*nm
        if (ip .gt. 1) in = in + PO(0,ip-1)
          do ie = 1,nel
            i1  = (ie-1) * nord + 1
            i2  = i1 + nord
            dx  = dist( xy(1,PO(i1,ip)), xy(2,PO(i1,ip)),
     &                  xy(1,PO(i2,ip)), xy(2,PO(i2,ip)) )
            x1  = dist( xy(1,PO( 1,ip)), xy(2,PO( 1,ip)),
     &                  xy(1,PO(i1,ip)), xy(2,PO(i1,ip)) )
            do j=1,nord+1
c     LSE10 mode
              integ = dx * real( Pjn_expa(nord+1,j,(0.,0.)) )
              BC(im+1,in+i1+j-1) = BC(im+1,in+i1+j-1) + integ
              CD(in+i1+j-1,im+1) = CD(in+i1+j-1,im+1) - (0.,1.) *
     &                             BI(1,ip) * RZ(1,ip) * integ
c     LSE1m modes
              do m=1,nm-1
                aux = (0.,1.) * m * piabp
                integ = dx * real( exp(aux*x1) *
     &                             Pjn_expa(nord+1,j,aux*dx) )
                BC(im+m+1,in+i1+j-1) = BC(im+m+1,in+i1+j-1) + integ
                CD(in+i1+j-1,im+m+1) = CD(in+i1+j-1,im+m+1) - (0.,1.) *
     &                                 BI(m+1,ip) * RZ(m+1,ip) * integ
              enddo
            enddo
          enddo
        enddo
      else
        do ip=1,np
          abp = dist( xy(1,PO(1,ip)), xy(2,PO(1,ip)),
     &                xy(1,PO(PO(0,ip),ip)), xy(2,PO(PO(0,ip),ip)) )
          piabp = pi / abp
          nel = (PO(0,ip) - 1) / nord
          im = (ip-1)*nm
          if (ip .gt. 1) in = in + PO(0,ip-1)
          do ie = 1,nel
            i1  = (ie-1) * nord + 1
            i2  = i1 + nord
            dx  = dist( xy(1,PO(i1,ip)), xy(2,PO(i1,ip)),
     &                  xy(1,PO(i2,ip)), xy(2,PO(i2,ip)) )
            x1  = dist( xy(1,PO( 1,ip)), xy(2,PO( 1,ip)),
     &                  xy(1,PO(i1,ip)), xy(2,PO(i1,ip)) )
            do j=1,nord+1
              do m=1,nm
                aux = (0.,1.) * m * piabp
                integ = dx * aimag( exp(aux*x1) *
     &                              Pjn_expa(nord+1,j,aux*dx) )
                BC(im+m,in+i1+j-1) = BC(im+m,in+i1+j-1) + integ
                CD(in+i1+j-1,im+m) = CD(in+i1+j-1,im+m) +
     &                               aux * BI(m,ip) * RZ(m,ip) * integ
              enddo
            enddo
          enddo
        enddo
      endif

      return
      end
     
C  =====================================================================
c
      COMPLEX FUNCTION NAIMSQRT(arg)
c
C  =====================================================================
c     Computes a complex sqrt and choose among the results that with
c     a negative imaginary part
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      implicit    none
      complex     arg

      naimsqrt = csqrt (arg)
      if (aimag(naimsqrt) .gt. 0. ) naimsqrt = - naimsqrt

      return
      end

c  =============================================================
c
      COMPLEX FUNCTION PREASQRT(arg)
c
C  =====================================================================
c     Computes a complex sqrt and choose among the results that with
c     a positive real part
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      implicit    none
      complex     arg

      preasqrt = csqrt (arg)
      if (real (preasqrt) .lt. 0. ) preasqrt = - preasqrt

      return
      end

































