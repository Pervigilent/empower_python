C  =====================================================================
C
      subroutine Periodicity (MAXNODE,XY,NLAB,nnode,EPSM,NEQ,
     &                        xmin,xmax,ymin,ymax,  
     &                        lfdx,ldx,lpecdx,lpmcdx,
     &                        lfsx,lsx,lpecsx,lpmcsx)
C
C  =====================================================================
C     Finds the correspondence for nodes on the lateral boundary
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real    xy(2,MAXNODE)
      integer MAXNODE,nlab(MAXNODE),nnode
      integer lfdx,ldx,lpecdx,lfsx,lsx,lpecsx,lpmcdx,lpmcsx
      real    EPSM

C     [OUT]
      integer neq(MAXNODE)
      real    xmin,xmax,ymin,ymax                                                  
      
C     [LOCAL]
      real    xtoll,ytoll          !Tolerances
      integer i,j
      logical FLAG  
      
      xtoll = epsm
      ytoll = epsm
      
      XMIN=XY(1,1)
      XMAX=XY(1,1)
      YMIN=XY(2,1)
      YMAX=XY(2,1)
      
      do i=1,nnode         
        Neq(i)=i
        if (XMIN.ge.XY(1,i)) XMIN=XY(1,i)
        if (XMAX.le.XY(1,i)) XMAX=XY(1,i)
        if (YMIN.ge.XY(2,i)) YMIN=XY(2,i)
        if (YMAX.le.XY(2,i)) YMAX=XY(2,i)
      enddo
           
      do i=1,nnode     
        if (nlab(i).eq.ldx .or. nlab(i).eq.lfdx .or.
     &      nlab(i).eq.lpecdx .or. nlab(i).eq.lpmcdx) then
          flag = .FALSE.   
          do j=1,nnode
             if ((nlab(j).eq.lsx .or. nlab(j).eq.lfsx .or.
     &            nlab(j).eq.lpecsx .or. nlab(j).eq.lpmcsx) .and.
     &          abs(abs(XY(1,i)-XY(1,j))-(XMAX-XMIN)).lt.xtoll .and.
     &          abs(XY(2,i)-XY(2,j)).lt.ytoll) then
              Neq(i)=j
              Neq(j)=j 
              flag = .TRUE.
            endif
          enddo
          if(.not. FLAG) then    
            write (*,*) '***ERROR*** Orphan Periodic boundary node'
            stop
          endif
        endif
      enddo
      
      return
      end

C  =====================================================================
C
      subroutine FloquetB (MAXNODE,XY,NLAB,nnode,MAXNCEI,NNCYL,NCYL,
     &                     lfdx,lfloq,lfsx,EPSM,y)
C
C  =====================================================================
C     Finds the correspondence for nodes on the lateral boundary
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real    xy(2,MAXNODE),y,EPSM
      integer MAXNODE,MAXNCEI,nnode,nlab(MAXNODE),lfdx,lfloq,lfsx

C     [OUT]
      integer NNCYL(MAXNCEI),NCYL
      
C     [LOCAL]
      integer i,j,iaux
      real    ytoll
      
      ytoll = EPSM
      ncyl = 0
      do i=1,nnode
        if ((nlab(i).eq.lfloq.or.nlab(i).eq.lfdx.or.nlab(i).eq.lfsx)
     &      .and.(abs(xy(2,i)-y).lt.ytoll)) then
          ncyl = ncyl + 1
          nncyl(ncyl) = i
        endif
      enddo
      
C     SORTING
      if (ncyl.ne.0) then
       do i=1,ncyl
         do j=i+1,ncyl
           if (XY(1,nncyl(j)).lt.XY(1,nncyl(i))) then
             iaux     = nncyl(i)
             nncyl(i) = nncyl(j)
             nncyl(j) = iaux
           endif
         enddo
       enddo
       ncyl = ncyl - 1
      else
       write (*,*) 'none node whit Floquet''s label on boundary at y=',y
      endif
      
      return
      end

C  =====================================================================
C
      subroutine SetToZero (MAXARM,MAXNCEI,MAXBAND,MAXNODE,
     &                      A,B,C,D,I1,I2,V1,V2)
C
C  =====================================================================
C     Finds the correspondence for nodes on the lateral boundary
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer MAXARM,MAXNCEI,MAXBAND,MAXNODE
      
C     [OUT]
      complex A(2*MAXARM+2,2*MAXARM+2), B(2*MAXARM+2,2*MAXNCEI)
      complex C(2*MAXNCEI,2*MAXARM+2), D(3*MAXBAND+1,MAXNODE) 
      complex I1(2*MAXARM+2), I2(2*MAXNCEI) 
      complex V1(2*MAXARM+2), V2(MAXNODE)           
      
C     [LOCAL]
      integer i,j
      
      do i=1,2*maxarm+2                        
        do j=1,2*maxarm+2 
          a(i,j) = (0.,0.)
        enddo
      enddo
      do i=1,2*maxarm+2                  
        do j=1,2*maxncei
          b(i,j) = (0.,0.)
        enddo
      enddo
      do i=1,2*maxncei                  
        do j=1,2*maxarm+2
          c(i,j) = (0.,0.)
        enddo
      enddo
      do i=1,3*maxband+1
        do j=1,maxnode
          d(i,j) = (0.,0.)
        enddo
      enddo
      do i=1,2*Maxarm+2
        i1(i) = (0.,0.)
        v1(i) = (0.,0.)
      enddo
      do i=1,2*maxncei
        i2(i) = (0.,0.)
      enddo
      do i=1,maxnode
        v2(i) = (0.,0.)
      enddo
      
       
      return
      end


C  =====================================================================
C
      subroutine ELENBED (MAXELE,MAXBAND,MAXNODE,MAXDIEL,ele,D,nlab,neq,
     &                    Eps,Mu,Coeff,k0,SE,TE,ie,ielapp,
     &                    nnode,polarization,ku,kl,kd,exp_jkdcos,
     &                    lpecdx,lpec,lpecsx,lpmcdx,lpmc,lpmcsx)
C
C  =====================================================================
C     Finds the correspondence for nodes on the lateral boundary
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      implicit none
      integer MAXELE,MAXDIEL,MAXBAND,MAXNODE
      integer ele(0:8,MAXELE)
      integer nlab(MAXNODE),Neq(MAXNODE),ie,ielapp,nnode,ku,kl,kd
      integer lpecdx,lpec,lpecsx,lpmcdx,lpmc,lpmcsx
      real    SE(8,8),TE(8,8),Eps(MAXDIEL)
      real    Mu(MAXDIEL),Coeff(MAXDIEL),K0
      character*1 polarization
      complex exp_jkdcos
      
C     [OUT]
      complex D(3*MAXBAND+1,MAXNODE) 
      
C     [LOCAL]
      integer i,j,k,iauxj,iauxk
      
C     [FUNCTIONS]
      complex cbget
          
      do i=1,ele(0,ie)
        do j=1,ele(0,ie)
          se(i,j) = - se(i,j) +
     &              k0*k0*(Eps(ielapp)*Mu(ielapp))*TE(i,j)
        enddo
      enddo
         
      do j=1,ele(0,ie)
        iauxj = ele(j,ie)
        if (polarization.eq.'E'.and.nlab(iauxj).ne.LPEC.and.
     &      nlab(iauxj).ne.LPECDx.and.nlab(iauxj).ne.LPECSx
     &      .or.
     &      polarization.eq.'H'.and.nlab(iauxj).ne.LPMC.and.
     &      nlab(iauxj).ne.LPMCDx.and.nlab(iauxj).ne.LPMCSx) then
          do k=1,ele(0,ie)
            iauxk = ele(k,ie)
C     ---------- Non border node                
            if (Neq(iauxj).eq.iauxj) then
              call CBPUT(
     &             CBGET(D,iauxj,iauxk,ku,kl,kd,3*MAXBAND+1,NNODE,
     &             'U')+(1.,0.)*SE(j,k)*Coeff(ielapp),D,iauxj,iauxk,
     &             ku,kl,kd,3*MAXBAND+1,NNODE,'U')
                  
            else           
C     ---------- Periodic border node                 
              call CBPUT(
     &             CBGET(D,Neq(iauxj),iauxk,ku,kl,kd,3*MAXBAND+1,
     &             NNODE,'U')+exp_jkdcos*SE(j,k)*Coeff(ielapp),D,
     &             Neq(iauxj),iauxk,ku,kl,kd,3*MAXBAND+1,NNODE,'U')
            endif
          enddo
        else
          call CBPUT((1.,0.),D,iauxj,iauxj,
     &               ku,kl,kd,3*MAXBAND+1,NNODE,'U')
        endif
      enddo
      
      return
      end

      
C  =====================================================================
C
      subroutine BUILDAB  (MAXARMU,MAXNCEI,MAXNODE,NNCYLU,NNCYLD,xy,
     &                    ncylu,Mmaxu,ncyld,Mmaxd,A,B,K0,phi,Dp,
     &                    expjkdcos,Pi,ymax,ymin,narmu,MAXARM,eleflag)
C
C  =====================================================================
C     Computes Matrices A and B of Floquet expansion
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      implicit none
      integer MAXARMU,MAXNCEI,MAXNODE,NNCYLU(MAXNCEI),NNCYLD(MAXNCEI)
      integer Mmaxu,ncylu,MAXARM,Mmaxd,ncyld,narmu
      real    xy(2,MAXNODE),K0,phi,Dp,Pi,ymax,ymin
      complex expjkdcos
      integer eleflag
      
C     [OUT]
      complex A(2*MAXARM+2,2*MAXARM+2), B(2*MAXARM+2,2*MAXNCEI)
      
C     [LOCAL]
      integer i,n,ip,id
      complex int1,int2,int3
      complex auxc
      real    lenseg

C     [FUNCTIONS]
      complex Pjn_expa
      
C     ---------- Matrix A      
      do n=-Mmaxu,Mmaxu
        a(n+Mmaxu+1,n+Mmaxu+1) = Dp 
      enddo
      do n=-Mmaxd,Mmaxd
        a(narmu+Mmaxd+1+n,narmu+Mmaxd+1+n) = Dp 
      enddo
      
C     ---------- Matrix B
      if (eleflag.eq.3.or.eleflag.eq.4) then
        do n=-Mmaxu,Mmaxu
          auxc = (0., 1.) * (2.*n*pi/Dp-k0*cos(phi))
          do i=1,ncylu       
                         
            lenseg = xy(1,nncylu(i+1))-xy(1,nncylu(i))
            int1 = Pjn_expa(2,1,auxc*lenseg)*lenseg
            int3 = Pjn_expa(2,2,auxc*lenseg)*lenseg
                      
            b(n+Mmaxu+1,i) = b(n+Mmaxu+1,i)
     &                    - exp(auxc * xy(1,nncylu(i))) * int1
            if(i.lt.ncylu) then
              b(n+Mmaxu+1,i+1) = b(n+Mmaxu+1,i+1)
     &                    - exp(auxc * xy(1,nncylu(i))) * int3
            else
              b(n+Mmaxu+1,1) = b(n+Mmaxu+1,1)
     &                    - exp(auxc * xy(1,nncylu(i))) * int3
     &                    * expjkdcos 
            endif
          enddo
          do i=1,ncyld       
                         
            lenseg = xy(1,nncyld(i+1))-xy(1,nncyld(i))
            int1 = Pjn_expa(2,1,auxc*lenseg)*lenseg
            int3 = Pjn_expa(2,2,auxc*lenseg)*lenseg
                      
            b(n+Mmaxu+1+narmu,i+ncylu) = b(n+Mmaxu+1+narmu,i+ncylu)
     &                    - exp(auxc * xy(1,nncylu(i))) * int1
            if(i.lt.ncyld) then
              b(n+Mmaxu+1+narmu,i+ncylu+1)=b(n+Mmaxu+1+narmu,i+ncylu+1)
     &                    - exp(auxc * xy(1,nncyld(i))) * int3
            else
              b(n+Mmaxu+1+narmu,ncylu+1)=b(n+Mmaxu+1+narmu,ncylu+1)
     &                    - exp(auxc * xy(1,nncyld(i))) * int3
     &                    * expjkdcos 
            endif
          enddo
        enddo

      elseif (eleflag.eq.6.or.eleflag.eq.8) then

        do n=-Mmaxu,Mmaxu
          auxc = (0., 1.) * (2.*n*pi/Dp-k0*cos(phi))
          do i=1,ncylu/2
            ip=2*i
            id=ip-1   
            lenseg = xy(1,nncylu(id+2))-xy(1,nncylu(id))
            int1 = Pjn_expa(3,1,auxc*lenseg)*lenseg
            int2 = Pjn_expa(3,2,auxc*lenseg)*lenseg
            int3 = Pjn_expa(3,3,auxc*lenseg)*lenseg                             
            b(n+Mmaxu+1,id) = b(n+Mmaxu+1,id)
     &                    - exp(auxc * xy(1,nncylu(id))) * int1
     
            b(n+Mmaxu+1,ip) = 
     &                    - exp(auxc * xy(1,nncylu(id))) * int2
     
            if(ip.lt.ncylu) then
              b(n+Mmaxu+1,id+2) = b(n+Mmaxu+1,id+2)
     &                    - exp(auxc * xy(1,nncylu(id))) * int3
            else
              b(n+Mmaxu+1,1) = b(n+Mmaxu+1,1)
     &              - exp(auxc * xy(1,nncylu(ncylu-1))) * int3
     &              * expjkdcos 
            endif
          enddo
          
          do i=1,ncyld/2
            ip=2*i
            id=ip-1   
            lenseg = xy(1,nncyld(id+2))-xy(1,nncyld(id))
            int1 = Pjn_expa(3,1,auxc*lenseg)*lenseg
            int2 = Pjn_expa(3,2,auxc*lenseg)*lenseg
            int3 = Pjn_expa(3,3,auxc*lenseg)*lenseg                 
            b(narmu+Mmaxd+1+n,ncylu+id) = b(narmu+Mmaxd+1+n,ncylu+id)
     &                    - exp(auxc * xy(1,nncyld(id))) * int1
     
            b(narmu+Mmaxd+1+n,ncylu+ip) = 
     &                   - exp(auxc * xy(1,nncyld(id))) * int2
     
            if(ip.lt.ncylu) then
              b(narmu+Mmaxd+1+n,ncylu+id+2) = 
     &                    b(narmu+Mmaxd+1+n,ncylu+id+2)
     &                    - exp(auxc * xy(1,nncyld(id))) * int3
            else
              b(narmu+Mmaxd+1+n,ncylu+1) = b(narmu+Mmaxd+1+n,ncylu+1)
     &              - exp(auxc * xy(1,nncyld(ncyld-1))) * int3
     &              * expjkdcos 
            endif
        
          enddo
        enddo

      else      
        write (*,*)'Elements must have numbers 3,4,6 or 8'
      endif

      return
      end
      
C  =====================================================================
C
      subroutine BUILDC (MAXARMU,MAXNCEI,MAXNODE,NNCYLU,NNCYLD,xy,ncylu,
     &                   ncyld,Mmaxu,Mmaxd,Narmu,C,K0,phi,Dp,exp_jkdcos,
     &                   Pi,lensegu,lensegd,ymax,ymin,MAXARM,eleflag)
C
C  =====================================================================
C     Computes matrix C
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      implicit none
      integer MAXARMU,MAXNCEI,MAXNODE,NNCYLU(MAXNCEI),NNCYLD(MAXNCEI)
      integer ncylu,ncyld,Mmaxu,mmaxd,narmu,MAXARM
      real    xy(2,MAXNODE),K0,phi,Dp,Pi
      real    lensegu,lensegd,ymax,ymin
      complex exp_jkdcos
      integer eleflag
      
C     [OUT]
      complex C(2*MAXNCEI,2*MAXARM+2) 
      
C     [LOCAL]
      integer i,m,ip,id
      complex expjGz(-MAXARMU:MAXARMU)   !Harmonics
      complex auxd,auxc,auxc0,auxc1,auxc2,auxc3

C     [FUNCTIONS]
      complex GAMMAm
      complex Pjn_expa
      complex PSIm
      
      if (eleflag.eq.3.or.eleflag.eq.4) then
      
      do m=-Mmaxu,Mmaxu
        auxd  = GAMMAm(m,phi,k0,Dp)
        expjgz(m) = exp((0.,-1.)*auxd*ymax)
        auxc0 = -(0.,1.) * auxd 
        auxc  = -(0.,1.) * (2.*m*pi/Dp-k0*cos(phi))
        auxc1 = auxc0 * Pjn_expa(2,1,auxc*lensegu)*lensegu  
        auxc3 = auxc0 * Pjn_expa(2,2,auxc*lensegu)*lensegu 
        c(1,Mmaxu+1+m) = ( PSIm(xy(1,nncylu(1)),m,phi,k0,Dp) * auxc1 +
     &                    PSIm(xy(1,nncylu(ncylu)),m,phi,k0,Dp) *
     &                    auxc3 * exp_jkdcos )
        do i=2,ncylu
          c(i,Mmaxu+1+m) = PSIm(xy(1,nncylu(i)),m,phi,k0,Dp) * auxc1 + 
     &                     PSIm(xy(1,nncylu(i-1)),m,phi,k0,Dp) * auxc3

        enddo
      enddo
      
      elseif (eleflag.eq.6.or.eleflag.eq.8) then
      
      do m=-Mmaxu,Mmaxu
        auxd  = GAMMAm(m,phi,k0,Dp)
        expjgz(m) = exp((0.,-1.)*auxd*ymax)
        auxc0 = -(0.,1.) * auxd  
        auxc  = -(0.,1.) * (2.*m*pi/Dp-k0*cos(phi))
        auxc1 = auxc0 * Pjn_expa(3,1,auxc*lensegu)*lensegu  
        auxc2 = auxc0 * Pjn_expa(3,2,auxc*lensegu)*lensegu  
        auxc3 = auxc0 * Pjn_expa(3,3,auxc*lensegu)*lensegu  
        c(1,Mmaxu+1+m) = ( PSIm(xy(1,nncylu(1)),m,phi,k0,Dp) * auxc1 +
     &                    PSIm(xy(1,nncylu(ncylu-1)),m,phi,k0,Dp) *
     &                    auxc3 * exp_jkdcos )
        c(2,Mmaxu+1+m) = PSIm(xy(1,nncylu(1)),m,phi,k0,Dp) * auxc2
       do i=2,ncylu/2
         ip=2*i
         id=ip-1
          c(id,Mmaxu+1+m) = PSIm(xy(1,nncylu(id)),m,phi,k0,Dp) * auxc1 +
     &                      PSIm(xy(1,nncylu(id-2)),m,phi,k0,Dp) * auxc3
          c(ip,Mmaxu+1+m) = PSIm(xy(1,nncylu(id)),m,phi,k0,Dp) * auxc2
        enddo
      enddo
      
      else
      endif

C     ---------- Lower boundary
      if (eleflag.eq.3.or.eleflag.eq.4) then
      
      do m=-Mmaxd,Mmaxd
        auxd  = GAMMAm(m,phi,k0,Dp)
        expjgz(m) = exp((0.,1.)*auxd*ymin) 
        auxc0 = -(0.,1.) * auxd    
        auxc  = -(0.,1.) * (2.*m*pi/Dp-k0*cos(phi))
        auxc1 = auxc0 * Pjn_expa(2,1,auxc*lensegd)*lensegd
        auxc3 = auxc0 * Pjn_expa(2,2,auxc*lensegd)*lensegd 
        c(ncylu+1,narmu+Mmaxd+1+m) = ( PSIm(xy(1,nncyld(1)),m,phi,k0,Dp) 
     &             * auxc1 + PSIm(xy(1,nncyld(ncyld+1)),m,phi,k0,Dp)
     &             * auxc3 * exp_jkdcos )
        do i=2,ncyld
          c(ncylu+i,narmu+Mmaxd+1+m) = PSIm(xy(1,nncyld(i)),m,phi,k0,Dp) 
     &             * auxc1 + PSIm(xy(1,nncyld(i-1)),m,phi,k0,Dp) * auxc3
        enddo 
      enddo
      
      elseif (eleflag.eq.6.or.eleflag.eq.8) then
      
      do m=-Mmaxd,Mmaxd
        auxd  = GAMMAm(m,phi,k0,Dp)
        expjgz(m) = exp((0.,1.)*auxd*ymin) 
        auxc0 = -(0.,1.) * auxd   
        auxc  = -(0.,1.) * (2.*m*pi/Dp-k0*cos(phi))
        auxc1 = auxc0 * Pjn_expa(3,1,auxc*lensegd)*lensegd 
        auxc2 = auxc0 * Pjn_expa(3,2,auxc*lensegd)*lensegd 
        auxc3 = auxc0 * Pjn_expa(3,3,auxc*lensegd)*lensegd 
        c(ncylu+1,narmu+Mmaxd+1+m) = ( PSIm(xy(1,nncyld(1)),m,phi,k0,Dp) 
     &             * auxc1 + PSIm(xy(1,nncyld(ncyld+1)),m,phi,k0,Dp)
     &             * auxc3 * exp_jkdcos )
        c(ncylu+2,narmu+Mmaxd+1+m) = PSIm(xy(1,nncyld(2)),m,phi,k0,Dp) 
     &             * auxc2
     
        do i=2,ncyld/2
         ip=2*i
         id=ip-1
         c(ncylu+id,narmu+Mmaxd+1+m) = 
     &         PSIm(xy(1,nncyld(id)),m,phi,k0,Dp)   * auxc1 +
     &         PSIm(xy(1,nncyld(id-2)),m,phi,k0,Dp) * auxc3 
         c(ncylu+ip,narmu+Mmaxd+1+m) = 
     &         PSIm(xy(1,nncyld(ip)),m,phi,k0,Dp)  * auxc2
        enddo 
      enddo
      else
      endif
       
      return
      end                 

C  =====================================================================
C
      subroutine BUILDI2  (MAXNCEI,MAXNODE,NNCYLU,xy,ncylu,
     &            I2,K0,phi,exp_jkdcos,lensegu,ymax,ncyld,ncyl,eleflag)
C
C  =====================================================================
C     Finds the correspondence for nodes on the lateral boundary
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      implicit none
      integer MAXNCEI,MAXNODE,ncylu,NNCYLU(MAXNCEI),ncyld,ncyl
      real    xy(2,MAXNODE),K0,phi,lensegu,ymax
      complex exp_jkdcos
      integer eleflag
      
C     [OUT]
      complex I2(2*MAXNCEI)
      
C     [LOCAL]
      integer i,ip,id
      complex auxc,auxc0,auxc1,auxc2,auxc3,auxd

C     [FUNCTIONS]
      complex Pjn_expa
      complex Ei
      
      if (eleflag.eq.3.or.eleflag.eq.4) then
             
      auxd    = exp ((0.,1.)*k0*ymax*sin(phi))      
      auxc0   = (0.,1.)*k0*sin(phi)*auxd
      auxc    = (0.,1.)*k0*cos(phi)
      auxc1   = auxc0 * Pjn_expa(2,1,auxc*lensegu)*lensegu
      auxc3   = auxc0 * Pjn_expa(2,2,auxc*lensegu)*lensegu
           
      i2(1) = - ( Ei(xy(1,nncylu(1)),0.,phi,k0) * auxc1 +
     &            Ei(xy(1,nncylu(ncylu)),0.,phi,k0) * auxc3
     &            * exp_jkdcos )

      do i=2,ncylu
        i2(i) = - Ei(xy(1,nncylu(i)),0.,phi,k0) * auxc1
     &          - Ei(xy(1,nncylu(i-1)),0.,phi,k0) * auxc3   
      enddo
      
      do i=ncylu+1,ncyl
        i2(i) = 0
      enddo
      
      elseif (eleflag.eq.8.or.eleflag.eq.6) then
      
      auxd    = exp ((0.,1.)*k0*ymax*sin(phi))      
      auxc0   = (0.,1.)*k0*sin(phi)*auxd
      auxc    = (0.,1.)*k0*cos(phi)
      auxc1   = auxc0 * Pjn_expa(3,1,auxc*lensegu)*lensegu    
      auxc2   = auxc0 * Pjn_expa(3,2,auxc*lensegu)*lensegu 
      auxc3   = auxc0 * Pjn_expa(3,3,auxc*lensegu)*lensegu       
           
      i2(1) = - ( Ei(xy(1,nncylu(1)),0.,phi,k0) * auxc1 +
     &            Ei(xy(1,nncylu(ncylu-1)),0.,phi,k0) * auxc3
     &            * exp_jkdcos )
      i2(2) = - ( Ei(xy(1,nncylu(1)),0.,phi,k0) * auxc2 ) 
     
      do i=2,ncylu/2
        ip=2*i
        id=ip-1
        i2(id) = - ( Ei(xy(1,nncylu(id)),0.,phi,k0) * auxc1 +
     &               Ei(xy(1,nncylu(id-2)),0.,phi,k0) * auxc3)
        i2(ip) = - ( Ei(xy(1,nncylu(id)),0.,phi,k0) * auxc2 )
      enddo
      
      do i=ncylu+1,ncyl
        i2(i) = (0.,0.)
      enddo
      
      else
      endif
      
      return
      end
           
      
C  =====================================================================
C
      complex function Ei(x,z,phi,k)
C
C  =====================================================================
C     TM Plane wave field in (x,z) incident from phi, wavenumber k
c     Ei(x,z) = exp[+j*k*(x*cos(phi)+z*sin(phi))]
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real x,z,phi,k                             
      
      Ei = exp((0.,1.)*k*(x*cos(phi)+z*sin(phi)))
      return
      end

C  =====================================================================
C
      complex function Hi(x,z,phi,k,zeta)
C
C  =====================================================================
C     TE Plane wave field in (x,z) incident from phi, wavenumber k
c     Hi(x,z) = (1/zeta) (-x*sin(phi)+z*cos(phi)) * 
c               * exp[+j*k*(x*cos(phi)+z*sin(phi))]
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real x,z,phi,k,zeta                             
      
      Hi = (1/zeta) * (-x*sin(phi)+z*cos(phi)) * 
     &      exp((0.,1.)*k*(x*cos(phi)+z*sin(phi)))
      return
      end

C  =====================================================================
C
      complex function PSIm(x,m,phi,k,d)
C
C  =====================================================================
C     Floquet PSI function  
c     PSIm(x) = exp{-j*[2*m*pi/d-k*cos(phi)]*x}
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real    x,phi,k,d
      integer m                          
C     [LOCAL]
      real    pi      
      parameter (pi=3.141592653589793238)
      
      PSIm = exp(-(0.,1.)*(2*m*pi/d-k*cos(phi))*x)
      return
      end

C  =====================================================================
C
      complex function GAMMAm(m,phi,k,d)
C
C  =====================================================================
C     Floquet GAMMA function  
C     GAMMAm = sqrt{k**2-(2*pi*m/d-k*cos(phi))**2}
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real    phi,k,d
      integer m                          
C     [LOCAL]
      real    pi      
      parameter (pi=3.141592653589793238)
      real    G2
      G2 = k**2-(2*pi*m/d-k*cos(phi))**2
      if (G2.ge.0.) then
        GAMMAm = cmplx(sqrt(G2),0.)
      else
        GAMMAm = cmplx(0.,-sqrt(-G2))
      endif  
      return
      end

C  =====================================================================
C
      complex function ZETAm(m,phi,k,zeta,d)
C
C  =====================================================================
C     Floquet ZETA function  
c     ZETAm = GAMMAm/k*zita
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real    phi,k,zeta,d
      integer m                          
C     [FUNCTION]
      complex GAMMAm
      
      ZETAm = GAMMAm(m,phi,k,d)/k*zeta
      return
      end
