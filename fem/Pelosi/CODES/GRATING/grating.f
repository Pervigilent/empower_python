C  =====================================================================
C
      program GRATING
C
C  =====================================================================
C     2-D scattering from periodic surfaces. Soft case.
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      
      implicit none
      
      integer MAXARMU,MAXARMD,MAXARM,MAXNCEI,MAXNODE,MAXELE,
     &        MAXNXE,MAXBAND,MAXDIEL,NDMAX,KU,KL,KD
      real    EPSM
					       
      parameter (
     &  MAXARMU      = 20,       !Maximum number of Floquet harmonics 
     &                           !on top of the boundary
     &  MAXARMD      = 20,       !Maximum number of Floquet harmonics 
     &                           !on bottom of the boundary
     &  MAXARM = MAXARMU + MAXARMD,  !Sum between maximum numbers of
     &                           !Floquet harmonics on top and 
     &                           !bottom of the boundary
     &  MAXNCEI      = 31,       !Maximum number of nodes on top and bottom 
     &                           !of the boundary    
     &  MAXNODE      = 1700,     !Maximum number of nodes
     &  MAXELE       = 3000,     !Maximum number of elements
     &  MAXNXE       = 8,        !Maximum number of nodes per element
     &  MAXBAND      = 180,      !Maximum band
     &  MAXDIEL      = 2,        !Maximum number of different dielectrics
     &  NDMAX        = 190,      !Maximum frequency steps
     &  EPSM         = 1.E-04,   !Tolerance
     &  KU           = 180,      !Upper band
     &  KL           = 180,      !Lower band
     &  KD           = 0)        !Nonsymmetric band
      real PI
      parameter (pi=3.141592653589793238)

      complex cj
      real    k0
      
      parameter (     
     &   cj      = (0.,1.),
     &   k0      = 2 * pi)
      
C     FEM Mesh and related variables
      integer ele(0:8,MAXELE)    !Connectiviti matrix
      real    xy(2,MAXNODE)      !Node co-ordinates
      integer nlab(MAXNODE)      !Node labels
      integer elab(MAXELE)       !Element labels
      integer nnode              !Node number
      integer nele               !Element number
				       
      integer nncylu(MAXNCEI)    !Nodes on floquet upper boundary
      integer ncylu              !Number of nodes on floquet upper boundary
      integer nncyld(MAXNCEI)    !Nodes on floquet lower boundary
      integer ncyld              !Number of nodes on floquet lower boundary
      integer nncyl(2*MAXNCEI)   !Nodes on floquet boundary
      integer ncyl               !Total number of nodes on floquet boundary
      integer Neq(MAXNODE)       !Node equivalence for periodicity
      integer LFSx,LFloq,LFDx    !Labels for Floquet nodes
      integer Ldx,Lsx            !Labels for periodic conditions
      integer LPECSx,LPEC,LPECDx !Labels for PEC
      integer LPMCSx,LPMC,LPMCDx !Labels for PMC
      real    xmin,xmax,ymin,ymax!Mesh limits   
      real    ymed               !Average between ymax and ymin  
      real    xmed               !Average between xmax and xmin                                                 
      
      integer Ndie               !Number of dielectrics
      real    Eps(MAXDIEL)       !Permittivities
      real    Mu(MAXDIEL)        !Permeabilities
      integer Material(MAXDIEL)  !Material labels
      
      real    Dp_1,Dp_2,Dp_s     !Scaling for frequency sweep
      real    Dps,Dp_old         !Scaling parameters
      real    Dp                 !Period
      real    LAMBDA(0:NDMAX)    !Frequency vector
      real    TRAS(0:NDMAX)      !Power transmitted vector
      real    COFTRAS(0:NDMAX)   !Transmitted coefficient vector
      real    LOGTRAS(0:NDMAX)   !Power transmitted vector, in DB  
      
C     Incidence data
      real    phi                !Incidence angle
      character*1 polarization   !Polarization 'e' or 'h'
      integer int_field          !Flag for the computation of interior field
				 !0 = No, 1 = Yes
      integer N_div              !Frequency divisions
      integer k1                 !Frequency step
	    
C     Solving system variables
      real    Coeff(MAXDIEL)           !Coefficients
      complex A(2*MAXARM+2,2*MAXARM+2) !matrice cond. Dirichlet x Alp
      complex B(2*MAXARM+2,2*MAXNCEI)  !matrice cond. Dirichlet x nodi
      complex C(2*MAXNCEI,2*MAXARM+2)  !matrice cond. Neumann x Alp
      complex D(3*MAXBAND+1,MAXNODE)   !FEM + Neumann matrix
      complex I1(2*MAXARM+2)           !Right hand side Dirichlet
      complex I2(2*MAXNCEI)            !Right hand side Neumann
      complex V1(2*MAXARM+2)           !Left hand side Dirichlet     
      complex V2(MAXNODE)              !Left hand side Neumann

      real    SE(8,8),TE(8,8)          !Element matrices
      
      integer ie
      real    alpum(NDMAX),alpuf(NDMAX) !Scattering coefficients
      real    alpdm(NDMAX),alpdf(NDMAX) !Transmitting coefficients
      
C     Floquet variables on top of the boundary
      integer Mmaxu               !Maximum number of harmonics      
      integer Narmu               !Total number of harmonics
      
C     Floquet variables on bottom of the boundary
      integer Mmaxd               !Maximum number of harmonics      
      integer Narmd               !Total number of harmonics
      
      integer Narm                !Sum of harmonics between lower
     &                            !and upper boundary      
      
C     File names and auxiliary      
      character*256 fgeoname      !Geometry file
      character*256 felename      !Electromagnetic file
      integer       i,j,k         !Some indexes   
      integer       iband         !Bandwidth extimation
      real          aux
      real          lensegu,lensegd
      complex       expjkdcos    
      complex       exp_jkdcos
      complex       auxr,auxt
      integer       ielapp
      character*3   cnum
      integer       eleflag
      
C     Functions
      complex       GammaM    

c     Reading Mesh file     
      
      WRITE (*,*) '+--- Mesh File Name              '
      READ (*,*) fgeoname
      WRITE (*,*) '+--- Reading Mesh File           '
      
      call ReadNodalMesh  (fgeoname,MAXNODE,MAXNXE,MAXELE,
     &                     XY,ELE,NLAB,ELAB,NNODE,NELE)
      
c     Reading Electromagnetic File

      WRITE(*,*) '+--- Electromagnetic File Name              '
      READ (*,*) felename
      WRITE(*,*) '+--- Reading Electromagnetic File           '

c      ---------------------- Parametri in ingresso ------------------------
      open (unit=1,status='old',
     &      form='formatted',file=felename)
      read(1,*) LFSx, LFloq, LFDx     ! Label Floquet Boundary
      read(1,*) LSx, LDx              ! Label Dx,Sx Boundary
      read(1,*) LPECSx, LPEC, LPECDx  ! Label PEC Boundary
      read(1,*) LPMCSx, LPMC, LPMCDx  ! Label PMC Boundary
      
      read(1,*) Mmaxu                 !Number of harmonics on upper boundary
      read(1,*) Mmaxd                 !Number of harmonics on lower boundary
      read(1,*) Ndie                  !Maximum number of dielectrics
      read(1,*)(Eps(i),i=1,Ndie)      !Relative dielectric constant
      read(1,*)(Mu(i),i=1,Ndie)       !Relative magnetic permeabilitie  
      read(1,*)(Material(i),i=1,Ndie) !Vector labels of materials
      read(1,*) phi                   !Incidence angle of plain wave (deg)
      read(1,*) Dp_1,Dp_2,Dp_s        !Frequency initial, final, step
      
      read(1,'(a1)') polarization     !Polarization incidence wave (E/H)
      read(1,*) int_field             !Computes interior field too
      close(1)

C     Builds the Neq vector of node equivalences
      call Periodicity (MAXNODE,XY,NLAB,nnode,EPSM,NEQ,
     &                  xmin,xmax,ymin,ymax,  
     &                  lfdx,ldx,lpecdx,lpmcdx,
     &                  lfsx,lsx,lpecsx,lpmcsx)

C     Builds the NNcylu vector of nodes on floquet upper boundary
      call FloquetB (MAXNODE,XY,NLAB,nnode,MAXNCEI,NNCYLU,NCYLU,
     &                  lfdx,lfloq,lfsx,EPSM,ymax) 
     
C     Builds the NNcyld vector of nodes on floquet lower boundary
      call FloquetB (MAXNODE,XY,NLAB,nnode,MAXNCEI,NNCYLD,NCYLD,
     &                  lfdx,lfloq,lfsx,EPSM,ymin)

C     Costruzione del vettore NNCYl(ncyl)
        ncyl=ncylu+ncyld
        do i=1,ncylu
          nncyl(i)=nncylu(i)
        enddo 
        do i=1,ncyld   
          nncyl(ncylu+i)=nncyld(i)
        enddo 
         
          
C     X and Y axis centering
      xmed = (XMAX+XMIN)/2.
      ymed = (YMAX+YMIN)/2.
      do i=1,nnode
	 xy(1,i) = xy(1,i) - xmed
	 xy(2,i) = xy(2,i) - ymed
      enddo
      XMAX = XMAX - xmed
      XMIN = XMIN - xmed
      YMAX = YMAX - ymed
      YMIN = YMIN - ymed
      
      
C     Band Extimation with periodicity
      write (*,*) '+---  FEM matrix Band Extimation'
      iband = 0
      do ie=1,nele
	do j=1,3
	  do k=1,3
	    if (abs(Neq(ele(j,ie))-ele(k,ie)).gt.iband)
     &         iband = abs(Neq(ele(j,ie))-ele(k,ie))
	  enddo
	enddo
C	write (*,*) "ELE = ",ie,";  band = ",iband
      enddo
      write (*,*) '|         Band of FEM : ',iband
      
C     Armonics
      Narmu = 2*Mmaxu+1
      Narmd = 2*Mmaxd+1
      
C     Check matrix dimensions
      if (iband.gt.MAXBAND) then
	write(*,*) '***ERROR*** FEM matrix too large'
c	stop 
      endif
	
      if (Narmu.gt.2*MAXARMU+1) then
	write(*,*) '***ERROR*** Too many harmonics in the Floquet
     &              expansion on top of the boundary'
	stop
      endif
	
      if (Narmd.gt.2*MAXARMD+1) then
	write(*,*) '***ERROR*** Too many harmonics in the Floquet
     &              expansion on bottom of the boundary'
	stop
      endif
	
      if (ncylu.gt.MAXNCEI) then
	write(*,*) '***ERROR*** Too many nodes on top of the boundary'
	stop
      endif
      
      if (ncyld.gt.MAXNCEI) then
	write(*,*) '***ERROR*** Too many nodes on bottom of the boundary'
	stop
      endif
	
C     Lesser settings      
      phi = phi * pi/180.
      if (polarization .eq. 'h') polarization = 'H'
      if (polarization .eq. 'e') polarization = 'E'
      if ((polarization.ne. 'E') .and. (polarization.ne. 'H')) then
	write(*,*) '***ERROR*** Unrecognized polarization'
	stop
      endif  
      
      if ( polarization .eq. 'E') then
	do i=1,Ndie
	  Coeff(i) = 1. / ( Mu(i) )
	enddo
      
      else if ( polarization .eq. 'H') then
	do i=1,Ndie
	   Coeff(i) = 1. / ( Eps(i) )
	enddo
      endif
		  
C     Prepare Frequency cycle
      Dps = 1.
      N_div = (Dp_2-Dp_1)/Dp_s + 1.0E-03
      if (N_div.gt.NDMAX) then
	write(*,*) '***ERROR*** Scaling too fine'
	stop
      endif  
      
      do k1=0,N_div
	Dp_old = Dps
	if (N_div.gt.0) then
	  Dps = Dp_1 + (Dp_2-Dp_1) / N_div * k1
	else
	  Dps = Dp_1
	endif

C     Mesh scaling to simulate frequency sweep Scaling
	do i=1,nnode
	  xy(1,i) = xy(1,i) / Dp_old * Dps
	  xy(2,i) = xy(2,i) / Dp_old * Dps
	enddo

	XMAX = XMAX / Dp_old * Dps
	XMIN = XMIN / Dp_old * Dps
	YMAX = YMAX / Dp_old * Dps
	YMIN = YMIN / Dp_old * Dps

	Dp  = 2 * XMAX
      
	exp_jkdcos = exp(-cj * k0 * Dp * cos(phi))
	expjkdcos  = exp( cj * k0 * Dp * cos(phi))

C     writes the data of this cycle
	write(*,*)'-----------------------------'      
	write(*,'(1x ,7h[ in ] ,a, f14.5 )')'Period.     : ',Dp
	write(*,'(1x ,7h[ in ] ,a, f14.5 )')'Dim X box   : ',XMAX*2.
	write(*,'(1x ,7h[ in ] ,a, f15.5 )')'Dim Y box   : ',YMAX*2.
	
C     FEM HYBRID MATRIX GENERATION
	write (*,*) '>-+-- Start'
	write (*,*) '  +-- Initializations'
        call SetToZero (MAXARM,MAXNCEI,MAXBAND,MAXNODE,
     &                  A,B,C,D,I1,I2,V1,V2)
     
       eleflag=ELE(0,1)

c     D Matrix construction
	write (*,*) '  +-- D Matrix construction'
	do ie=1,nele
	  do i=1,Ndie
	    if (Material(i).eq.elab(ie)) ielapp = i
	  enddo
	  
	  call ELENMAT (MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,SE,8,TE,8)  

	  call ELENBED (MAXELE,MAXBAND,MAXNODE,MAXDIEL,ele,D,nlab,neq,
     &                  Eps,Mu,Coeff,k0,SE,TE,ie,ielapp,
     &                  nnode,polarization,ku,kl,kd,exp_jkdcos,
     &                  lpecdx,lpec,lpecsx,lpmcdx,lpmc,lpmcsx)
	enddo


C     Periodicity
	do i=1,nnode
	  if(Neq(i).ne.i) then
	    call CBPUT(exp_jkdcos,D,i,i,ku,kl,kd,3*MAXBAND+1,NNODE,'U')            
	    call CBPUT(-(1.,0.),  D,i,Neq(i),ku,kl,kd,3*MAXBAND+1,NNODE,
     &                'U')            
	  endif
	enddo	
            
C     I1 Vector construction
        i1(Mmaxu+1) = - Dp * exp((0.,1.)*K0*ymax*sin(phi)) 
         
C     UPPER BOUNDARY

        if (ncylu.ne.0) then
          Narm = Narmu
          if (eleflag.eq.3.or.eleflag.eq.4) then
	    lensegu = abs(xy(1,nncylu(1))-xy(1,nncylu(ncylu+1)))
     &      /real(ncylu) 
	  elseif (eleflag.eq.8.or.eleflag.eq.6) then
            lensegu = 2*Dp/real(ncylu)
	  endif
        endif

C    LOWER BOUNDARY 
    
        if (ncyld.ne.0) then
          Narm = Narm + Narmd
          if (eleflag.eq.3.or.eleflag.eq.4) then
	    lensegd = abs(xy(1,nncyld(1))-xy(1,nncyld(ncyld+1)))
     &      /real(ncyld)
	  elseif (eleflag.eq.8.or.eleflag.eq.6) then
	    lensegd = 2*Dp/real(ncyld)
          endif
        endif

        if (ncylu.eq.0.and.ncyld.eq.0) then
         write (*,*) '***ERROR*** Orphane periodic structure'
         stop
        endif

C     A and B Matrix construction 
        write (*,*)'  |   Integral Dirichlet conditions '	                
        call BUILDAB (MAXARMU,MAXNCEI,MAXNODE,NNCYLU,NNCYLD,xy,
     &                ncylu,Mmaxu,ncyld,Mmaxd,A,B,K0,phi,Dp,
     &                expjkdcos,Pi,ymax,ymin,narmu,MAXARM,eleflag)
     
C     I2 Vector construction
	call BUILDI2  (MAXNCEI,MAXNODE,NNCYLU,xy,ncylu,
     &     I2,K0,phi,exp_jkdcos,lensegu,ymax,ncyld,ncyl,eleflag)
     
C     Scattered Field
        write (*,*) '  |   Integral Dirichlet conditions'
	call BUILDC (MAXARMU,MAXNCEI,MAXNODE,NNCYLU,NNCYLD,xy,ncylu,
     &               ncyld,Mmaxu,MMaxd,Narmu,C,K0,phi,Dp,exp_jkdcos,
     &               Pi,lensegu,lensegd,ymax,ymin,MAXARM,eleflag) 
             
c     SOLUTION  
	write (*,*) '>-+-- Solving'
	    
	call BLOCK_SOLVER(A, 2*MAXARM+2, B, 2*MAXNCEI, C, D, 
     &                    3*MAXBAND+1, MAXNODE, kl, ku, I1, I2,
     &                    NNODE, NNCYL, NCYL, NARM,
     &                    V1, V2, int_FIELD)  
	

C     WRITING OF RESULTS
C     Scattering coefficients of upper Floquet harmonics
	write(cnum,'(I3.3)') k1
	write (*,*) '  +-- Scattering coefficients'
	open(unit=1,status='unknown',form='formatted',
     &       file='Scatt'//cnum//'.dat') 
        do i=1,narmu 
          V1(i)=V1(i)*exp((0.,1.)
     &   *(GAMMAm(i+Mmaxu-Narmu,phi,k0,Dp))*ymax)
             
	  write(1,*) i-Mmaxu-1,V1(i)
	
	enddo
	close(1)
	
      if (ncyld.ne.0) then
C     Transmitting coefficients of lower Floquet harmonics
	write(cnum,'(I3.3)') k1
	write (*,*) '  +-- Transmitting coefficients'
	open(unit=1,status='unknown',form='formatted',
     &       file='Trans'//cnum//'.dat')
        do i=1,narmd
         V1(i+narmu)=V1(i+narmu)*exp((0.,-1.)
     &    *(GAMMAm(i+Mmaxd-Narmd,phi,k0,Dp))*ymin)
     
	  write(1,*)  i-Mmaxd-1,V1(i+narmu)
	
	enddo
	close(1)
       endif	

c     Fields
	if (int_field.eq.1) then
	write (*,*) '  +-- Field'
	open(unit=1,status='unknown',form='formatted',
     &       file='Field'//cnum//'.dat')
	write (1,'(a9,I5)') 'Type = ',1
	write (1,'(a9,I5)') 'Values = ',nnode
	do i=1,nnode
	   write(1,*) i,abs(V2(i))
	enddo   
	close(1)
	endif

c     Power Check
	write (*,*) '  +-- Power Check scattered'
	WRITE (*,*) '      |Ro|^2      = ', abs(V1(Narmu/2+1))**2.
	
	auxr = (0.,0.)	
	do i=1,Narmu
	 if (abs(aimag(GAMMAm(i+Mmaxu-Narmu,phi,k0,Dp))).lt.EPSM) then
	  auxr = auxr + abs(V1(i))**2 * GAMMAm(i+Mmaxu-Narmu,phi,k0,Dp)
        endif	    	  
	enddo  
	auxr = auxr / (k0*sin(phi))      
        
	if ( polarization .eq. 'E' ) auxr = conjg ( auxr )

	WRITE (*,*) '      Total scattered power = ',
     &  real(auxr)
	
	if(ncyld.eq.0)then
	  WRITE (*,*) '      Relative power error  = ',
     &                     abs(1-real(auxr))
        endif
     
c       Reflection Coefficient Extimation
	alpum(1+k1)= abs(V1(Narmu/2+1))**2.
	alpuf(1+k1)= atan2d(aimag(V1(Narmu/2+1)),real(V1(Narmu/2+1)))
	aux = atan2d(aimag(V1(Narmu/2+1)),real(V1(Narmu/2+1)))
	if ( polarization .eq. 'E' ) then
	WRITE (*,*) '      |Re|     = ',(abs(V1(Narmu/2+1)))
        WRITE (*,*) '      |<Re|    = ',aux
	else if ( polarization .eq. 'H' ) then 
	WRITE (*,*) '      |Rh|     = ',(abs(V1(Narmu/2+1)))
	WRITE (*,*) '      |<Rh|    = ',aux
	endif
	if (ncyld.eq.0) then
	WRITE (*,*)      '+-- Done ----------------------------' 
	endif
	 
C    Transmission power
      if (ncyld.ne.0) then
        WRITE (*,*) '  +-- Power check transmitted'
        WRITE (*,*) '      |To|^2      = ',abs(V1(Narmu+Narmd/2+1))**2 
       
       auxt = (0.,0.)	
	do i=1,Narmd
	 if (abs(aimag(GAMMAm(i+Mmaxd-Narmd,phi,k0,Dp))).lt.EPSM) then
	  auxt = auxt + abs(V1(narmu+i))**2 
     &    * GAMMAm(i+Mmaxd-Narmd,phi,k0,Dp)
	 else
        endif  	  
	enddo
	auxt = auxt / (k0*sin(phi))       
        
	if ( polarization .eq. 'E' ) auxt = conjg ( auxt )

	WRITE (*,*) '      Total transmitted power = ',
     &  real(auxt)
	WRITE (*,*) '      Relative power error    = ',
     &              abs(1-real(auxt)-real(auxr))

c       Transmission Coefficient Extimation
	alpdm(1+k1)= abs(V1(Narmu+Narmd/2+1))**2.
	alpdf(1+k1)= atan2d(aimag(V1(Narmu+Narmd/2+1)),
     &  real(V1(Narmu+Narmd/2+1)))
	aux = atan2d(aimag(V1(Narmu+Narmd/2+1)),
     &  real(V1(Narmu+Narmd/2+1)))
	if ( polarization .eq. 'E' ) then
	WRITE (*,*) '      |Re|                    = ',
     &  (abs(V1(Narmu+Narmd/2+1)))
	WRITE (*,*) '      |<Re|                   = ',aux
	else if ( polarization .eq. 'H' ) then 
	WRITE (*,*) '      |Rh|                    = ',
     &  (abs(V1(Narmu+Narmd/2+1)))
	WRITE (*,*) '      |<Rh|                   = ',aux
	endif
	WRITE (*,*) '+-- Done ----------------------------' 
      endif
      
      COFTRAS(K1) = log10(abs(V1(Narmu+Narmd/2+1))**2)
      TRAS(K1) = real(auxt)
      LOGTRAS(k1) = log10(real(auxt))
      LAMBDA(K1) = 1./Dp

      enddo
      
        open(unit=1,status='unknown',form='formatted',
     &     file='Reflet.dat')
      do k1=0,N_div
	write (1,*) k1, alpum(k1+1), alpuf(k1+1)
      enddo   
      
      if (ncyld.ne.0) then
       open(unit=1,status='unknown',form='formatted',
     &     file='Transmit.dat')
      do k1=0,N_div
	write (1,*) k1, alpdm(k1+1), alpdf(k1+1)
      enddo   
      endif
      
       open(unit=1,status='unknown',form='formatted',
     &     file='PLOT.dat')
      do k1=0,N_div     
	write (1,*)  LAMBDA(K1), TRAS(K1), LOGTRAS(K1), COFTRAS(K1)  
      enddo
        
      end






