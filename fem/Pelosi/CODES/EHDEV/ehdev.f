C  =====================================================================
C
      PROGRAM Bend
C
C  =====================================================================
C     Rectanglar Waveguide Bends-devices in H or E plane
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      IMPLICIT NONE
      integer MAXNODE,MAXELE,MAXNXE,MAXBAND,MAXDIEL,KU,KL,KD
                                               
      parameter (
     &  MAXNODE      = 1200,     !Maximum number of nodes
     &  MAXELE       = 1200,     !Maximum number of elements
     &  MAXNXE       = 8,        !Maximum number of nodes per element
     &  MAXBAND      = 85,       !Maximum band
     &  MAXDIEL      = 5,        !Maximum number of different dielectrics
     &  KU           = 85,       !Upper band
     &  KL           = 85,       !Lower band
     &  KD           = 0)        !Simmetric band
     
      integer MAXPORTS,MAXNXP,MAXMODE,MAXFREQ
      parameter (
     &  MAXPORTS     =2,         !Maximum number of ports 
     &  MAXNXP       =35,        !Maximum number of nodes per port  
     &  MAXMODE      =80,        !Maximum number of Wg modes
     &  MAXFREQ      =100)       !Maximum number of frequency points
      
      real PI
      parameter (pi = 3.141592653589793238)
      
      real Zeta0,c0,epsm
      parameter (Zeta0 = 120*pi)
      parameter (c0    = 3.E+08)    
      parameter (epsm  = 1.E-08)
      
c     FEM Mesh and related variables
      integer ele(0:MAXNXE,MAXELE)!Connectivity matrix
      real    xy(2,MAXNODE)      !Node co-ordinates
      integer nlab(MAXNODE)      !Node labels
      integer elab(MAXELE)       !Element labels
      integer nnode              !Node number
      integer nele               !Element number
      integer nord               !Element order (1 or 2)
      integer nel                !Elements x port
      
      integer Lpec,Lpmc          !Label of nodes on pec and pmc
      integer Lp(MAXPORTS)       !Label on nodes on prts
      integer Nport(MAXNXP*MAXPORTS)!All the nodes on all ports
      real    scal(2*MAXMODE)    !Scaling factors
      real    a,b                !Waveguide dimensions
      character*1 plane          !Plane 'E' or 'H'

      integer Ndie               !Number of dielectrics
      integer Material(MAXDIEL)  !Material Labels
      real    Eps(MAXDIEL)       !Permittivities
      real    Mu(MAXDIEL)        !Permittivities

      integer PO(0:MAXNXP,MAXPORTS)!Indexes to nodes on ports
      integer Np                 ! Number of ports of the junction
      
C     EM variables
      integer   Nmode            ! Number of modes to be used in expansion
      integer   Nfreq            ! Number of frequncies
      real      Freq(MAXFREQ)    ! Frequencies [Hz]
      real      k0,k02           ! Wavenumber and its square
      complex   epsr             ! Relative Permittivity
      complex   mir              ! Relative Permeability
      
C     Auxiliary
      real      dim              ! a or b, debends on the plane
      integer   i,j,k            ! Loop variables
      integer   iband            ! Bandwidth
      integer   npt              ! Number of ports
      integer   ip               ! Actual port     
      integer   icc,im,nmnp
      real      ft10,ft20        ! First and second cutoff
      real      abp,piabp,pia
      real	dx,integ
      integer   ifl1,ifl2,i1,i2
      complex   aux,aux1
      complex   VKT2(0:MAXDIEL), VCoeff(0:MAXDIEL)
      integer   flgfld
      character*256 fieldfile       
     
C     Solving system variables
      complex     AC (MAXPORTS*MAXMODE,MAXPORTS*MAXMODE)
      complex     BC (MAXPORTS*MAXMODE,MAXPORTS*MAXNXP)
      complex     IC (MAXPORTS*MAXMODE)
      complex     FEM(3*MAXBAND+1,MAXNODE)
      complex     CD (MAXPORTS*MAXNXP,MAXPORTS*MAXMODE)
      complex     ID (MAXPORTS*MAXNXP)

      complex     V1((MAXMODE+1)*MAXPORTS)
      complex     V2(MAXNODE)
      
      complex     RZ(MAXMODE,MAXPORTS)  !Radix of mode impedances
      complex     BI(MAXMODE,MAXPORTS)  !Mode propagation constants
      
      complex     Field(MAXNODE)  
      
      integer     Ipvt(MAXNODE)
      
C     Filennames       
      character*256 fgeoname     !Geometry file
      character*256 felename     !Electromagnetic file
      character*256 foutname     !Output file

C     Functions
      integer nbandex
      complex Pjn_expa
                  
      real dist,x1,x2,y1,y2
      dist(x1,y1,x2,y2) = sqrt( (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) )
      
      
C     STARTUP      
      write (*,*) '+--- Begin '
      write (*,*) '+--- Mesh file NAME ? '
      READ  (*,*) fgeoname
      WRITE (*,*) '+--- Reading Mesh File           '

      call ReadNodalMesh  (fgeoname,MAXNODE,MAXNXE,MAXELE,
     &                     XY,ELE,NLAB,ELAB,NNODE,NELE)

      write (*,*) '+--- Electromagnetic file NAME ? '
      READ  (*,*) felename
      WRITE (*,*) '+--- Reading Electromagnetic File           '

      open (unit=1,status='old',
     &      form='formatted',file=felename)
      read (1,*) np              !Number of ports
      read (1,*) a,b             !Waveguide dimensions (millimeters)
      read (1,*) (Lp(i),i=1,np)  !Port Labels
      read (1,*) lpec,lpmc       !Labels on pec and pmc
      read (1,*) Nmode           !Number of modes
      read (1,*) Plane           !Plane 'E' or 'H'
      read (1,*) Ndie                   !Maximum number of dielectrics
      read (1,*) (Eps(i),i=1,Ndie)      !Relative dielectric constant
      read (1,*) (Mu(i),i=1,Ndie)       !Relative magnetic constants
      read (1,*) (Material(i),i=1,Ndie) !Vector labels of materials
      read (1,*) Nfreq                  !Number of frequency points (GHz)
      read (1,*) (Freq(i),i=1,Nfreq)    !Frequency points
      close (1)  

C     ---------- Rescale everything to MKS
      a = a * 1.E-3
      b = b * 1.E-3
      do i=1,Nfreq
        Freq(i) = Freq(i) * 1.E09
      end do
      
      do i=1,nnode
	xy(1,i) = xy(1,i) * 1.E-3
	xy(2,i) = xy(2,i) * 1.E-3
      enddo	
 
      write (*,*) '+--- Output file NAME ? '
      READ  (*,*) foutname
      
C     VERIFIES BANDWIDTH
      iband = nbandex(MAXELE,MAXNXE,ELE,NELE)
      if (iband.gt.maxband) then
        write(*,*) '*** ERROR *** MAXBAND should be at least to ',iband
        stop
      endif    

C     Checkings      
      if(NDie .gt. 1 .and. (Plane .eq. 'e' .or. Plane .eq. 'E')) then
        WRITE(*,*) '**** Error **** E - plane only HOMOGENEOUS '
        stop
      endif

      if(NNODE .gt. MAXNODE) then
        WRITE(*,*) '**** Error **** TOO MANY NODES '
        stop
      endif

      if(NELE .gt. MAXELE) then
        WRITE(*,*) '**** Error **** TOO MANY ELEMENTS '
        stop
      endif

      if(Np .gt. MAXPORTS) then
        WRITE(*,*) '**** Error **** TOO MANY PORTS '
        stop
      endif

      if(Nmode .gt. MAXMODE) then
        WRITE(*,*) '**** Error **** TOO MANY MODES '
        stop
      endif

      if(Nfreq .gt. MAXFREQ) then
        WRITE(*,*) '**** Error **** TOO MANY FREQUENCY POINTS '
        stop
      endif
      
      if (plane.eq.'e') plane='E'
      if (plane.eq.'h') plane='H'
      if(plane .ne. 'E' .and. plane .ne. 'H') then
        WRITE(*,*) '**** Error **** UNRECOGNIZABLE PLANE '
        stop
      endif

      dim = a
      if (plane.eq.'E') dim=b

C     Verifies cut-off        
      epsr = (1.,0.)
      mir  = (1.,0.)
      ft10 = c0 / (2*a)
      if (b.le.a/2) then
        ft20 = 2. * ft10
      else
        ft20 = c0 / (2*b)
      endif

      ifl1 = 0
      ifl2 = 0
      if (Freq(1).lt.ft10) then
        write(*,*) 'WARNING: Freq(1) is below cut-off'
      endif
      if (Freq(NFreq).gt.ft20) then
        write(*,*) 'WARNING: Freq(NFreq) is above 2nd mode cut-off'
      endif
            
C     Creates the vectors of the nodes on the ports
      call ports(MAXNODE,MAXNXP,NLAB,XY,PO,np,nnode,dim,LP)
            
C     Verifies consistency
      do i = 1,np
        if (abs( dim - 
     &      sqrt((xy(1,PO(1,i))-xy(1,PO(PO(0,i),i)))**2+
     &           (xy(2,PO(1,i))-xy(2,PO(PO(0,i),i)))**2)).gt..0001) then
          write (*,*) 'ERROR: Wrong size of port ',i
          stop
        endif
      enddo

C     Creates another similar vector
      npt = 0
      do ip=1,np
        do i=1,PO(0,ip)
          npt = npt + 1
          nport(npt) = PO(i,ip)
        enddo
      enddo

C     Opens the Output file
      open (unit=11,status='unknown',
     &      form='formatted',file=foutname)

C     Begins the frequency loop
      do k=1,NFreq

        k0  = 2 * pi * Freq(k) / c0
        k02 = k0 * k0

c     Set Coefficients .
        if ( Plane .eq. 'H' ) then
          VKT2(0)   = k02 * Epsr*Mir
          VCOEFF(0) = 1. / Mir
          do i=1,Ndie
            VKT2(i)   = k02 * Eps(i)*Mu(i)
            VCOEFF(i) = 1. / Mu(i)
          enddo
        else 
          VKT2(0)   = k02 * EPSR * MIR - (pi/a)**2
          VCOEFF(0) = 1. / EPSR
        endif      

C     Known vector building     
        icc = 0
        if (ELE(0,1).eq.3 .or. ELE(0,1).eq.4) then
          nord = 1
        else
          nord = 2
        endif
        im = 1
            
        do ip=1,np      

C     INIZIALIZATION
        DO I = 1,3*MAXBAND+1
          DO J = 1,nnode
            FEM(I,J) = (0.,0.)
          ENDDO
        ENDDO
        DO I = 1,nmode*np
          DO J = 1,nmode*np
            AC(I,J) = (0.,0.)
          ENDDO
        ENDDO
        DO I = 1,nmode*np
          DO J = 1,MAXPORTS*MAXNXP
            BC(I,J) = (0.,0.)
          ENDDO
        ENDDO
        DO I = 1,MAXPORTS*MAXNXP
          DO J = 1,nmode*np
            CD(I,J) = (0.,0.)
          ENDDO
        ENDDO

C     FEM Matrix building
        call BUILD_FEM(MAXNODE,MAXNXE,MAXELE,MAXBAND,MAXNXP,
     &                 ELE,xy,NLAB,ELAB,nele,nnode,Lpec,Lpmc,PO,
     &                 Ndie,Material,Eps,Np,FEM,ku,kl,kd,
     &                 Plane,Vcoeff,vkt2) 
             
C     BUILDS AC, BC and CD
        call BUILD_AC(MAXNODE,MAXNXP,MAXPORTS,MAXMODE,XY,AC,BI,RZ,
     &                PO,a,k0,k02,zeta0,Np,Nmode,Plane)

        call BUILD_BC_CD (MAXNODE,MAXNXP,MAXPORTS,MAXMODE,XY,
     &                    BC,CD,BI,RZ,PO,a,Np,Nmode,Plane)        
              
        write (*,*) '+--- Exciting port ',ip,' with mode 1'        
        call vetcinit ( Field,nnode )
        call vetcinit ( IC,nmnp )
        call vetcinit ( ID,npt )
        do i=1,nnode
          ipvt(i) = 0
        enddo
        abp = dist( xy(1,PO(1,ip)), xy(2,PO(1,ip)),
     &              xy(1,PO(PO(0,ip),ip)),xy(2,PO(PO(0,ip),ip)) )
        piabp = pi / abp
        nel = (PO(0,ip) - 1) / nord
        if (ip .gt. 1) icc = icc + PO(0,ip-1)
        i1  = (ip-1)*Nmode
        if (Plane .eq.'E') then
          pia = pi / a
          IC(i1+1) =  RZ(1,ip) * abp
          aux = (0.,1.) * BI(1,ip) * RZ(1,ip)
          do i = 1,nel
            i1  = (i-1) * nord + 1
            i2  = i1 + nord
            dx  = dist( xy(1,PO(i1,ip)), xy(2,PO(i1,ip)),
     &                  xy(1,PO(i2,ip)), xy(2,PO(i2,ip)) )
            x1  = dist( xy(1,PO( 1,ip)), xy(2,PO( 1,ip)),
     &                  xy(1,PO(i1,ip)), xy(2,PO(i1,ip)) )
            do j=1,nord+1
              integ = dx * real( Pjn_expa(nord+1,j,(0.,0.)) )
              ID(icc+i1+j-1) = ID(icc+i1+j-1) - aux * integ
            enddo
          enddo
        else

          IC(i1+im) = - Pi * RZ(im,ip) / 2.
          aux  = (0.,1.) * piabp * im
          aux1 = aux * BI(im,ip) * RZ(im,ip)
          do i = 1,nel
            i1  = (i-1) * nord + 1
            i2  = i1 + nord
            dx  = dist( xy(1,PO(i1,ip)), xy(2,PO(i1,ip)),
     &                  xy(1,PO(i2,ip)), xy(2,PO(i2,ip)) )
            x1  = dist( xy(1,PO( 1,ip)), xy(2,PO( 1,ip)),
     &                  xy(1,PO(i1,ip)), xy(2,PO(i1,ip)) )
            do j=1,nord+1
              integ = dx * aimag( exp(aux*x1) *
     &                            Pjn_expa(nord+1,j,aux*dx) )
              ID(icc+i1+j-1) = ID(icc+i1+j-1) +
     &                              aux1 * integ
            enddo
          enddo
        endif


          
C     Solution
              
        call Block_Solver (AC,MAXPORTS*MAXMODE,BC,MAXPORTS*MAXNXP,CD,
     &                FEM,3*MAXBAND+1,MAXNODE,KL,KU,
     &                IC,ID,NNODE,nport,npt,Nmode*2,V1,V2,1)
       
C     STORE RESULTS
         write(*,*) ' +--- STORING results'
         write(11,*) 'Frequency = ', Freq(k)
         if (ip .eq. 1) then
          do j = 1,Nmode
            write(11,*) "S11 Mode ",j, 
     &                              real(v1(j)),aimag(v1(j)),
     &                              abs(v1(j)),
     &                              atan2(aimag(v1(j)),real(v1(j)))
          enddo  
          do j = Nmode+1,2*Nmode
            write(11,*) "S21 Mode ",j-Nmode, 
     &                              real(v1(j)),aimag(v1(j)),
     &                              abs(v1(j)),
     &                              atan2(aimag(v1(j)),real(v1(j)))
          enddo  
          if (flgfld.eq.1) then
            write(fieldfile,'(a3,I3.3,a4)'),"f1p",k,".dat"
            CALL writecres (fieldfile,V2,nnode,1,'M')
          endif
         else
          do j = 1,Nmode
            write(11,*) "S12 Mode ",j, 
     &                              real(v1(j)),aimag(v1(j)),
     &                              abs(v1(j)),
     &                              atan2(aimag(v1(j)),real(v1(j)))
          enddo  
          do j = Nmode+1,2*Nmode
            write(11,*) "S22 Mode ",j-Nmode,
     &                              real(v1(j)),aimag(v1(j)),
     &                              abs(v1(j)),
     &                              atan2(aimag(v1(j)),real(v1(j)))
          enddo
          if (flgfld.eq.1) then  
            write(fieldfile,'(a3,I3.3,a4)'),"f2p",k,".dat"
            CALL writecres (fieldfile,V2,nnode,1,'M')
          endif
         endif
        enddo
         
C     STORE RESULTS
        
      enddo    
      
C     STORE RESULTS

      END



