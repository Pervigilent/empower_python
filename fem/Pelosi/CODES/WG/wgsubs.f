C  =====================================================================
C
      subroutine globm (MAXNODE, MAXNXE, MAXELE, MAXSPIG, 
     &                  KU, KL, KD, MAXMAT, ELE, ELAB, SPIG, XY, 
     &                  NNODE, NELE, NSPIG,
     &                  A, B)
C
C  =====================================================================
C     CONSTRUCTS THE FREQUENCY INDEPENDENT PART OF THE GLOBAL SYSTEM 
C     MATRICES
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      implicit none
C     [IN]
      integer    MAXNODE, MAXNXE, MAXELE,
     &           MAXSPIG, KU, KL, KD
      integer    MAXMAT
      real    XY(2,MAXNODE)         ! Coordinates of nodes
      integer ELE(0:MAXNXE,MAXELE)  ! Connettivity matrix
      integer SPIG(0:MAXNXE,MAXELE) ! Edges Matrix
      integer ELAB(MAXELE)          ! Element Labels   
      integer NNODE                 ! Number of Nodes
      integer NELE                  ! Number of Elements                                           
      integer NSPIG                 ! Number of Edges

C     [OUT]
      REAL a(MAXNODE+MAXSPIG, MAXNODE+MAXSPIG)
      REAL b(MAXNODE+MAXSPIG, MAXNODE+MAXSPIG)          

C     [LOCAL]
      INTEGER i,j,IE,n1,n2
      REAL    gg(8, 8),st(8, 8),sz(8, 8),Tz(8, 8),tt(8, 8)
      real    l(3),Ar

c     ---------- Zeroing
      do j = 1,NSPIG+NNODE
         do i = 1,NSPIG+NNODE
	    a(i, j) = 0.
	    b(i, j) = 0.
	 end do
      end do
      
      do IE = 1,NELE
c     ---------- Element matrices, three kinds
         call ELENMAT(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,SZ,8,TZ,8)
         call ELEEMAT(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,ST,8,TT,8)
      	 call ELEENMAT(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,GG,8)

c     ---------- Assembling
c     (   |                )(  )   ( Sz      2      | trans(G) )(  )
c     ( 0 |        0       )(Hz)   (---- - k0 eps Tz|--------- )(Hz)
c     (   |                )(  )  2( mu             |   mu     )(  )
c     (---|----------------)(--)=g (----------------|----------)(--)
c     (   | St      2      )(  )   (      G         |    Tt    )(  )
c     ( 0 |---- - k0 eps Tt)(Ht)   (     ----       |   ----   )(Ht)
c     (   | mu             )(  )   (      mu        |    mu    )(  )
          
	  do i = 1,ELE(0, IE)
	     n1 = ELE(i, IE)
c      ----------Bzz
	     do j = 1,ELE(0, IE)
		n2 = ELE(j, IE)
                b(n1, n2) = b(n1, n2) + Sz(i, j)
	     end do
c      ----------Bzt
	     do j = 1,SPIG(0, IE)
		n2 = SPIG(j, IE)
                b(n1, NNODE + IABS(n2)) = b(n1, NNODE + IABS(n2)) + 
     &               ISIGN(1, n2)*Gg(j, i)
	     end do
	  end do

	  do i = 1,SPIG(0, IE)
	     n1 = SPIG(i, IE)
c     ----------Btz 
	     do j = 1,ELE(0, IE)
		n2 = ELE(j, IE)
                b(NNODE + IABS(n1), n2) = b(NNODE + IABS(n1), n2) +
     &               ISIGN(1, n1)*Gg(i, j)
	     end do
c     ----------Btt,A
	     do j = 1,SPIG(0, IE)
		n2 = SPIG(j, IE)
                b(NNODE + IABS(n1), NNODE + IABS(N2)) = 
     &               b(NNODE + IABS(n1), NNODE + IABS(n2)) +
     &               ISIGN(1, n1)*ISIGN(1, n2)*Tt(i, j)
                a(NNODE + IABS(n1), NNODE + IABS(n2)) = 
     &               a(NNODE + IABS(n1), NNODE + IABS(n2)) +
     &               ISIGN(1, n1)*ISIGN(1, n2)*St(i, j)
	     end do
          end do
       end do
     
      return
      end

C  =====================================================================
C
      subroutine dimmat (MAXNODE, MAXNXE, MAXELE, MAXSPIG, MAXMAT,
     &                   KU, KL, KD, ELE, SPIG, XY, NNODE, NELE, NSPIG, 
     &                   MLAB, EPS, NEM,  A, B, k02, nnn, ierr, peclab, 
     &                   ELAB, NLAB, SLAB, alfai, alfar, beta)
C
C  =====================================================================
C     CONSTRUCTS THE FREQUENCY DEPENDENT PART OF THE GLOBAL SYSTEM 
C     MATRICES AND COMPUTES THE EIGENVALUES
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      implicit none
C     [IN]
      integer    MAXNODE, MAXNXE, MAXELE,
     &           MAXSPIG, MAXMAT, KU, KL, KD
      real    XY(2,MAXNODE)         ! Coordinates of nodes
      integer ELE(0:MAXNXE,MAXELE)  ! Connettivity matrix
      integer SPIG(0:MAXNXE,MAXELE) ! Edges Matrix
      integer NLAB(MAXNODE)         ! Node Labels
      integer ELAB(MAXELE)          ! Element Labels   
      integer SLAB(MAXSPIG)         ! Edge Labels
      integer NNODE                 ! Number of Nodes
      integer NELE                  ! Number of Elements                                           
      integer NSPIG                 ! Number of Edges
      integer MLAB(MAXMAT)          ! Material Labels
      real    EPS(MAXMAT)           ! Material Epsilons
      integer nnn                                
      integer peclab
      integer NEM
      real    k02
      
C     [IN/OUT]
      REAL    a(MAXNODE+MAXSPIG, MAXNODE+MAXSPIG)  
      REAL    b(MAXNODE+MAXSPIG, MAXNODE+MAXSPIG)
      
C     [OUT]      
      INTEGER IERR                  ! if = 0 => Everything OK
      REAL    alfai(MAXNODE+MAXSPIG),alfar(MAXNODE+MAXSPIG)
      REAL    beta(MAXNODE+MAXSPIG),z(MAXNODE+MAXSPIG, MAXNODE+MAXSPIG)      

C     [LOCAL]
      INTEGER i, j, k, n1, n2, IE, ofs, act
      REAL    epsr,st(8, 8),sz(8, 8),Tz(8, 8),tt(8, 8)
      real    l(3),mina,maxa,minb,maxb
C     [LAPACK] 
      integer lwork
      parameter (lwork=16000)
      real    work(lwork),VSL(1),VSR(1)


      do IE = 1,NELE
         
c     ---------- Element matrices
         call ELENMAT(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,SZ,8,TZ,8)
         call ELEEMAT(MAXNODE,MAXNXE,MAXELE,XY,ELE,IE,ST,8,TT,8)
 
c     ---------- Assembling
c     (   |                )(  )   ( Sz      2      | trans(G) )(  )
c     ( 0 |        0       )(Hz)   (---- - k0 eps Tz|--------- )(Hz)
c     (   |                )(  )  2( mu             |   mu     )(  )
c     (---|----------------)(--)=g (----------------|----------)(--)
c     (   | St      2      )(  )   (      G         |    Tt    )(  )
c     ( 0 |---- - k0 eps Tt)(Ht)   (     ----       |   ----   )(Ht)
c     (   | mu             )(  )   (      mu        |    mu    )(  )

          EPSR = 1.
          do i = 1,NEM
            if (MLAB(i).eq.ELAB(IE)) EPSR = EPS(i)
          end do

c     ----------Bzz
	 do i = 1,ELE(0, IE)
	    n1 = ELE(i, IE)
	    do j = 1,ELE(0, IE)
	       n2 = ELE(j, IE)
               b(n1, n2) = b(n1, n2) - k02*EPSR*Tz(i, j)
	    end do
	 end do
c     ----------A
	 do i = 1,SPIG(0, IE)
	    n1 = SPIG(i, IE)
	    do j = 1,SPIG(0, IE)
	       n2 = SPIG(j, IE)               
               a(NNODE + IABS(n1), NNODE + IABS(n2)) = 
     &              a(NNODE + IABS(n1), NNODE + IABS(n2)) -
     &              k02*EPSR*ISIGN(1, n1)*ISIGN(1, n2)*Tt(i, j)
	    end do
	 end do
      end do

c     ---------- Killing of PEC nodes and edges
      if (nnn .ne. (NNODE+NSPIG)) then

c     ----------Nodal Rows and Columns
        ofs=0
        act=0
        do i = 1,NNODE
          act=act+1
          if (NLAB(i).eq.PECLAB) then
            ofs=ofs+1
            do j = act+1,NNODE+NSPIG
              do k = 1,NNODE+NSPIG
                a(j-1,k)=a(j,k)
                b(j-1,k)=b(j,k)
              enddo
            enddo
            do j = act+1,NNODE+NSPIG
              do k = 1,NNODE+NSPIG
                a(k,j-1)=a(k,j)
                b(k,j-1)=b(k,j)
              enddo
            enddo
            act=act-1
          endif
        enddo
c     ----------Edge Rows and Columns
        act=0
        do i = 1,NSPIG
          act=act+1
          if (SLAB(i).eq.PECLAB) then
            do j = act+1+NNODE-ofs,NNODE+NSPIG-ofs
              do k = 1,NNODE+NSPIG
                a(j-1,k)=a(j,k)
                b(j-1,k)=b(j,k)
              enddo
            enddo
            do j = act+1+NNODE-ofs,NNODE+NSPIG-ofs
              do k = 1,NNODE+NSPIG
                a(k,j-1)=a(k,j)
                b(k,j-1)=b(k,j)
              enddo
            enddo
            act=act-1
          endif
        enddo         
      endif

      mina=A(1,1)
      maxa=A(1,1)
      minb=b(1,1)
      maxb=b(1,1)

      do i=1,nnn
         do j=1,nnn
            if(a(i,j).gt.maxa) maxa=A(i,j)
            if(a(i,j).lt.mina) mina=A(i,j)
            if(b(i,j).gt.maxb) maxb=b(i,j)
            if(b(i,j).lt.minb) minb=b(i,j)
         enddo
      enddo

      write (*,*) mina,maxa,minb,maxb

      if (-mina.gt.maxa) maxa=-mina
      if (-minb.gt.maxb) maxb=-minb

      do i=1,nnn
         do j=1,nnn
            a(i,j)=A(i,j)/(maxa)
            b(i,j)=b(i,j)/(maxb)
         enddo
      enddo

c     ----------Solution
      call SGEGV('N','N',nnn,A,MAXNODE+MAXSPIG,B,MAXNODE+MAXSPIG,
     &           alfar,alfai,beta,VSL,1,VSR,1,work,lwork,ierr) 

      do i=1,nnn
        beta(i)=beta(i)*maxb/maxa 
      enddo

      return
      end


C  =====================================================================
C
      subroutine write_res (nmfl, MAXNODE, MAXSPIG, iter, NNODE, 
     &                      nd40, nl40, nnn, alfai, alfar, beta, f, 
     &                      scl, nmode)
C
C  =====================================================================
C     COMPUTES PROPAGATIN CONSTANTS, SORTS AND SAVES THEM
C     QUICK_FEM (C) 1996 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      implicit none
C     [IN]
      integer MAXNODE ,MAXSPIG
      CHARACTER nmfl*256
      INTEGER iter, NNODE, nd40, nl40, nnn, nmode
      REAL    alfai(MAXNODE+MAXSPIG), alfar(MAXNODE+MAXSPIG)
      REAL    beta(MAXNODE+MAXSPIG), f
      
c     [LOCAL]
      CHARACTER nmfl_cpl*256   !File for complex modes
      COMPLEX auxmodoc, gam, modoc(MAXNODE+MAXSPIG)
      INTEGER caux, i, j, k, nwri
      REAL    scl, aux, gama(MAXNODE+MAXSPIG), toll             
    
      toll = 0.5E-4
      nmfl_cpl = nmfl
      nmfl_cpl(INDEX(nmfl,' '):)='.cpl'

c     --------- Discrimination between propagating, evanescent and 
c               complex modes
      caux = 0
      do i = 1,nnn
         if(beta(i) .eq. 0.) then
            write(*, '(A6,I3,A6)') 'beta(',i,' ) = 0'
            gama(i) = -100000.  
         else
           gam = csqrt(alfar(i)/beta(i)+(0.,1.)*alfai(i)/beta(i))
           if (alfar(i) .le. 0. .and. abs(alfai(i)) .le. toll) then
             gama(i) =  sqrt(-alfar(i)/beta(i))
           else if (alfar(i) .gt. 0. .and. abs(alfai(i)) .le. toll) then
             gama(i) = -sqrt(alfar(i)/beta(i))
           else
             gama(i) = -90000.  
             caux = caux+1
             modoc(caux) = gam
           end if
         end if
      end do
      
c     ---------- Sorting
      do k = 1,nnn-1
         do i = 1,nnn-1
            if (gama(i) .lt. gama(i+1)) then
               aux = gama(i)
	       gama(i) = gama(i+1)
	       gama(i+1) = aux
            end if
         end do
      end do
         
c     ---------- Elimination of nonphisical zero eigenvalues
      nwri=0
      do i = 1,nnn
        nwri=nwri+1
        if (ABS(gama(nwri)) .le. toll ) then
          do j=nwri+1,nnn
            gama(j-1)=gama(j)
          enddo  
          nwri=nwri-1
        endif
      enddo     

c     ---------- Writing of results
      open(13, file=nmfl, form='FORMATTED')
      write(13,'(E12.6,10(1X,E12.6))') f,
     &          (gama(i)*1000./scl,i=1,nmode)
      if (caux .gt. 0) then
	 open(14, file=nmfl_cpl, form='FORMATTED')  
	 do k = 1,caux-1
	    do i = 1,caux-1
	       if (AIMAG(modoc(i)) .lt. AIMAG(modoc(i+1))) then
		  auxmodoc = modoc(i)
		  modoc(i) = modoc(i+1)
		  modoc(i+1) = auxmodoc
	       end if
	    end do
	 end do
	       
	 do i = 1,caux/2
	    write(14, '(E12.6,1X,E12.6,1X,E12.6)') f, 
     &   	        AIMAG(modoc(i))*1000./scl,
     &                  -REAL(modoc(i))*1000./scl         
	 enddo
      endif

      return
      end






