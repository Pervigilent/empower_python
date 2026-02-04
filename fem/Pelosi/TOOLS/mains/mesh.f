C  =====================================================================
C
C     SIMPLE MESH GENERATOR FOR ARBITRARY DOMAINS
C     FOR TRIANGULAR AND QUADRILATERAL ELEMENTS
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C
C  =====================================================================
      program mesh
C  =====================================================================
      integer    MAXNODE, MAXNXE, MAXELE, 
     &           MAXSPIG, MAXBLCK, NPTXBLK, 
     &           MAXPT, MAXEXS 
      parameter (MAXNODE = 3000,    ! Maximum node number
     &           MAXNXE  = 8,       ! Maximum nuber of nodes in each element
     &           MAXELE  = 4000,    ! Maximum number of elements
     &           MAXSPIG = 6000,    ! Maximum number of edges
     &           MAXBLCK = 100,     ! Maximum number of blocks
     &           NPTXBLK = 8,       ! Maximum number of points per block
     &           MAXPT   = 300,     ! Maximum number of Points
     &           MAXEXS  = 50)      ! Maximum element side on block side                                  
C     ---------- Primary mesh variables    
      real    XY(2,MAXNODE)         ! Coordiantes of nodes
      integer ELE(0:MAXNXE,MAXELE)  ! Connettivity matrix
      integer SPIG(0:MAXNXE,MAXELE) ! Edges Matrix
      integer NLAB(MAXNODE)         ! Node Labels
      integer ELAB(MAXELE)          ! Element Labels   
      integer SLAB(MAXSPIG)         ! Edge Labels
      integer NNODE                 ! Number of Nodes
      integer NELE                  ! Number of Elements                                           
      integer NSPIG                 ! Number of Edges
C     ---------- Secondary mesh variables      
      integer NBLOCK(MAXBLCK,NPTXBLK)!Block Definitions
      real    XYPOINT(MAXPT,2)      ! Coordinate of Points
      integer LBLOCK(MAXBLCK)       ! Label of Blocks
      integer LPOINT(MAXPT)         ! Label of Points       
      integer NSEBL(MAXBLCK,2)      ! Number of element sides on block side
      real    STSEBL(MAXBLCK,2,MAXEXS)! Steps for the element sides
      integer NPT                   ! Number of points
      integer NBL                   ! Number of Blocks
      integer NeleType(MAXBLCK)     ! Element Type       
                                    ! 3 - Triangular, first order      
                                    ! 6 - Triangular, second order      
                                    ! 4 - Qadrilateral, first order      
                                    ! 8 - Quadrilateral, second order      
      character*256 foutname        ! Output File
C     ---------- Inputs 
      write (*,*) '+-------------------------------+'
      write (*,*) '| Reading Input File            |'
      call GetInputs (MAXBLCK,NPTXBLK,MAXPT,MAXEXS,
     &                NBLOCK,XYPOINT,LBLOCK,LPOINT,NSEBL,STSEBL,
     &                NPT,NBL,NeleType,foutname)
C     ---------- Generation                          
      write (*,*) '| Generating Nodal Mesh         |'
      call Generate  (MAXNODE,MAXNXE,MAXELE,MAXBLCK,NPTXBLK,
     &                MAXPT,MAXEXS,NBLOCK,XYPOINT,LBLOCK,
     &                LPOINT,NSEBL,STSEBL,NBL,NeleType,
     &                XY,ELE,NLAB,ELAB,NNODE,NELE)
C     ---------- Edge Generation                     
      write (*,*) '| Generating Edge Mesh          |'
      call Edges     (MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                ELE,SPIG,SLAB,NLAB,NELE,NSPIG)
C     ---------- Saving Mesh                
      write (*,*) '| Writing Output File           |'
      call WriteMesh (foutname,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                NNODE,NELE,NSPIG)              
      write (*,*) '+-------------------------------+'
      end                                    
      
C  =====================================================================
C  
      Subroutine GetInputs (MAXBLCK,NPTXBLK,MAXPT,MAXEXS,
     &                      NBLOCK,XYPOINT,LBLOCK,LPOINT,NSEBL,STSEBL,
     &                      NPT,NBL,NeleType,foutname)
C  
C  =====================================================================
C     Reads the Inpot data file
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer    MAXBLCK, NPTXBLK, MAXPT, MAXEXS
C     [OUT] 
      integer NBLOCK(MAXBLCK,NPTXBLK),LBLOCK(MAXBLCK)
      integer LPOINT(MAXPT),NSEBL(MAXBLCK,2)
      real    XYPOINT(MAXPT,2),STSEBL(MAXBLCK,2,MAXEXS)
      integer Npt,Nbl,NeleType(MAXBLCK)
      character*256 foutname
C     [LOCAL]      
      character*256 fn        
      integer       nl,i,j
      integer       NEx,NEy

C     ---------- Keyboard Input
      write (*,*) 'Geometry File ? '
      read (*,*)  fn   

C     ---------- File Input      
      open (UNIT=1,ACCESS='SEQUENTIAL', ERR=1000, 
     &      FILE=fn, FORM='FORMATTED')           
      read (1,'(a256)',ERR=1001) foutname
      read (1,*,ERR=1001) NPT,NBL
      do i=1,Nbl
        read (1,*,ERR=1002) NL,NELETYPE(NL),
     &       (NBLOCK(NL,j),j=1,8),LBLOCK(NL)
      enddo                    
      Do i=1,NPT             
        read (1,*,ERR=1003) NL,XYPOINT(NL,1),XYPOINT(NL,2),LPOINT(NL)
      enddo  
      do i=1,Nbl
        read (1,*,ERR=1004) NL,NEx,NEy                             
        read (1,*,ERR=1004) (STSEBL(NL,1,j),j=1,NEx)
        read (1,*,ERR=1004) (STSEBL(NL,2,j),j=1,NEy)
        NSEBL(NL,1)=NEx
        NSEBL(NL,2)=NEy
      enddo
      goto 1010
1000  write(*,*) '****ERROR****'
      write(*,*) 'File ', fn, ' not found'
      stop
1001  write(*,*) '****ERROR****'
      write(*,*) 'Error reading header of file ', fn
      stop
1002  write(*,*) '****ERROR****'
      write(*,*) 'Error reading Block definitions in file ', fn
      stop
1003  write(*,*) '****ERROR****'
      write(*,*) 'Error reading Point definitions in ', fn
      STOP
1004  write(*,*) '****ERROR****'
      write(*,*) 'Error reading Block subdivision in ', fn
      STOP
1010  close(1)
      return
      end
      
C  =====================================================================
C
      Subroutine Generate  (MAXNODE,MAXNXE,MAXELE,MAXBLCK,NPTXBLK,
     &                      MAXPT,MAXEXS,NBLOCK,XYPOINT,LBLOCK,
     &                      LPOINT,NSEBL,STSEBL,NBL,NeleType,
     &                      XY,ELE,NLAB,ELAB,NNODE,NELE)
C  
C  =====================================================================
C     Performs the actual mesh generation
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      real       epsilon
      parameter (epsilon=0.00001)
C     [IN]
      integer MAXNODE, MAXNXE, MAXELE 
      integer MAXBLCK, NPTXBLK, MAXPT, MAXEXS 
      integer NBLOCK(MAXBLCK,NPTXBLK),LBLOCK(MAXBLCK)
      integer LPOINT(MAXPT),NSEBL(MAXBLCK,2)
      real    XYPOINT(MAXPT,2),STSEBL(MAXBLCK,2,MAXEXS)
      integer NBL,NeleType(MAXBLCK)
C     [OUT]
      real    XY(2,MAXNODE)
      integer ELE(0:MAXNXE,MAXELE),NLAB(MAXNODE),ELAB(MAXELE)
      integer NNODE,NELE
C     [LOCAL]
      integer i,j,k,l,nNODEX,NNODEY,noffset
      REAL    AUX,xscale,yscale,eta,zeta

C     ---------- External Function
      real    Shapef
      integer Labellize

C     ---------- Inizialization
      nnode=0          
      nele=0
      do i=1,NBL
C     ---------- Loop on blocks                                              
        AUX=0
        DO J=1,NSEBL(I,1)
          IF (STSEBL(I,1,J).EQ.0.0) STSEBL(I,1,J)=1.0
          AUX=AUX+STSEBL(I,1,J)
        ENDDO   
        xscale = 1/aux
        aux = 0
        DO J=1,NSEBL(I,2)
          IF (STSEBL(I,2,J).EQ.0.0) STSEBL(I,2,J)=1.0
          AUX=AUX+STSEBL(I,2,J)        
        ENDDO  
        yscale = 1/aux
        NNodex=NSEBL(I,1)+1
        NNodey=NSEBL(I,2)+1
C     ---------- Computation of Nodes
        zeta = -1.0
        noffset = nnode
        do j=1,nnODEy
          eta = -1.0
          do k=1,nnodex
            nnode = nnode+1
            xy(1,nnode)=0.0
            xy(2,nnode)=0.0        
c     ---------- Order 1 (Quad & Tria)
            do l=1,8
              xy(1,nnode) = xy(1,nnode)+
     &                    Shapef(l,eta,zeta)*XYPOINT(NBLOCK(i,l),1)
              xy(2,nnode) = xy(2,nnode)+
     &                    Shapef(l,eta,zeta)*XYPOINT(NBLOCK(i,l),2)
            enddo
c     ---------- Labels
            NLAB(nnode) = labellize(k,j,nnodex,nnodey,
     &                    lpoint(nblock(i,1)),lpoint(nblock(i,2)),
     &                    lpoint(nblock(i,3)),lpoint(nblock(i,4)),
     &                    lpoint(nblock(i,5)),lpoint(nblock(i,6)),
     &                    lpoint(nblock(i,7)),lpoint(nblock(i,8)))
c     ---------- Order 2 (Quad & Tria)
            if (NeleType(i) .gt. 4 .and. k .lt. nnodex) then
              nnode = nnode+1
              xy(1,nnode)=0.0
              xy(2,nnode)=0.0        
              do l=1,8
                xy(1,nnode) = xy(1,nnode)+Shapef(l,eta+xscale*
     &                    STSEBL(I,1,k),zeta)*XYPOINT(NBLOCK(i,l),1)
                xy(2,nnode) = xy(2,nnode)+Shapef(l,eta+xscale*
     &                    STSEBL(I,1,k),zeta)*XYPOINT(NBLOCK(i,l),2)
              enddo                            
c     ---------- Labels                                                         
              NLAB(nnode) = labellize(2*k,j,2*nnodex-1,nnodey,
     &                    lpoint(nblock(i,1)),lpoint(nblock(i,2)),
     &                    lpoint(nblock(i,3)),lpoint(nblock(i,4)),
     &                    lpoint(nblock(i,5)),lpoint(nblock(i,6)),
     &                    lpoint(nblock(i,7)),lpoint(nblock(i,8)))
            endif
            if (NeleType(i) .gt. 4 .and. j .lt. nnodey) then
              nnode = nnode+1
              xy(1,nnode)=0.0
              xy(2,nnode)=0.0        
              do l=1,8
                xy(1,nnode) = xy(1,nnode)+Shapef(l,eta,zeta+
     &                    yscale*STSEBL(I,2,j))*XYPOINT(NBLOCK(i,l),1)
                xy(2,nnode) = xy(2,nnode)+Shapef(l,eta,zeta+
     &                    yscale*STSEBL(I,2,j))*XYPOINT(NBLOCK(i,l),2)
              enddo              
c     ---------- Labels                                                         
              NLAB(nnode) = labellize(k,2*j,nnodex,2*nnodey-1,
     &                    lpoint(nblock(i,1)),lpoint(nblock(i,2)),
     &                    lpoint(nblock(i,3)),lpoint(nblock(i,4)),
     &                    lpoint(nblock(i,5)),lpoint(nblock(i,6)),
     &                    lpoint(nblock(i,7)),lpoint(nblock(i,8)))
            endif
            if (NeleType(i).eq.6.and.j.lt.nnodey.and.k.lt.nnodex) then
              nnode = nnode+1
              xy(1,nnode)=0.0
              xy(2,nnode)=0.0        
              do l=1,8
                xy(1,nnode) = xy(1,nnode)+Shapef(l,eta+xscale*
     &                    STSEBL(I,1,k),zeta+
     &                    yscale*STSEBL(I,2,j))*XYPOINT(NBLOCK(i,l),1)
                xy(2,nnode) = xy(2,nnode)+Shapef(l,eta+xscale*
     &                    STSEBL(I,1,k),zeta+
     &                    yscale*STSEBL(I,2,j))*XYPOINT(NBLOCK(i,l),2)
              enddo                                          
c     ---------- Need no label, for sure              
            endif
            eta = eta + 2*xscale*STSEBL(I,1,k)
          enddo                               
          zeta = zeta + 2*yscale*STSEBL(I,2,j)
        enddo
c     -------------- Build up elements from the node mesh
        if (neletype(i).le.4) then
          do j=1,nnodey-1
            do k=1,nnodex-1
              if (Neletype(i).eq.3) then
                nele=nele+1 
                ele(0,nele)=3
                ele(1,nele)=k+(j-1)*nnodex +noffset
                ele(2,nele)=k+(j-1)*nnodex+1 +noffset
                ele(3,nele)=k+j*nnodex +noffset
                elab(nele) = LBLOCK(i)
                nele=nele+1 
                ele(0,nele)=3
                ele(1,nele)=k+(j-1)*nnodex+1+noffset
                ele(2,nele)=k+j*nnodex+1+noffset
                ele(3,nele)=k+j*nnodex+noffset
                elab(nele) = LBLOCK(i)
              else         
                nele=nele+1
                ele(0,nele)=4
                ele(1,nele)=k+(j-1)*nnodex+noffset
                ele(2,nele)=k+(j-1)*nnodex+1+noffset
                ele(3,nele)=k+j*nnodex+1+noffset
                ele(4,nele)=k+j*nnodex+noffset
                elab(nele) = LBLOCK(i)
              endif
            enddo
          enddo
        else
          do j=1,nnodey-1
            do k=1,nnodex-1
              if (Neletype(i).eq.6) then
                nele=nele+1    
                ele(0,nele)=6
                ele(1,nele)=4*(k-1)+4*nnodex*(j-1)-2*j+3+noffset
                ele(2,nele)=4*(k-1)+4*nnodex*(j-1)-2*j+3+noffset + 1
                ele(3,nele)=4*(k-1)+4*nnodex*(j-1)-2*j+3+noffset + 2
                ele(4,nele)=4*k+4*nnodex*(j-1)-2*j+3+noffset
                ele(5,nele)=4*(k-1)+4*nnodex*(j-1)-2*j+3+noffset + 3                
                if (j.eq.nnodey-1) then
                  ele(6,nele)=2*(k-1)+4*nnodex*j-2*(j+1)+3+noffset
                else
                  ele(6,nele)=4*(k-1)+4*nnodex*j-2*(j+1)+3+noffset
                endif
                elab(nele) = LBLOCK(i)
                nele=nele+1 
                ele(0,nele)=6
                ele(1,nele)=4*k+4*nnodex*(j-1)-2*j+3+noffset
                if (k.eq.nnodex-1) then
                  ele(2,nele)=4*k+4*nnodex*(j-1)-2*j+3+noffset +1
                else
                  ele(2,nele)=4*k+4*nnodex*(j-1)-2*j+3+noffset + 2
                endif
                ele(3,nele)=4*(k-1)+4*nnodex*(j-1)-2*j+3+noffset + 3                                
                if (j.eq.nnodey-1) then
                  ele(4,nele)=2*k+4*nnodex*j-2*(j+1)+3+noffset
                else
                  ele(4,nele)=4*k+4*nnodex*j-2*(j+1)+3+noffset
                endif
                if (j.eq.nnodey-1) then
                  ele(5,nele)=2*k+4*nnodex*j-2*(j+1)+3+noffset - 1
                else
                  ele(5,nele)=4*k+4*nnodex*j-2*(j+1)+3+noffset - 3
                endif                                           
                if (j.eq.nnodey-1) then
                  ele(6,nele)=2*(k-1)+4*nnodex*j-2*(j+1)+3+noffset 
                else
                  ele(6,nele)=4*(k-1)+4*nnodex*j-2*(j+1)+3+noffset 
                endif                                           
                elab(nele) = LBLOCK(i)
              else                    
                nele=nele+1
                ele(0,nele)=8
                ele(1,nele)=3*(k-1)+3*nnodex*(j-1)-j+2+noffset
                ele(2,nele)=3*(k-1)+3*nnodex*(j-1)-j+2+noffset + 1
                ele(3,nele)= 3*k+3*nnodex*(j-1)-j+2+noffset                  
                if( k.eq.nnodex-1) then
                  ele(4,nele)= 3*k+3*nnodex*(j-1)-j+2+noffset + 1
                else
                  ele(4,nele)= 3*k+3*nnodex*(j-1)-j+2+noffset + 2 
                endif
                if ( j.eq.nnodey-1) then                                  
                  ele(5,nele)= 2*k+3*nnodex*j-j+1+noffset                  
                else
                  ele(5,nele)= 3*k+3*nnodex*j-j+1+noffset                  
                endif                
                if ( j.eq.nnodey-1) then                                  
                  ele(6,nele)= 2*k+3*nnodex*j-j+1+noffset - 1
                else
                  ele(6,nele)= 3*k+3*nnodex*j-j+1+noffset - 2                 
                endif  
                if ( j.eq.nnodey-1) then                                  
                  ele(7,nele)= 2*(k-1)+3*nnodex*j-j+1+noffset                  
                else
                  ele(7,nele)= 3*(k-1)+3*nnodex*j-j+1+noffset                  
                endif  
                ele(8,nele)=3*(k-1)+3*nnodex*(j-1)-j+2+noffset + 2              
                elab(nele) = LBLOCK(i)
              endif                  
            enddo
          enddo
        endif                              
      enddo  
c-------------- Extermination of duplicate nodes
      do i=1,nnode
        j=i+1
        do while (j.le.nnode)
          if (sqrt((xy(1,i)-xy(1,j))**2 +      
     &             (xy(2,i)-xy(2,j))**2) .le. epsilon) then
            do k=j+1,nnode
              xy(1,k-1)=xy(1,k)
              xy(2,k-1)=xy(2,k)                       
              nlab(k-1)=nlab(k)
            enddo  
            nnode=nnode-1
            do k=1,nele
              do l=1,ele(0,k)
                if (ele(l,k).eq.j) ele(l,k)=i
                if (ele(l,k).gt.j) ele(l,k)=ele(l,k)-1
              enddo
            enddo
	    j=j-1
          endif 
	  j=j+1
        enddo
      enddo          
c     ---------- For Tria 2 only, deisoparametrize
      do i=1,nele
        if (ele(0,i).eq.6) then
          xy(1,ele(2,i))=(xy(1,ele(1,i))+xy(1,ele(4,i)))/2.
          xy(2,ele(2,i))=(xy(2,ele(1,i))+xy(2,ele(4,i)))/2.
          xy(1,ele(3,i))=(xy(1,ele(1,i))+xy(1,ele(6,i)))/2.
          xy(2,ele(3,i))=(xy(2,ele(1,i))+xy(2,ele(6,i)))/2.
          xy(1,ele(5,i))=(xy(1,ele(6,i))+xy(1,ele(4,i)))/2.
          xy(2,ele(5,i))=(xy(2,ele(6,i))+xy(2,ele(4,i)))/2.
        endif                      
      enddo
      return  
      end      
      
C  =====================================================================
C  
      Subroutine Edges  (MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                   ELE,SPIG,SLAB,NLAB,NELE,NSPIG)
C      
C  =====================================================================
C     Generates edge elements
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]      
      integer MAXNODE,MAXNXE,MAXELE,MAXSPIG 
      integer ELE(0:MAXNXE,MAXELE),NLAB(MAXNODE),NELE
C     [OUT]      
      integer SPIG(0:MAXNXE,MAXELE),SLAB(MAXSPIG),NSPIG
C     [LOCAL]      
      integer SIDE(2,4,2),NSIDE,NSIDEB,i,j,k,l,n1,n2
      logical exists,flag

C     ---------- Local Edges                    
      side(1,1,1)=1
      side(1,1,2)=2
      side(1,2,1)=2
      side(1,2,2)=3
      side(1,3,1)=3
      side(1,3,2)=1
      side(2,1,1)=1
      side(2,1,2)=2
      side(2,2,1)=2
      side(2,2,2)=3
      side(2,3,1)=3
      side(2,3,2)=4
      side(2,4,1)=4
      side(2,4,2)=1         
      flag = .false.
C     ---------- Edge Numbering      
      nspig=0        
      do i=1,nele
        nside = ele(0,i)
        if (nside.gt.5) then
          flag = .true.
        else  
          do j = 1,nside
            n1=ele(side(nside-2,j,1),i)
            n2=ele(side(nside-2,j,2),i)
            exists=.FALSE.
            do k = 1,i-1     
              nsideb = ele(0,k)
              if (nsideb.lt.5) then
                do l = 1,nsideb
                  if (ele(side(nsideb-2,l,1),k) .eq. n1 .and.
     &                ele(side(nsideb-2,l,2),k) .eq. n2) then
                    spig(j,i)=spig(l,k) 
                    exists=.TRUE.
                  endif 
                  if (ele(side(nsideb-2,l,1),k) .eq. n2 .and.  
     1                ele(side(nsideb-2,l,2),k) .eq. n1) then
                     spig(j,i)=-spig(l,k) 
                     exists=.TRUE.
                  endif 
                enddo
              endif  
            enddo
            if (.not.exists) then
              nspig=nspig+1
              spig(j,i)=nspig
              slab(nspig) = LabelEdge(MAXNXE,MAXELE,ELE,NELE,
     &                                n1,n2,nlab(n1),nlab(n2))
            endif
          enddo
          spig(0,i)=nside
        endif  
      enddo                             
      if (flag) then
        write(*,*) '****WARNING****'
        write(*,*) 'EDGES can be computed only for first order elements'
      endif
      return
      end
      
C  =====================================================================
C  
      integer function Labellize (k,j,nnodex,nnodey,
     &                            l1,l2,l3,l4,l5,l6,l7,l8)                                           
C      
C  =====================================================================
C     Provides nodes labels
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer k,j,nnodex,nnodey,l1,l2,l3,l4,l5,l6,l7,l8
C     ---------- Compute label      
      Labellize = 0
      if (k.eq.1.and.j.eq.1) Labellize=l1
      if (k.gt.1.and.k.lt.nnodex.and.j.eq.1) Labellize=l2
      if (k.eq.nnodex.and.j.eq.1) Labellize=l3
      if (k.eq.nnodex.and.j.gt.1.and.j.le.nnodey) Labellize=l4
      if (k.eq.nnodex.and.j.eq.nnodey) Labellize=l5
      if (k.gt.1.and.k.lt.nnodex.and.j.eq.nnodey) Labellize=l6
      if (k.eq.1.and.j.eq.nnodey) Labellize=l7
      if (k.eq.1.and.j.gt.1.and.j.lt.nnodey) Labellize=l8
      return
      end

C  =====================================================================
C  
      integer function LabelEdge (MAXNXE,MAXELE,ELE,NELE,
     &                            n1,n2,l1,l2)                                         
C      
C  =====================================================================
C     Provides edges labels
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer MAXNXE,MAXELE 
      integer ELE(0:MAXNXE,MAXELE)
      integer n1,n2
C     [LOCAL]
      integer j,k,belong,flag
            
C     ---------- Compute label      
      LabelEdge = 0
      belong=0
      do j=1,NELE
        flag=0
        do k=1,ELE(0,j)
          if (ELE(k,j).eq.n1) then
            flag=flag+1
          endif  
          if (ELE(k,j).eq.n2) then
            flag=flag+1
          endif  
        enddo
        if (flag.gt.1) then
          belong = belong+1
        endif
      enddo  
      
      if (belong.eq.1) then
        if(l1.lt.l2) then
          LabelEdge=l1
        else
          LabelEdge=l2
        endif    
      endif
                 
      return
      end

C  =====================================================================
C  
      Real Function Shapef (l,s,t)
C      
C  =====================================================================
C     Shape functions for quadrilateral isoparametric elements
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer l
      real    s,t
C     ---------- Computes shapef Functions            
      if (l.eq.1) then
        shapef = 0.25*(1.0-s)*(1.0-t)*(-s-t-1.0)
      elseif (l.eq.2) then
        shapef = 0.5*(1.0-s*s)*(1.0-t)
      elseif (l.eq.3) then        
        shapef = 0.25*(1.0+s)*(1.0-t)*(s-t-1.0)
      elseif (l.eq.4) then
        shapef = 0.5*(1.0-t*t)*(1.0+s)           
      elseif (l.eq.5) then        
        shapef = 0.25*(1.0+s)*(1.0+t)*(s+t-1.0)
      elseif (l.eq.6) then
        shapef = 0.5*(1.0-s*s)*(1.0+t)
      elseif (l.eq.7) then        
        shapef = 0.25*(1.0-s)*(1.0+t)*(-s+t-1.0)
      else
        shapef = 0.5*(1.0-t*t)*(1.0-s)
      endif
      return
      end                            
