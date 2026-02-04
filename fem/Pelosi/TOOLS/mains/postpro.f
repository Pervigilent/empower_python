C  =====================================================================
C
C  SIMPLE FEM POST-PROCESSOR FOR ARBITRARY DOMAINS
C  FOR TRIANGULAR AND QUADRILATERAL ELEMENTS
C  QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C
C  =====================================================================
      program postpro
      integer    MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &           MAXBLCK,NPTXBLK,MAXPT,MAXEXS,MAXRES
      parameter (MAXNODE = 3000,    ! Maximum node number
     &           MAXNXE  = 8,       ! Maximum nuber of nodes in each element
     &           MAXELE  = 4000,    ! Maximum number of elements
     &           MAXSPIG = 4000,    ! Maximum number of edges
     &           MAXBLCK = 100,     ! Maximum number of blocks
     &           NPTXBLK = 8,       ! Maximum number of points per block
     &           MAXPT   = 300,     ! Maximum number of Points
     &           MAXEXS  = 50,      ! Maximum element side on block side                                  
     &           MAXRES  = 4000)    ! Maximum number of results = 
                                    !         max(MAXNODE,MAXSPIG)
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
      real    Result(MAXRES)        ! Results of computation, nodal or edge values.
      integer NRES                  ! Number of results
      integer NTYPE                 ! Type 1 = NODAL, 2 = EDGE
C     ---------- File Names
      character*256 fingname        ! Input File, Geometry
      character*256 findname        ! Input File, Data
      character*256 foutname        ! Output File
C     ---------- Auxiliary Variables
      integer nax,nay  
            
C     ---------- Inputs                         
      write (*,*) '+-------------------------------+'
      write (*,'(1x,a)') '| Input File (Geometry) ?   '
      read  (*,*) fingname
      write (*,'(1x,a)') '| Input File (Data) ?       '
      read  (*,*) findname
      write (*,'(1x,a)') '| Output File (Data) ?      '
      read  (*,*) foutname
      write (*,'(1x,a)') '| Number of point along X ? '
      read  (*,*) nax
      write (*,'(1x,a)') '| Number of point along Y ? '
      read  (*,*) nay
      
C     ---------- File reading
      call ReadMesh (fingname,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &               XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &               NNODE,NELE,NSPIG)              

      call ReadValues (findname,MAXRES,result,nres,ntype)
      
C     ---------- Interpolation
      call Interpolate  (MAXNODE,MAXNXE,MAXELE,MAXRES,
     &                   XY,RESULT,ELE,NNODE,NELE,
     &                   NAX,NAY,foutname)
      write (*,*) '+-------------------------------+'
      end                                    
      
C  =====================================================================
C
      Subroutine ReadValues (findname,MAXRES,result,nres,ntype)
C
C  =====================================================================
C     Reads the datafile
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [OUT]
      integer MAXRES,nres,ntype
      real    result(maxres)
      character*256 findname
C     [LOCAL]      
      character*256 dump          
      integer nr,i
C     ---------- File Input      
      open (UNIT=1,ACCESS='SEQUENTIAL', ERR=4000, 
     &      FILE=findname, FORM='FORMATTED')           
      read (1,'(a7,I5)',ERR=4001) dump,ntype
      read (1,'(a9,I5)',ERR=4001) dump,nres
      
      do i=1,Nres
        read (1,*,ERR=4002) NR,result(NR)
      enddo                    
      goto 4010
4000  write(*,*) '****ERROR****'
      write(*,*) 'File ', findname, ' not found'
      stop
4001  write(*,*) '****ERROR****'
      write(*,*) 'Error reading Header of file ', findname
      stop
4002  write(*,*) '****ERROR****'
      write(*,*) 'Error reading Values in file ', findname
      stop
4010  close(1)
      return
      end

C  =====================================================================
C
      subroutine Interpolate  (MAXNODE,MAXNXE,MAXELE,MAXRES,XY,
     &                         RESULT,ELE,NNODE,NELE,NAX,NAY,foutname)
C  
C  =====================================================================
C     Performs the interpolation of the nodal values
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer MAXNODE,MAXNXE,MAXELE,MAXRES,NAX,NAY
      real    XY(2,MAXNODE),RESULT(MAXRES)
      integer ELE(0:MAXNXE,MAXELE)
      integer NNODE,Nele
      character*256 foutname
C     [LOCAL]
      integer i,j,nec
      real ax,ay,xmax,xmin,ymax,ymin,f,coordsl(8)
C     [EXTERNAL FUNCTION]
      integer nEleContaining
      real    SPointInter

C     ---------- Inizialization
      xmax=xy(1,1)
      xmin=xy(1,1)
      ymax=xy(2,1)
      ymin=xy(2,1)
      do i=1,nnode
        if (xy(1,i).gt.xmax) xmax=xy(1,i)
        if (xy(1,i).lt.xmin) xmin=xy(1,i)
        if (xy(2,i).gt.ymax) ymax=xy(2,i)
        if (xy(2,i).lt.ymin) ymin=xy(2,i)
      enddo        
      
      open (UNIT=1,ACCESS='SEQUENTIAL', ERR=5000, 
     &      FILE=foutname, FORM='FORMATTED')           
                                                               
C     ---------- Loop on Rectangular Grid
      do j=1,NAY
        do i=1,NAX
          ax = (i-1)*(xmax-xmin)/(NAX-1) + xmin
          ay = (j-1)*(ymax-ymin)/(NAY-1) + ymin
          nec = nEleContaining (MAXNODE,MAXNXE,MAXELE,ax,ay,
     &                          XY,ELE,NELE,coordsl)

          if (nec.gt.0) then
            f = SPointInter (MAXNXE,MAXELE,MAXRES,
     &                       RESULT,ELE,
     &                       nec,coordsl)
          else
            f = 0.0
          endif
            
          if (i.eq.NAX) then
            write (1,'(e13.7/)',ERR=5001) f
          else
            write (1,'(e13.7)',ERR=5001) f
          endif  
        enddo          
      enddo    
      goto 5010
      
5000  write(*,*) '****ERROR****'
      write(*,*) 'Cannot open file ', foutname
      stop 
5001  write(*,*) '****ERROR****'
      write(*,*) 'Cannot write in file ', foutname
      stop          
5010  return
      end  
        
C  =====================================================================
C
      function nEleContaining (MAXNODE,MAXNXE,
     &                 MAXELE,ax,ay,XY,ELE,NELE,coordsl)
C      
C  =====================================================================
C     Verifyes if a point is within a certain element
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer MAXNODE,MAXNXE,MAXELE
      real    XY(2,MAXNODE),ax,ay
      integer ELE(0:MAXNXE,MAXELE)
      integer Nele
C     [OUT]
      integer nEleContaining
      real    coordsl(3)                                 
C     [LOCAL] 
      real x(9),y(9),sum
      integer i,j    
      logical flag,IsInsideQ2
      
      nEleContaining=0
      do i=1,nele
        if (ele(0,i).eq.3) then
c     ---------- Triangular, first order        
          x(1)=xy(1,ele(1,i))
          x(2)=xy(1,ele(2,i))
          x(3)=xy(1,ele(3,i))
          y(1)=xy(2,ele(1,i))
          y(2)=xy(2,ele(2,i))
          y(3)=xy(2,ele(3,i))
          coordsl(3)= (x(1)-ax)*(y(2)-ay)-(x(2)-ax)*(y(1)-ay)
          coordsl(1)= (x(2)-ax)*(y(3)-ay)-(x(3)-ax)*(y(2)-ay)
          coordsl(2)= (x(3)-ax)*(y(1)-ay)-(x(1)-ax)*(y(3)-ay)
          if (coordsl(1).ge.0.0 .and.
     &        coordsl(2).ge.0.0 .and.
     &        coordsl(3).ge.0.0 ) then
            sum = coordsl(1)+coordsl(2)+coordsl(3)
            do j=1,3
              coordsl(j)=coordsl(j)/sum
            enddo
            nEleContaining=i
            return
          endif
        elseif (ele(0,i).eq.6) then     
c     ---------- Triangular, second order        
          x(1)=xy(1,ele(1,i))
          x(2)=xy(1,ele(4,i))
          x(3)=xy(1,ele(6,i))
          y(1)=xy(2,ele(1,i))
          y(2)=xy(2,ele(4,i))
          y(3)=xy(2,ele(6,i))     
          coordsl(3)= (x(1)-ax)*(y(2)-ay)-(x(2)-ax)*(y(1)-ay)
          coordsl(1)= (x(2)-ax)*(y(3)-ay)-(x(3)-ax)*(y(2)-ay)
          coordsl(2)= (x(3)-ax)*(y(1)-ay)-(x(1)-ax)*(y(3)-ay)
          if (coordsl(1).ge.0.0 .and.
     &        coordsl(2).ge.0.0 .and.
     &        coordsl(3).ge.0.0 ) then
            sum = coordsl(1)+coordsl(2)+coordsl(3)
            do j=1,3
              coordsl(j)=coordsl(j)/sum
            enddo
            nEleContaining=i
            return
          endif
        elseif (ele(0,i).eq.4) then      
c     ---------- Quadrilateral, first order        
          x(1)=xy(1,ele(1,i))
          x(2)=xy(1,ele(2,i))
          x(3)=xy(1,ele(3,i))
          x(4)=xy(1,ele(4,i))
          x(5)=xy(1,ele(1,i))
          y(1)=xy(2,ele(1,i))
          y(2)=xy(2,ele(2,i))
          y(3)=xy(2,ele(3,i))      
          y(4)=xy(2,ele(4,i))
          y(5)=xy(2,ele(1,i))      
          flag=.true.
          do j=1,4                            
            if ((x(j)-ax)*(y(j+1)-ay)-(x(j+1)-ax)*(y(j)-ay)
     &           .lt. 0.0 ) flag=.false.
          enddo
          if (flag) then
            call RecLocCooQ1 (x,y,ax,ay,coordsl)      
            nEleContaining = i
            return
          endif      
        ELSE  
          x(1)=xy(1,ele(1,i))
          x(2)=xy(1,ele(2,i))
          x(3)=xy(1,ele(3,i))
          x(4)=xy(1,ele(4,i))
          x(5)=xy(1,ele(5,i))
          x(6)=xy(1,ele(6,i))
          x(7)=xy(1,ele(7,i))
          x(8)=xy(1,ele(8,i))
          y(1)=xy(2,ele(1,i))
          y(2)=xy(2,ele(2,i))
          y(3)=xy(2,ele(3,i))      
          y(4)=xy(2,ele(4,i))
          y(5)=xy(2,ele(5,i))      
          y(6)=xy(2,ele(6,i))
          y(7)=xy(2,ele(7,i))
          y(8)=xy(2,ele(8,i))      
          if (IsInsideQ2(x,y,ax,ay)) then
            call RecLocCooQ2 (x,y,ax,ay,coordsl)      
            if (abs(coordsl(1)).le.1 .and. abs(coordsl(2)).le.1) then
              nEleContaining = i
              return
            endif  
          endif      
        endif
      enddo
      end    

C  =====================================================================
C
      function SPointInter (MAXNXE,MAXELE,MAXRES,
     &                      RESULT,ELE,
     &                      nec,coordsl)
C      
C  =====================================================================
C     computes the value in a point by interpolation of the nodal values
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer MAXNXE,MAXELE,MAXRES
      real    RESULT(MAXRES)
      integer ELE(0:MAXNXE,MAXELE)
      integer nec
      real    coordsl(3),values(8)
C     [OUT]
      real    spointinter
C     [LOCAL]
      integer i
C     [EXTERNAL FUNCTION]
      real    Shapef

      if (ele(0,nec).eq.3) then
c     ---------- Triangular, first order   
        spointinter = result(ele(1,nec))*coordsl(1) + 
     &                result(ele(2,nec))*coordsl(2) + 
     &                result(ele(3,nec))*coordsl(3)
        return
      elseif (ele(0,nec).eq.6) then
c     ---------- Triangular, second order         
        spointinter = result(ele(1,nec))*(2.*coordsl(1)-1.)*coordsl(1) + 
     &                result(ele(2,nec))*4.*coordsl(1)*coordsl(2) + 
     &                result(ele(3,nec))*4.*coordsl(3)*coordsl(1) +     
     &                result(ele(4,nec))*(2.*coordsl(2)-1.)*coordsl(2) + 
     &                result(ele(5,nec))*4.*coordsl(3)*coordsl(2) +     
     &                result(ele(6,nec))*(2.*coordsl(3)-1.)*coordsl(3)
        return
      elseif (ele(0,nec).eq.4) then
c     ---------- Quadrilatral, first order         
        spointinter = result(ele(1,nec))*(coordsl(1)-1.)*(coordsl(1)-1.)
     &        /4.-result(ele(2,nec))*(coordsl(1)+1.)*(coordsl(1)-1.)/4.+ 
     &        result(ele(3,nec))*(coordsl(1)+1.)*(coordsl(1)+1.)/4.- 
     &        result(ele(3,nec))*(coordsl(1)-1.)*(coordsl(1)+1.)/4.
        return
      else
c     ---------- Quadrilatral, second order  
        do i=1,8
          values(i) = result(ele(i,nec))
        enddo         
        spointinter = Shapef(values,coordsl(1),coordsl(2))
        return
      endif
      end       
      
C  =====================================================================
C
      subroutine RecLocCooQ1 (x,y,ax,ay,coordsl)      
C
C  =====================================================================
C     Computes the element coordinates of a given point in the element
C     for first order quadrilaterals
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      real eps
      parameter (eps = 1.e-5)
C     [IN]
      real x(8),y(8),ax,ay
C     [OUT]
      real    coordsl(3)                                 
C     [LOCAL] 
      real  cx(4),cy(4),A,B,C,delta,v1,anum,aden
      
      cx(1)=(x(3)-x(4)+x(1)-x(2))/4.
      cx(2)=(x(3)-x(4)-x(1)+x(2))/4.
      cx(3)=(x(3)+x(4)-x(1)-x(2))/4.
      cx(4)=(x(3)+x(4)+x(1)+x(2))/4.      
      cy(1)=(y(3)-y(4)+y(1)-y(2))/4.
      cy(2)=(y(3)-y(4)-y(1)+y(2))/4.
      cy(3)=(y(3)+y(4)-y(1)-y(2))/4.
      cy(4)=(y(3)+y(4)+y(1)+y(2))/4.      

      if (abs(cy(1)).gt.eps) then
        A=cx(1)*cy(3)-cx(3)*cy(1)
        B=cx(1)*(cy(4)-ay)-cy(1)*(cx(4)-ax)+cx(2)*cy(3)-cx(3)*cy(2)
        C=cx(2)*(cy(4)-ay)-cy(2)*(cx(4)-ax)
      
        if (Abs(A) .lt. eps ) then
          coordsl(2) = -C/B                                    
        else
          delta = B*B - 4*A*C
          if (delta .lt. 0.0) then
            write (*,*) '*****ERROR*****'
            write (*,*) 'Uninvertible Q1 element'
            stop
          else
            delta = sqrt(delta)    
            v1 = (-B+delta)/(2*A)
            if (v1.ge.-1. .and. v1 .le. 1.) then  
              coordsl(2) = v1
            else
              coordsl(2) = (-B-delta)/(2*A)
            endif
          endif
        endif
        anum=(ay - cy(3)*coordsl(2) - cy(4))
        aden=(cy(1)*coordsl(2) + cy(2))    
        coordsl(1)= anum/aden
      elseif (abs(cx(1)).gt.eps) then
        A=cx(1)*cy(2)-cx(2)*cy(1)
        B=cx(1)*(cy(4)-ay)-cy(1)*(cx(4)-ax)+cx(3)*cy(2)-cx(2)*cy(3)
        C=cx(3)*(cy(4)-ay)-cy(4)*(cx(4)-ax)
      
        if (Abs(A) .lt. eps ) then
          coordsl(1) = -C/B                                    
        else
          delta = B*B - 4*A*C
          if (delta .lt. 0.0) then
            write (*,*) '*****ERROR*****'
            write (*,*) 'Uninvertible Q1 element'
            stop
          else
            delta = sqrt(delta)    
            v1 = (-B+delta)/(2*A)
            if (v1.ge.-1. .and. v1 .le. 1.) then  
              coordsl(1) = v1
            else
              coordsl(1) = (-B-delta)/(2*A)
            endif
          endif
        endif
        anum=(ax - cx(4) - cx(2)*coordsl(1))
        aden=(cx(1)*coordsl(1) + cx(3))    
        coordsl(2)= anum/aden
      else
        aden = cx(2)*cy(3)-cx(3)*cy(2)
        coordsl(1)=(ax*cy(3)-ay*cx(3)+cx(3)*cy(4)-cx(4)*cy(3))/aden
        coordsl(2)=-(ax*cy(2)-ay*cx(2)+cx(2)*cy(4)-cx(4)*cy(2))/aden
      endif  
      return
      end                   
      
C  =====================================================================
C  
      subroutine RecLocCooQ2 (x,y,sx,sy,coordsl)      
C      
C  =====================================================================
C     Computes the element coordinates of a given point in the element
C     for second order quadrilaterals
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      real eps
      parameter (eps = 1.e-5)
C     [IN]
      real x(8),y(8),sx,sy
C     [OUT]
      real    coordsl(3)                                 
C     [LOCAL] 
      real  ui,vi,uip1,vip1
      real  ax,bx,cx,ay,by,cy
      real  aden
      integer ncnt
      logical flag
C     [EXTERNAL FUNCTION]
      real    GShapeU,GShapeV,Shapef
      
      ui=0
      vi=0
      flag=.true.
      ncnt=0
      
      do while (flag)
        ax=GShapeU(x,ui,vi)
        bx=GShapeV(x,ui,vi)
        cx=sx-Shapef(x,ui,vi)+GShapeU(x,ui,vi)*ui+GShapeV(x,ui,vi)*vi
        ay=GShapeU(y,ui,vi)
        by=GShapeV(y,ui,vi)
        cy=sy-Shapef(y,ui,vi)+GShapeU(y,ui,vi)*ui+GShapeV(y,ui,vi)*vi
        
        aden = ax*by-ay*bx
        if (abs(aden).lt.eps) then
          write (*,*) '****ERROR****'
          write (*,*) 'Cannot extrapolate points'
          return
        else
          uip1=(cx*by-bx*cy)/aden
          vip1=(ax*cy-cx*ay)/aden
          if ((vip1-vi)**2+(uip1-ui)**2 .le. eps) flag=.false.
          ui=uip1
          vi=vip1  
          ncnt=ncnt+1
          if (abs(Shapef(x,ui,vi)-sx).le.eps .and.
     &        abs(Shapef(y,ui,vi)-sy).le.eps) flag=.false.
        endif        
        if (ncnt.gt.30) then
          write (*,*) '****ERROR****'
          write (*,*) 'Inversion does not converge - ',sx,sy
          coordsl(1)=-10.0
          return
        endif                         
      enddo
      coordsl(1)=uip1
      coordsl(2)=vip1
      return
      end


C  =====================================================================
C  
      Logical Function IsInsideQ2(x,y,ax,ay)
C      
C  =====================================================================
C     Tells if a point is inside an isoparametrically deformed quad.
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real x(8),y(8),ax,ay
C     [LOCAL]
      real aq,toll
      logical isi
      
      aq = ((x(4)-x(2))*(y(8)-y(2))-(x(8)-x(2))*(y(4)-y(2)) +
     &      (x(8)-x(6))*(y(4)-y(6))-(x(4)-x(6))*(y(8)-y(6)))/2.
     
      toll = aq/20.0
      
c     ---------- Verify on enlarged subtriangles                
      Isi=.false.
      if ( (x(1)-ax)*(y(2)-ay)-(x(2)-ax)*(y(1)-ay) .gt. -toll .and. 
     &   (x(2)-ax)*(y(8)-ay)-(x(8)-ax)*(y(2)-ay) .gt. -toll .and.
     &   (x(8)-ax)*(y(1)-ay)-(x(1)-ax)*(y(8)-ay) .gt. -toll ) isi=.true.
      if ( (x(2)-ax)*(y(3)-ay)-(x(3)-ax)*(y(2)-ay) .gt. -toll .and. 
     &   (x(3)-ax)*(y(4)-ay)-(x(4)-ax)*(y(3)-ay) .gt. -toll .and.
     &   (x(4)-ax)*(y(2)-ay)-(x(2)-ax)*(y(4)-ay) .gt. -toll ) isi=.true.
      if ( (x(4)-ax)*(y(5)-ay)-(x(5)-ax)*(y(4)-ay) .gt. -toll .and. 
     &   (x(5)-ax)*(y(6)-ay)-(x(6)-ax)*(y(5)-ay) .gt. -toll .and.
     &   (x(6)-ax)*(y(4)-ay)-(x(4)-ax)*(y(6)-ay) .gt. -toll ) isi=.true.
      if ( (x(6)-ax)*(y(7)-ay)-(x(7)-ax)*(y(6)-ay) .gt. -toll .and. 
     &   (x(7)-ax)*(y(8)-ay)-(x(8)-ax)*(y(7)-ay) .gt. -toll .and.
     &   (x(8)-ax)*(y(6)-ay)-(x(6)-ax)*(y(8)-ay) .gt. -toll ) isi=.true.
      if ( (x(2)-ax)*(y(4)-ay)-(x(4)-ax)*(y(2)-ay) .gt. -toll .and. 
     &   (x(4)-ax)*(y(8)-ay)-(x(8)-ax)*(y(4)-ay) .gt. -toll .and.
     &   (x(8)-ax)*(y(2)-ay)-(x(2)-ax)*(y(8)-ay) .gt. -toll ) isi=.true.
      if ( (x(4)-ax)*(y(6)-ay)-(x(6)-ax)*(y(4)-ay) .gt. -toll .and. 
     &   (x(6)-ax)*(y(8)-ay)-(x(8)-ax)*(y(6)-ay) .gt. -toll .and.
     &   (x(8)-ax)*(y(4)-ay)-(x(4)-ax)*(y(8)-ay) .gt. -toll ) isi=.true.
      IsInsideQ2=Isi
      return
      end
         
         
C  =====================================================================
C  
      Real Function Shapef (c,u,v)
C      
C  =====================================================================
C     Isoparametric shape functions for nodal values
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real    c(8)
      real    u,v
C     [LOCAL]
      real    sum      
C     ---------- Computes Shape Functions            
      sum =       c(1) * 0.25*(1.0-u)*(1.0-v)*(-u-v-1.0)
      sum = sum + c(2) * 0.5*(1.0-u*u)*(1.0-v)
      sum = sum + c(3) * 0.25*(1.0+u)*(1.0-v)*(u-v-1.0)
      sum = sum + c(4) * 0.5*(1.0-v*v)*(1.0+u)           
      sum = sum + c(5) * 0.25*(1.0+u)*(1.0+v)*(u+v-1.0)
      sum = sum + c(6) * 0.5*(1.0-u*u)*(1.0+v)
      sum = sum + c(7) * 0.25*(1.0-u)*(1.0+v)*(-u+v-1.0)
      Shapef = sum + c(8)*0.5*(1.0-v*v)*(1.0-u)
      return
      end                            

C  =====================================================================
C  
      Real Function GShapeU (c,u,v)
C      
C  =====================================================================
C     Isoparametric shape functions for coordinates (u)
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real    c(8)
      real    u,v
C     [LOCAL]
      real    sum      
C     ---------- Computes Shape Functions            
      sum =       c(1) * 0.25*(1.0-v)*(2.0*u+v)
      sum = sum + c(2) * (v-1.0)*u
      sum = sum + c(3) * 0.25*(1.0-v)*(2.0*u-v)
      sum = sum + c(4) * 0.5*(1.0-v*v)
      sum = sum + c(5) * 0.25*(v+1.0)*(2.0*u+v)
      sum = sum + c(6) * (v+1.0)*(-u)
      sum = sum + c(7) * 0.25*(1.0+v)*(2.0*u-v)
      GShapeU = sum + c(8)*0.5*(v*v-1.0)
      return
      end                            

C  =====================================================================
C  
      Real Function GShapeV (c,u,v)
C      
C  =====================================================================
C     Isoparametric shape functions for coordinates (v)
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      real    c(8)
      real    u,v
C     [LOCAL]
      real    sum      
C     ---------- Computes Shape Functions            
      sum =       c(1) * 0.25*(1.0-u)*(2.0*v+u)
      sum = sum + c(2) * 0.5*(u*u-1.0)
      sum = sum + c(3) * 0.25*(1.0+u)*(2.0*v-u)
      sum = sum + c(4) * (u+1.0)*(-v)
      sum = sum + c(5) * 0.25*(u+1.0)*(2.0*v+u)
      sum = sum + c(6) * 0.5*(1-u*u)
      sum = sum + c(7) * 0.25*(1.0-u)*(2.0*v-u)
      GShapeV = sum + c(8)*(u-1.0)*v
      return
      end                            

      
