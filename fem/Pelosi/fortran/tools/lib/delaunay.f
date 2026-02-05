C  =====================================================================
C
      Subroutine Delaunay  (MAXNODE,MAXNXE,MAXELE,
     &                      XY,ELE,SPIG,ELAB,NELE)
C      
C
C  =====================================================================
C     Perform Delaunay regularization on a given mesh
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]      
      integer MAXNODE,MAXNXE,MAXELE
      real    XY(2,MAXNODE)
      integer ELE(0:MAXNXE,MAXELE),ELAB(MAXELE)             
      integer SPIG(0:MAXNXE,MAXELE),NELE,NELETYPE
C     [LOCAL]
      integer i,j,k,nearE(3),NearON(3),nnear,Local(3),ntoswap
      real    x(3),y(3)
      real    xc,yc,det,R,Rmin
      integer nn1(6),nn2(6),ns1(3),ns2(3),ntoken1,ntoken2 
C     [EXTERNAL FUNCTIONS]
      integer NcircIdx
      
      do i=1,Nele        
        NeleType = ele(0,i)
        if (NeleType.eq.3) then
          local(1)=1
          local(2)=2
          local(3)=3
        else if (NeleType.eq.6) then
          local(1)=1
          local(2)=4
          local(3)=6
        else 
          NeleType=0  
        endif
        
        if (NeleType .gt. 0 ) then    
          do j=1,3
            x(j)=XY(1,ELE(local(j),i))
            y(j)=XY(2,ELE(local(j),i))
          enddo        

c     ---------- Calculation of the Center and Radius      
          det= 2*((x(2)-x(1))*(y(3)-y(1))-(y(2)-y(1))*(x(3)-x(1)))
          xc = ((x(2)**2+y(2)**2-x(1)**2-y(1)**2)*(y(3)-y(1))-
     &          (y(2)-y(1))*(x(3)**2+y(3)**2-x(1)**2-y(1)**2))/det
          yc = ((x(3)**2+y(3)**2-x(1)**2-y(1)**2)*(x(2)-x(1))-
     &          (x(3)-x(1))*(x(2)**2+y(2)**2-x(1)**2-y(1)**2))/det
          R  = sqrt((xc-x(1))**2+(yc-y(1))**2)
 
c     ---------- Quest for the Adiacent Elements      
          call FindAdEle (MAXNXE,MAXELE,ELE,NELE,
     &                    local,i,NEARE,NEARON,nnear)

c     ---------- Verification of Delaunay criterium     
          rmin=R-0.01
          ntoswap=0
          do j=1,nnear          
            if (sqrt((xc-xy(1,ele(local(nearon(j)),neare(j))))**2+
     &         (yc-xy(2,ele(local(nearon(j)),neare(j))))**2).lt.rmin) 
     &         then
              rmin = sqrt((xc-xy(1,ele(local(nearon(j)),neare(j))))**2+
     &               (yc-xy(2,ele(local(nearon(j)),neare(j))))**2)
              ntoswap=j
            endif
          enddo

c     ---------- Side swapping                  
          if (ntoswap.ne.0 .and. elab(i).eq.elab(neare(ntoswap))) then
            if (neletype.eq.3) then
              do j=1,3
                nn1(j) = ele(j,i) 
                nn2(j) = ele(j,neare(ntoswap))
                ns1(j) = spig(j,i) 
                ns2(j) = spig(j,neare(ntoswap))
              enddo
              ntoken2 = NcircIdx(nearon(ntoswap),3,-1)
              do j=3,1,-1
                if (nn1(j).eq.nn2(ntoken2)) then
                  ntoken1 = j
                endif  
              enddo                      
              ele(1,i)=nn1(NcircIdx(ntoken1,3,-1))
              ele(2,i)=nn2(NcircIdx(ntoken2,3,+1))
              ele(3,i)=nn1(NcircIdx(ntoken1,3,+1))
              ele(1,neare(ntoswap))=nn1(NcircIdx(ntoken1,3,-1))
              ele(2,neare(ntoswap))=nn2(ntoken2)
              ele(3,neare(ntoswap))=nn2(NcircIdx(ntoken2,3,+1))
              spig(1,i)=ns1(ntoken1)
              spig(2,i)=ns2(NcircIdx(ntoken2,3,+1))
              spig(3,i)=ns1(NcircIdx(ntoken1,3,+1))
              spig(1,neare(ntoswap))=ns1(NcircIdx(ntoken1,3,-1))
              spig(2,neare(ntoswap))=ns2(ntoken2)
              spig(3,neare(ntoswap))=ns2(NcircIdx(ntoken2,3,-1))
            else
              nn1(1) = ele(1,i) 
              nn1(2) = ele(2,i) 
              nn1(3) = ele(4,i) 
              nn1(4) = ele(5,i) 
              nn1(5) = ele(6,i) 
              nn1(6) = ele(3,i) 
              nn2(1) = ele(1,neare(ntoswap))
              nn2(2) = ele(2,neare(ntoswap))
              nn2(3) = ele(4,neare(ntoswap))
              nn2(4) = ele(5,neare(ntoswap))
              nn2(5) = ele(6,neare(ntoswap))
              nn2(6) = ele(3,neare(ntoswap))
              ntoken2 = NcircIdx(nearon(ntoswap),6,-1)
              do j=6,1,-1
                if (nn1(j).eq.nn2(ntoken2)) then
                  ntoken1 = j
                endif  
              enddo                      
              ele(1,i)=nn1(NcircIdx(ntoken1,6,-2))
              ele(2,i)=nn1(NcircIdx(ntoken1,6,+1))
              ele(3,i)=nn1(NcircIdx(ntoken1,6,+3))
              ele(4,i)=nn2(NcircIdx(ntoken2,6,+2))
              ele(5,i)=nn2(NcircIdx(ntoken2,6,+3))
              ele(6,i)=nn1(NcircIdx(ntoken1,6,+2))
              ele(1,neare(ntoswap))=nn1(NcircIdx(ntoken1,6,-2))
              ele(2,neare(ntoswap))=nn1(NcircIdx(ntoken1,6,-1))
              ele(3,neare(ntoswap))=nn1(NcircIdx(ntoken1,6,+1))
              ele(4,neare(ntoswap))=nn1(ntoken1)
              ele(5,neare(ntoswap))=nn2(NcircIdx(ntoken2,6,+1))
              ele(6,neare(ntoswap))=nn2(NcircIdx(ntoken2,6,+2))
c     ----------Move center node
              xy(1,nn1(NcircIdx(ntoken1,6,+1))) = 
     &          (xy(1,nn1(NcircIdx(ntoken1,6,-2))) +
     &           xy(1,nn2(NcircIdx(ntoken2,6,+2))) )/2.0 
              xy(2,nn1(NcircIdx(ntoken1,6,+1))) = 
     &          (xy(2,nn1(NcircIdx(ntoken1,6,-2))) +
     &           xy(2,nn2(NcircIdx(ntoken2,6,+2))) )/2.0
            endif
          endif                
        endif
      enddo
      return
      end  
      
C  =====================================================================
C
      Subroutine FindAdEle (MAXNXE,MAXELE,ELE,NELE,
     &                      local,NELENO,NEARE,NEARON,NFOUND)
C
C  =====================================================================
C     Finds Adjoining elements to a given element
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]      
      integer MAXNXE,MAXELE
      integer ELE(0:MAXNXE,MAXELE),local(3)
      integer NELE,NELENO         
C     [OUT]      
      integer nearE(3),NearON(3)               
C     [LOCAL]
      integer i,j,k,nfound,nc,non
      
      nfound=0                              
      do i=1,nele          
        if (i.ne.neleno) then
          nc=0          
          non=0
          do j=1,3                 
            do k=1,3
              if (ele(local(j),i).eq.ele(local(k),neleno)) then 
                nc=nc+1
                non=non+j      
              endif  
            enddo
          enddo
          if (nc.eq.2) then
            nfound=nfound+1
            nearE(nfound)=i
            nearon(nfound)=6-non
          endif
        endif
      enddo
      return
      end        
      
C  =====================================================================
C  
      Integer function NcircIdx (n,mo,ns)
C      
C  =====================================================================
C     circulates indices
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer n,ns,mo,nc
      
      nc = n+ns
      if (nc.gt.mo) nc=nc-mo
      if (nc.lt.1) nc=nc+mo
      NcircIdx=nc
      return
      end      
