C  =====================================================================       
C 
      subroutine FindEqN (XY,MAXNODE,NNODE,PERX,EQN) 
C 
C  ===================================================================== 
C     Find Nodes which differs for a traslation alonx x given by PERX 
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI 
C  ===================================================================== 
C     [IN] 
      integer   MAXNODE, NNODE 
      real      XY(2,MAXNODE) 
      real      PERX 
 
C     [OUT] 
      integer   EQN(NNODE)   ! EqNode(j) = j if node j has no 
                             !             corresponding node 
                             ! EqEdge(j) = i if node j corresponds 
                             !             to node i 
C     [LOCAL] 
      logical*1   found 
      integer     i,j 
      real        TOL 
 
C     ---------- START 
      TOL = PERX * 1.E-04 
      do i=1,nnode-1 
        j=i 
        found = .FALSE. 
        do while (.NOT. Found .AND. j .lt. nnode ) 
          j = j+1	     
          if ( abs((XY(1,i)-XY(1,j))-PERX) .lt. TOL .and.
     &         abs((XY(2,i)-XY(2,j))) .lt. TOL) then 
            EQN(i) = j 
            found = .TRUE. 
          else if (abs((XY(1,j)-XY(1,i))-PERX) .lt. TOL .and.
     &         abs((XY(2,i)-XY(2,j))) .lt. TOL) then 
            EQN(j) = i 
            found = .TRUE. 
          endif 
        enddo 
      enddo 
      return 
      end 
 
