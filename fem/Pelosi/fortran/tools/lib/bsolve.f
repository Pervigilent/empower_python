c =========================================================================== 
c 
      SUBROUTINE BLOCK_SOLVER(A, M, B, N, C, D, MD, ND, kl, ku, I1, I2, 
     &                        NNODE, NCYL, iCYL, Narm, 
     &                        V1, V2, flag ) 
c 
c =========================================================================== 
c Solution of the linear system of equations  
c   
c     |A  B| |v1|   |i1|      with: D banded matrix, 
c     |    | |  | = |  |            B & C sparse matrices, 
c     |C  D| |v2|   |i2|            A diagonal matrix 
c                                          
c     if FLAG = 0 then only V1 is computed, 
C             = 1 then both V1 and V2 are computed 
c  
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI 
c =========================================================================== 
      implicit none         
      integer    MAXI,MAXJ 
      parameter (MAXI = 1200, 
     &           MAXJ = 200 ) 
       
C     [IN]                  
      integer M,N,MD,ND !Matrix dimensions 
      complex A(M,M)    !Full Matrix 
      complex B(M,N)    !Full Matrix 
      complex C(N,M)    !Full Matrix 
      complex D(MD,ND)  !Band Matrix 
      integer kl,ku     !Banwidths 
      complex I1(M)     !Right hand side Dirichelet 
      complex I2(N)     !Right hand side Neumann 
       
      integer NNODE     !TOTAL Number of unknowns 
      integer ICYL      !Number of nodes on the matching contour 
      integer Narm      !Number of unknowns in A (modes) 
      integer ncyl(icyl)  
       
      integer FLAG      !Computes only V1 (FLAG=0) or V1 and V2 (FLAG=1) 
       
C     [OUT]       
      complex V1(m),v2(nnode) !Solution 
       
C     [LOCAL]                                        
      integer IPVT(MAXI) 
      complex X(MAXI,MAXJ) 
      complex E(MAXI,MAXJ) 
      integer i,j,k,infod,infos,info 
      complex sumc 
                     
      IF (nnode .GT. maxi) THEN 
        write (*,*) '***ERROR*** MAXI should be at least ',nnode         
        stop 
      endif 
       
      IF (icyl .GT. maxj) THEN 
        write (*,*) '***ERROR*** MAXJ should be at least ',icyl     
        stop 
      endif 
         
c  --- LU factorization  
      write(*,*)'  |   LU factorization of the banded FEM matrix' 
       
      call CGBTRF(NNODE,NNODE,Kl,Ku,D,MD, 
     &            IPVT,INFOD) 
      IF(INFOD .NE. 0) THEN 
        WRITE(*,*) '***ERROR*** in LU factorization, INFOD =', INFOD 
        STOP 
      ENDIF 
 
c  --- computing the inverse of the FEM matrix without refinement 
      write(*,*) '  |   Computing ',icyl, 
     &           ' columns of the inverse matrix' 
 
c  --- computing the wanted columns of the inverse of the FEM matrix 
      do i=1,nnode 
        do j=1,icyl 
          x(i,j) = (0.,0.) 
        enddo 
      enddo 
           
      do i=1,icyl 
        x(ncyl(i),i) = (1.,0.) 
      enddo 
 
      call CGBTRS('N', NNODE, kl, ku, icyl, D, 
     &            MD, IPVT, X, MAXI, INFOS) 
      IF(INFOS .NE. 0) THEN 
        WRITE(*,*) ' INFOS =', INFOS 
        STOP 
      ENDIF 
 
c     ---------- Computing V1 
c     E = D^(-1) * C 
      if (FLAG.eq.0) then  
        do i=1,icyl 
          do j=1,Narm 
            sumc = (0.,0.) 
            do k=1,icyl 
              sumc = sumc + x(ncyl(i),k) * c(k,j) 
            enddo 
            e(i,j) = sumc 
          enddo 
        enddo 
      else 
        do i=1,nnode 
          do j=1,Narm 
            sumc = (0.,0.) 
            do k=1,icyl 
              sumc = sumc + x(i,k) * c(k,j) 
            enddo 
            e(i,j) = sumc 
          enddo 
        enddo 
      endif 
 
c     ----------Computing  A' = A - B * E 
      if (FLAG.eq.0) then 
        do i=1,Narm 
          do j=1,Narm 
            sumc = (0.,0.) 
            do k=1,icyl 
              sumc = sumc + b(i,k) * e(k,j) 
            enddo 
            a(i,j) = a(i,j) - sumc 
          enddo 
        enddo 
      else 
        do i=1,Narm 
          do j=1,Narm 
            sumc = (0.,0.) 
            do k=1,icyl 
              sumc = sumc + b(i,k) * e(ncyl(k),j) 
            enddo 
            a(i,j) = a(i,j) - sumc 
          enddo 
        enddo 
      endif 
           
c  --- Computing V2 = D^(-1) * I2 
      if (FLAG.eq.0) then 
        do i=1,icyl 
          sumc = (0.,0.) 
          do k=1,icyl 
            sumc = sumc + x(ncyl(i),k) * i2(k) 
          enddo 
          V2(i) = sumc 
        enddo 
      else 
        do i=1,nnode 
          sumc = (0.,0.) 
          do k=1,icyl 
            sumc = sumc + x(i,k) * i2(k) 
          enddo 
          V2(i) = sumc 
        enddo 
      endif 
 
c  --- Computing  I1' = I1 - B * V2 
      if (FLAG.eq.0) then 
        do i=1,Narm 
          sumc = (0.,0.) 
          do k=1,icyl 
            sumc = sumc + b(i,k) * V2(k) 
          enddo 
          i1(i) = i1(i) - sumc 
        enddo 
      else 
        do i=1,Narm 
          sumc = (0.,0.) 
          do k=1,icyl 
            sumc = sumc + b(i,k) * V2(ncyl(k)) 
          enddo 
          i1(i) = i1(i) - sumc 
        enddo 
      endif 
 
      call cgesv(Narm,1,A,M,IPVT,I1,M,INFO) 
      IF(INFO .NE. 0) THEN 
        WRITE(*,*) '***ERROR*** in solving for V1, INFO =', INFO 
        STOP 
      ENDIF 
      do i=1,Narm 
        V1(i)=I1(i) 
      enddo         
 
C     ---------- Computing Interior Field V2       
      if (FLAG.eq.1) then 
        do i=1,nnode 
          sumc = (0.,0.) 
          do k=1,Narm 
            sumc = sumc + e(i,k) * V1(k) 
          enddo 
          V2(i) = V2(i) - sumc 
        enddo 
      endif   
 
      return 
      end 
 
