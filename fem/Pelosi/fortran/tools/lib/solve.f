C  =====================================================================
C
      subroutine GaussSolve(A,B,LDA,N,IERR,ku,kl,kd,UPLO)                                      
C
C  =====================================================================
C     Very simple gauss solutor for banded matrices with pivoting
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer LDA,N            
      real    A(LDA,N),B(N)
      INTEGER ku,kl,kd 
      CHARACTER*1 UPLO
C     [OUT]
      integer IERR      
C     [LOCAL]      
      INTEGER i,j,k
      REAL    eps,value,pivot,subt
C     [EXTERNAL FUNCTIONS]
      REAL    BGET

  
      eps=1.e-4
      ierr=0

c     ---------- Forward subtraction
      do i=1,N-1
        if (abs(BGET(A,i,i,ku,kl,kd,LDA,N,UPLO)).lt.eps) then
          ierr=i
          return
        else      
          pivot = BGET(A,i,i,ku,kl,kd,LDA,N,UPLO)
          if (kd.eq.0) then
            do j=1,kl        
              if (i+j.le.N) then
                value=BGET(A,i+j,i,ku,kl,kd,LDA,N,UPLO)/pivot
                call BPUT(0.,A,i+j,i,ku,kl,kd,LDA,N,UPLO)
                do k=1,ku
                  if (i+k.le.N) then
                    subt=BGET(A,i,i+k,ku,kl,kd,LDA,N,UPLO)*value
                    call BPUT(BGET(A,i+j,i+k,ku,kl,kd,LDA,N,UPLO)-subt,
     &                   A,i+j,i+k,ku,kl,kd,LDA,N,UPLO)
                  endif
                enddo
                B(i+j)=B(i+j)-B(i)*value
              endif  
            enddo
          else
            do j=1,kd           
              if (i+j.le.N) then
                value=BGET(A,i+j,i,ku,kl,kd,LDA,N,UPLO)/pivot
                call BPUT(0.,A,i+j,i,ku,kl,kd,LDA,N,UPLO)
                do k=1,kd
                  if (i+k.le.N) then
                    subt=BGET(A,i,i+k,ku,kl,kd,LDA,N,UPLO)*value
                    call BPUT(BGET(A,i+j,i+k,ku,kl,kd,LDA,N,UPLO)-subt,
     &                   A,i+j,i+k,ku,kl,kd,LDA,N,UPLO)
                  endif 
                enddo
                B(i+j)=B(i+j)-B(i)*value
              endif  
            enddo
          endif    
        endif
      enddo    

c     ---------- Backward subtraction
      do i=N,2,-1
        if (abs(BGET(A,i,i,ku,kl,kd,LDA,N,UPLO)).lt.eps) then
          ierr=i
          return
        else      
          pivot = BGET(A,i,i,ku,kl,kd,LDA,N,UPLO)
          if (kd.eq.0) then
            do j=1,ku 
              if (i-j.ge.1) then
                value=BGET(A,i-j,i,ku,kl,kd,LDA,N,UPLO)/pivot
                call BPUT(0.,A,i-j,i,ku,kl,kd,LDA,N,UPLO)
                B(i-j)=B(i-j)-B(i)*value
              endif  
            enddo
          else
            do j=1,kd           
              if (i-j.ge.1) then
                value=BGET(A,i-j,i,ku,kl,kd,LDA,N,UPLO)/pivot
                call BPUT(0.,A,i+j,i,ku,kl,kd,LDA,N,UPLO)
                B(i-j)=B(i-j)-B(i)*value
              endif  
            enddo
          endif    
        endif
      enddo    

      do i=1,N
        B(i) = B(i) / BGET(A,i,i,ku,kl,kd,LDA,N,UPLO)
      enddo
        
      RETURN

      END
      
      
