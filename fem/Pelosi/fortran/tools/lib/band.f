C  =====================================================================
C
      SUBROUTINE BPUT (val,A,i,j,ku,kl,kd,LDA,N,UPLO)
C
C  =====================================================================
C     Sets an entry in a band storage mode matrix
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      REAL val             !Value to be stored
      INTEGER i,j          !Indexes in A
      INTEGER ku,kl        !Superdiagonal and Subdiagonal
                           !for Band matrices
      INTEGER kd           !Superdiagonal and Subdiagonal
                           !for symmetric band matrices
                           !Must be zero for nonsymmetric                           
      INTEGER LDA,N        !Dimensions of the array A                           
      CHARACTER*1 UPLO     !For symmetric band storage only:
                           !'U' for Upper Diagonals
                           !'L' for Lower Diagonals
C     [IN/OUT]      
      REAL A(LDA,N)        !Storage array of Matrix A
                           
      IF (kd.eq.0) THEN
c     ---------- Non Symmetric
        IF ((i.ge.1) .and. (i.ge.(j-ku)) .and.
     &      (i.le.N) .and. (i.le.(j+kl))) THEN
          A(ku+kl+1+i-j,j) = val
        ELSE
          WRITE (*,*) '**** ERROR in Band storage at i=',i,' j=',j
        ENDIF
      ELSE
c     ---------- Symmetric  
        IF (UPLO .eq. 'U') THEN
          IF ((i.ge.1) .and. (i.ge.(j-kd)) .and. (i.le.j)) THEN
            A(kd+1+i-j,j) = val
          ELSE
            WRITE (*,*) '**** ERROR in Symmetric U Band',
     &                  ' storage at i=',i,' j=',j
          ENDIF                           
        ELSE  
          IF ((i.ge.j) .and. (i.le.n) .and. (i.le.(j+kd))) THEN
            A(1+i-j,j) = val
          ELSE
            WRITE (*,*) '**** ERROR in Symmetric L Band',
     &                  ' storage at i=',i,' j=',j
          ENDIF                           
        ENDIF
      ENDIF

      RETURN
      END
      
      
C  =====================================================================
C
      REAL FUNCTION BGET (A,i,j,ku,kl,kd,LDA,N,UPLO)
C
C  =====================================================================
C     Gets an entry in a band storage mode matrix
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      INTEGER LDA,N        !Dimensions of the array A 
      REAL A(LDA,N)        !Storage array of Matrix A
                        
      INTEGER i,j          !Indexes in A
      INTEGER ku,kl        !Superdiagonal and Subdiagonal
                           !for Band matrices
      INTEGER kd           !Superdiagonal and Subdiagonal
                           !for symmetric band matrices
                           !Must be zero for nonsymmetric                           
      CHARACTER*1 UPLO     !For symmetric band storage only:
                           !'U' for Upper Diagonals
                           !'L' for Lower Diagonals
                           
      IF (kd.eq.0) THEN
c     ---------- Non Symmetric
        IF ((i.ge.1) .and. (i.ge.(j-ku)) .and.
     &      (i.le.N) .and. (i.le.(j+kl))) THEN
          BGET = A(ku+kl+1+i-j,j)
        ELSE
          WRITE (*,*) '**** ERROR in Band storage at i=',i,' j=',j
          BGET = 0.0
        ENDIF
      ELSE  
c     ---------- Symmetric
        IF (UPLO .eq. 'U') THEN
          IF ((i.ge.1) .and. (i.ge.(j-kd)) .and. (i.le.j)) THEN
            BGET = A(kd+1+i-j,j)
          ELSE
            WRITE (*,*) '**** ERROR in Symmetric U Band',
     &                  ' storage at i=',i,' j=',j
            BGET = 0.0
          ENDIF                           
        ELSE  
          IF ((i.ge.j) .and. (i.le.n) .and. (i.le.(j+kd))) THEN
            BGET = A(1+i-j,j)
          ELSE
            WRITE (*,*) '**** ERROR in Symmetric L Band',
     &                  ' storage at i=',i,' j=',j  
            BGET = 0.0
          ENDIF                           
        ENDIF
      ENDIF        
      
      RETURN
      END

C  =====================================================================
C
      SUBROUTINE CBPUT (val,A,i,j,ku,kl,kd,LDA,N,UPLO)
C
C  =====================================================================
C     Complex version of BPUT
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      COMPLEX val             !Value to be stored
      
      INTEGER i,j          !Indexes in A
      INTEGER ku,kl        !Superdiagonal and Subdiagonal
                           !for Band matrices
      INTEGER kd           !Superdiagonal and Subdiagonal
                           !for symmetric band matrices
                           !Must be zero for nonsymmetric                           
      INTEGER LDA,N        !Dimensions of the array A                           
      CHARACTER*1 UPLO     !For symmetric band storage only:
                           !'U' for Upper Diagonals
                           !'L' for Lower Diagonals
C     [IN/OUT]                           
      COMPLEX A(LDA,N)     !Storage array of Matrix A                        
                           
      IF (kd.eq.0) THEN
c     ---------- Non Symmetric
        IF ((i.ge.1) .and. (i.ge.(j-ku)) .and.
     &      (i.le.N) .and. (i.le.(j+kl))) THEN
          A(ku+kl+1+i-j,j) = val
        ELSE
          WRITE (*,*) '**** ERROR in Band storage at i=',i,' j=',j
        ENDIF
      ELSE
c     ---------- Symmetric  
        IF (UPLO .eq. 'U') THEN
          IF ((i.ge.1) .and. (i.ge.(j-kd)) .and. (i.le.j)) THEN
            A(kd+1+i-j,j) = val
          ELSE
            WRITE (*,*) '**** ERROR in Symmetric U Band',
     &                  ' storage at i=',i,' j=',j
          ENDIF                           
        ELSE  
          IF ((i.ge.j) .and. (i.le.n) .and. (i.le.(j+kd))) THEN
            A(1+i-j,j) = val
          ELSE
            WRITE (*,*) '**** ERROR in Symmetric L Band',
     &                  ' storage at i=',i,' j=',j
          ENDIF                           
        ENDIF
      ENDIF

      RETURN
      END
      
      
C  =====================================================================
C
      COMPLEX FUNCTION CBGET (A,i,j,ku,kl,kd,LDA,N,UPLO)
C
C  =====================================================================
C     Complex version of BGET
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      INTEGER LDA,N        !Dimensions of the array A
      COMPLEX A(LDA,N)     !Storage array of Matrix A
                        
      INTEGER i,j          !Indexes in A
      INTEGER ku,kl        !Superdiagonal and Subdiagonal
                           !for Band matrices
      INTEGER kd           !Superdiagonal and Subdiagonal
                           !for symmetric band matrices
                           !Must be zero for nonsymmetric                           
      CHARACTER*1 UPLO     !For symmetric band storage only:
                           !'U' for Upper Diagonals
                           !'L' for Lower Diagonals
                           
      IF (kd.eq.0) THEN
c     ---------- Non Symmetric
        IF ((i.ge.1) .and. (i.ge.(j-ku)) .and.
     &      (i.le.N) .and. (i.le.(j+kl))) THEN
          CBGET = A(ku+kl+1+i-j,j)
        ELSE
          WRITE (*,*) '**** ERROR in Band storage at i=',i,' j=',j
          CBGET = (0.0,0.0)
        ENDIF
      ELSE  
c     ---------- Symmetric
        IF (UPLO .eq. 'U') THEN
          IF ((i.ge.1) .and. (i.ge.(j-kd)) .and. (i.le.j)) THEN
            CBGET = A(kd+1+i-j,j)
          ELSE
            WRITE (*,*) '**** ERROR in Symmetric U Band',
     &                  ' storage at i=',i,' j=',j
            CBGET = (0.0,0.0)
          ENDIF                           
        ELSE  
          IF ((i.ge.j) .and. (i.le.n) .and. (i.le.(j+kd))) THEN
            CBGET = A(1+i-j,j)
          ELSE
            WRITE (*,*) '**** ERROR in Symmetric L Band',
     &                  ' storage at i=',i,' j=',j  
            CBGET = (0.0,0.0)
          ENDIF                           
        ENDIF
      ENDIF        
      
      RETURN
      END
        
C  ===================================================================== 
C 
      INTEGER FUNCTION IROWBANDED(UPLO,I,J,KU,IR) 
C 
C  ===================================================================== 
C     Returns the row position of entry (i,j) in Band Storage Mode 
C     If irowbanded = 0, the entry must not be stored. 
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI 
C  ===================================================================== 
C     [IN] 
      CHARACTER*1 UPLO     !'U' Stores only Upper Triangle 
                           !'L' Stores only Lower Triangle 
                           !'T' Stores all matrix 
      INTEGER I,J          ! Indexes in A 
      INTEGER KU           ! Superdiagonal of the Banded matrix 
                           ! ku = kl for symmetric matrices 
      INTEGER IR           ! The Banded matrix is stored starting from 
                           ! row IR+1. 
                           ! When IR = KL, the storage scheme obtained 
                           ! complies with the LAPACK standard 
 
      IROWBANDED = 0 
      IF (UPLO .EQ. 'T' .OR. 
     &    UPLO .EQ. 'L' .AND. I .GE. J .OR. 
     &    UPLO .EQ. 'U' .AND. J .GE. I     )  
     &        IROWBANDED = IR + KU + 1 + I - J 
 
      RETURN 
      END 
