C  ==================================================================
C  
      Subroutine WriteCRes   (foutname,R,N,NTYPE,WHAT)
C      
C  ==================================================================
C     PERFORM WRITING OF THE FIELD RESULTS
C     DUMMY ARGUMENTS ARE DEFINED AS USUAL
C     QUICK_FEM (C) PELOSI - COCCIOLI - SELLERI
C  ==================================================================
C     [IN]      
      character*256 foutname
      character*1 WHAT             ! 'R' = Stores Real Part 
                                   ! 'I' = Stores Imaginary Part 
                                   ! 'M' = Stores Magnitude 
                                   ! 'P' = Stores Phase 
      integer N 
      integer NTYPE                ! Type 1 = NODAL, 2 = EDGE 
      complex R(N)
      
C     [LOCAL]      
      integer i

C     -------- Open File      
      open (UNIT=1,ACCESS='SEQUENTIAL', ERR=7000, 
     &      FILE=foutname, FORM='FORMATTED')           
C     -------- Writing Data
      write (1,'(a7,I5)',ERR=7001) 'Type = ',ntype 
      write (1,'(a9,I5)',ERR=7001) 'Number = ',n 
 
      if (What .eq. 'R') then 
        do i=1,n
          write (1,'(I5,f13.6)',err=7002) i, real(R(i))
        enddo
      else if (What .eq. 'I') then 
        do i=1,n 
          write (1,'(I5,f13.6)',err=7002) i, aimag(R(i)) 
        enddo 
      else if (What .eq. 'M') then 
        do i=1,n 
          write (1,'(I5,f13.6)',err=7002) i, abs(R(i)) 
        enddo 
      else if (What .eq. 'P') then 
        do i=1,n 
	    if ( abs(R(i)) .eq. 0. ) then 
            write (1,'(I5,f13.6)',err=7002) 
     &                          i, 0. 
          else 
            write (1,'(I5,f13.6)',err=7002) 
     &                          i, atan2d(aimag(R(i)),real(R(i))) 
          endif 
        enddo 
      endif 
      
      goto 7010
7000  write(*,*) '****ERROR****'
      write(*,*) 'Cannot open file ', foutname
      stop
7001  write(*,*) '****ERROR****'
      write(*,*) 'Error writing header of file ', foutname
      stop
7002  write(*,*) '****ERROR****'
      write(*,*) 'Error writing Results Block in file ', foutname
      stop
7010  close(1)
      return
      end
        
