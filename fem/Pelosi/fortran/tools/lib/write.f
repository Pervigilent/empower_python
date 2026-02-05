C  =====================================================================
C
      Subroutine WriteMesh  (foutname,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                       XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                       NNODE,NELE,NSPIG)
C      
C  =====================================================================
C     writes a given mesh onto a named file
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]      
      character*256 foutname      
      integer MAXNODE,MAXNXE,MAXELE,MAXSPIG
      real    XY(2,MAXNODE)
      integer ELE(0:MAXNXE,MAXELE),NLAB(MAXNODE),ELAB(MAXELE)             
      integer SPIG(0:MAXNXE,MAXELE),SLAB(MAXSPIG),NSPIG
      integer NNODE,NELE
C     [LOCAL]      
      integer i,j

C     -------- Open File      
      open (UNIT=1,ACCESS='SEQUENTIAL', ERR=2000, 
     &      FILE=foutname, FORM='FORMATTED')           
C     -------- Writing Data
      write (1,*,err=2001) 'Mesh Geometry file'
      write (1,'(a11,I6)',err=2001) 'Elements = ',nele
      write (1,'(a8,I6)',err=2001) 'Nodes = ',nnode
      write (1,'(a8,I6)',err=2001) 'Edges = ',nspig
      write (1,*,err=2002) 'Elements'
      do i=1,nele        
        write (1,'(11I6)',err=2002) i, (ELE(j,i),j=0,ELE(0,i)),elab(i)
      enddo
      write (1,*,err=2003) 'Nodes'  
      do i=1,nnode
        write (1,'(I6,1X,f13.6,1X,f13.6,1X,I6)',err=2003) 
     &         i, xy(1,i), xy(2,i), nlab(i)
      enddo                                              
      if (nspig.gt.0) then
        write (1,*,err=2004) 'Edges'  
        do i=1,nele
          write (1,'(10I6)',err=2004) i,(SPIG(j,i),j=0,SPIG(0,i))
        enddo                                              
        write (1,*,err=2005) 'Edge Labels'  
        do i=1,nspig
          write (1,'(2I6)',err=2005) i,SLAB(i)
        enddo                                              
      endif    
      goto 2010
2000  write(*,*) '****ERROR****'
      write(*,*) 'Cannot open file ', foutname
      stop
2001  write(*,*) '****ERROR****'
      write(*,*) 'Error writing header of file ', foutname
      stop
2002  write(*,*) '****ERROR****'
      write(*,*) 'Error writing Elements Block in file ', foutname
      stop
2003  write(*,*) '****ERROR****'
      write(*,*) 'Error writing Nodes Block in file ', foutname
      STOP
2004  write(*,*) '****ERROR****'
      write(*,*) 'Error reading Edges Block in file ', foutname
      STOP
2005  write(*,*) '****ERROR****'
      write(*,*) 'Error reading Edge Labels Block in file ', foutname
      STOP
2010  close(1)
      return
      end
        
