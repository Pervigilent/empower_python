C  =====================================================================
C
      integer function nBANDEX (MAXELE,MAXNXE,ELE,NELE)
C
C  =====================================================================
C     Evaluates band occupation for a given mesh
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer MAXNXE,MAXELE,NELE
      integer ELE(0:MAXNXE,MAXELE)
      integer i,j,k,ib
      
      ib = 0
      do i=1,NELE
        do j=1,ele(0,i)
          do k=1,ele(0,i)
            if (abs(abs(ele(j,i))-abs(ele(k,i))).gt.ib)
     &        ib = abs(abs(ele(j,i))-abs(ele(k,i)))
          enddo
        enddo
      enddo
      nbandex=ib
      return
      end
