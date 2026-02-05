C  =====================================================================
C
      SUBROUTINE RENUMBER(MAXELE,MAXNXE,MAXNODE,MAXSPIG,
     &                    XY,ELE,SPIG,NNODE,NELE,NSPIG,ELAB,
     &                    NLAB,SLAB,THRE,EQN,NPERMUT,EPERMUT,SPERMUT)
C
C  =====================================================================
C     Renumber elements, nodes and edges so to provide minimum bandwidth
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
C     [IN]
      integer MAXELE,MAXNXE,MAXNODE,MAXSPIG 
      integer ELE(0:MAXNXE,MAXELE),SPIG(0:MAXNXE,MAXELE)   
      integer NLAB(MAXNODE),ELAB(MAXELE),SLAB(MAXSPIG)
      integer NNODE,NELE,NSPIG,EQN(NNODE),THRE
      real    XY(2,MAXNODE)
C     [OUT]                                      
      integer NPERMUT(MAXNODE),EPERMUT(MAXELE),SPERMUT(MAXSPIG)
C     [LOCAL]      
      integer    MN, ME, MS, MNEL
      parameter  (MN=3000, ME=4000, MS=6000, MNEL= 8)
      integer    iepermut(ME),inpermut(MN),ispermut(MS)
      logical    uele(ME),unod(MN),uspi(MS)
      logical    isadiacent
      integer    ELEAUX(0:MNEL),SPIAUX(0:MNEL),laux,lsaux
      integer    felefree,lelefree
      integer    inotfree,inaux,ifirst
      integer    i,j,iadj
      logical    toolarge
      real       ax,bx
      
	toolarge = .FALSE.
      if (MN.lt.NNODE) then
        print * ,'****ERROR****'
        print * ,'Parameter MN in Routine RENUMBER.F '//
     &           'must be at least ',NNODE
        toolarge = .TRUE.
      endif

      if (ME.lt.NELE) then
        print * ,'****ERROR****'
        print * ,'Parameter ME Routine RENUMBER.F '//
     &           'must be at least ',NELE
        toolarge = .TRUE.
      endif
      
      if (MS.lt.NSPIG) then
        print * ,'****ERROR****'
        print * ,'Parameter MS Routine RENUMBER.F '//
     &           'must be at least ',NSPIG
        toolarge = .TRUE.
      endif
      if (toolarge) stop
      
      ifirst = 1
c     ---------- renumber elements
      do i=1,nele
        uele(i)    = .TRUE.
        epermut(i) = 0
      enddo

      do j=1,nnode
       unod(j)    = .TRUE.
       npermut(j) = 0
      enddo

      do j=1,nspig
       uspi(j)    = .TRUE.
       spermut(j) = 0
      enddo

      epermut(1)   = ifirst
      uele(ifirst) = .FALSE.
      inotfree     = 0
      felefree     = 1  
      lelefree     = 1
      iadj         = 1

      do while (iadj.gt.0)
        iadj = 0
        do j=felefree,lelefree
          do i=1,nele
            if (uele(i)) then
              if(isadiacent(i,epermut(j),ele,MAXELE,
     &                         MAXNXE,THRE,EQN,NNODE)) then
                iadj = iadj + 1
                epermut(lelefree+iadj) = i
                uele(i) = .FALSE.
              endif
            endif
          enddo
        enddo
      
        felefree = lelefree + 1 
        lelefree = lelefree + iadj
      
      enddo
c---------- renumber nodes
      do i=1,nele
        do j=1,ele(0,epermut(i))
          inaux = ele(j,epermut(i))
          if (unod(inaux)) then
            inotfree          = inotfree + 1
            npermut(inotfree) = inaux
            unod(inaux)       = .FALSE.
          endif
        enddo   
      enddo

c---------- renumber edges
      if (nspig.gt.0) then
        inotfree     = 0
        do i=1,nele
          do j=1,spig(0,epermut(i))
            inaux = abs(spig(j,epermut(i)))
            if (uspi(inaux)) then
              inotfree          = inotfree + 1
              spermut(inotfree) = inaux
              uspi(inaux)       = .FALSE.
            endif
          enddo   
        enddo
      endif
      
c---------- Inverse node permutation
      do i=1,nnode
        inpermut(npermut(i))=i
      enddo

c---------- Inverse element permutation 
      do i=1,nele 
	  iepermut(epermut(i))=i 
      enddo 
 
c---------- Inverse edge permutation
      if (nspig.gt.0) then
        do i=1,nspig
          ispermut(spermut(i))=i
        enddo
      endif
        
c---------- Rebuild ELE, ELAB, SPIG
      do i=1,nele       
        if (i.eq.epermut(i)) then
          do j=1,ele(0,i)
            ele(j,i) = inpermut(ele(j,i))
          enddo                          
          if (nspig.gt.0) then
            do j=1,spig(0,i)
              spig(j,i) = sign(1,spig(j,i))*ispermut(abs(spig(j,i)))
            enddo                          
          endif  
        else
          do j=0,MAXNXE
            eleaux(j) = ele(j,i)            
            spiaux(j) = spig(j,i)
          enddo
          laux = ELab(i)         
          ele(0,i)=ele(0,epermut(i))
          do j=1,ele(0,i)
            ele(j,i)=inpermut(ele(j,epermut(i)))
          enddo   
	    spig(0,i)=spig(0,epermut(i)) 
          do j=1,spig(0,i)
            spig(j,i)=sign(1,spig(j,epermut(i)))*
     &                ispermut(abs(spig(j,epermut(i))))
          enddo   
          ELab(i)=Elab(epermut(i))
          do j=0,eleaux(0)
            ele(j,epermut(i))=eleaux(j)
            spig(j,epermut(i))=spiaux(j)
          enddo         
          elab(epermut(i)) = laux
          do j=i,nele
            if (epermut(j).eq.i) epermut(j)=epermut(i)
          enddo    
        endif
      enddo
      do i=1,nele 
	  epermut(iepermut(i))=i 
      enddo 

C     ---------- Rebuild SLAB 
      do i=1,nspig 
	  if(i.ne.spermut(i))then 
	    lsaux   = Slab(i) 
	    Slab(i) = Slab(spermut(i)) 
	    Slab(spermut(i))=lsaux 
	    j =i 
	    do while (spermut(j).ne.i) 
	      j=j+1 
	    enddo 
	    spermut(j)=spermut(i) 
	  endif 
      enddo 
      do i=1,nspig 
	  spermut(ispermut(i))=i 
      enddo 
 
C     ---------- Rebuild XY, NLAB
      do i=1,nnode
        if (i.ne.npermut(i)) then
          ax=xy(1,i)
          bx=xy(2,i)
          laux=Nlab(i)
          xy(1,i)=xy(1,npermut(i))
          xy(2,i)=xy(2,npermut(i))
          NLab(i)=nlab(npermut(i))
          xy(1,npermut(i))=ax
          xy(2,npermut(i))=bx
          NLab(npermut(i))=laux
          do j=i,nnode
            if (npermut(j).eq.i) npermut(j)=npermut(i)
          enddo  
        endif
      enddo
      do i=1,nnode 
	  npermut(inpermut(i))=i 
      enddo 

      return
      end

C  =====================================================================
C
      LOGICAL FUNCTION ISADIACENT(ele1,ele2,ELE,MAXELE,
     &                                       MAXNXE,THRE,EQN,NNODE)
C
C  =====================================================================
C     Check for element adiacency
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      integer     MAXNXE,MAXELE,NNODE,ELE(0:MAXNXE,MAXELE),EQN(NNODE)
      integer     ele1,ele2,thre
      integer     ln1,ln2
      integer     i,j,sum

      ISADIACENT = .FALSE.  
      if (ele1 .eq. ele2) return
                                       
      ln1 = ele(0,ele1)
      ln2 = ele(0,ele2)
      
      sum=0
      do i=1,ln1
      do j=1,ln2
        if (EQN(ele(i,ele1)) .eq. EQN(ele(j,ele2))) then
          sum = sum+1
        endif
      enddo
      enddo

      ISADIACENT = (sum.ge.thre)

      end
