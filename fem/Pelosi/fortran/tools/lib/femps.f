c  ==================================================================
c       
      subroutine WritePS (fname,MAXNODE,MAXNXE,MAXELE,MAXSPIG,
     &                    XY,ELE,SPIG,NLAB,ELAB,SLAB,
     &                    NNODE,NELE,FLAGS)              
c
c  ==================================================================
C     Converts a given mesh in a PostScript File ready for printing
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
c     [IN]    
      integer MAXELE,MAXNXE,MAXNODE,MAXSPIG
      real    XY(2,MAXNODE)
      integer ELE(0:MAXNXE,MAXELE),SPIG(0:MAXNXE,MAXELE)   
      integer NLAB(MAXNODE),ELAB(MAXELE),SLAB(MAXSPIG)
      integer NNODE,NELE
      character*256 fname                       
      logical FLAGS(6)
      
c     [LOCAL]
      integer i,j      
      real    exy(9,2)
      real    offx,offy,scale
      real    maxx,minx,maxy,miny
      
C     -------- Open File      
      open (UNIT=1,ACCESS='SEQUENTIAL', 
     &      FILE=fname, FORM='FORMATTED')           
             
      write (1,*) '%!PS-Adobe-2.0'
      write (1,*) '%%Creator: FEMPS'
      write (1,*) '%%DocumentFonts: Helvetica'
      write (1,*) '%%BoundingBox: 50 50 554 770'
      write (1,*) '%%Pages: (atend)'
      write (1,*) '%%EndComments'
      write (1,*) '/FEMPSdict 40 dict def'
      write (1,*) 'FEMPSdict begin'
      write (1,*) '/Color false def'
      write (1,*) '/Solid false def'
      write (1,*) '/FEMPSlinewidth 1.000 def'
      write (1,*) '/dl {10 mul} def'
      write (1,*) '/M {moveto} bind def'
      write (1,*) '/L {lineto} bind def'
      write (1,*) '/R {rmoveto} bind def'
      write (1,*) '/V {rlineto} bind def'
      write (1,*) 
     &      '/DL { Color {setrgbcolor Solid {pop []} if 0 setdash }'
      write (1,*)
     &      '{pop pop pop Solid {pop []} if 0 setdash} ifelse } def'
      write (1,*) '/BL { stroke FEMPSlinewidth 2 mul setlinewidth } def'
      write (1,*) '/AL { stroke FEMPSlinewidth 2 div setlinewidth } def'
      write (1,*) '/PL { stroke FEMPSlinewidth setlinewidth } def'
      write (1,*) '/LT { BL [] 0 0 0 DL } def'
      write (1,*) 'end'
      write (1,*) '%%EndProlog'
      write (1,*) '%%Page: 1 1'
      write (1,*) 'FEMPSdict begin'
      write (1,*) 'gsave'
      write (1,*) '50 50 translate'
      write (1,*) '0.100 0.100 scale'
      write (1,*) '0 setgray'
      write (1,*) '/Helvetica findfont 80 scalefont setfont'
      write (1,*) 'newpath'
      write (1,*) 'LT'

C     ---------- Determination of scaling factors      
      maxx=XY(1,1)
      minx=XY(1,1)
      maxy=XY(2,1)
      miny=XY(2,1)
      
      do i=1,NNODE
        if (XY(1,i).gt.maxx) maxx=XY(1,i)
        if (XY(1,i).lt.minx) minx=XY(1,i)
        if (XY(2,i).gt.maxy) maxy=XY(2,i)
        if (XY(2,i).lt.miny) miny=XY(2,i)
      enddo
      
      offx=-minx  
      offy=-miny
      if (abs(maxx-minx)/5. .gt. abs(maxy-miny)/7.) then
        scale = 5000.0/abs(maxx-minx)
      else
        scale = 7000.0/abs(maxy-miny)
      endif
          
C     ---------- Line Drawing      
      do i=1,NELE  
        if (ELE(0,i).ne.6) then
          do j=1,ELE(0,i)
            exy(j,1)=xy(1,ele(j,i))
            exy(j,2)=xy(2,ele(j,i))
          enddo
          exy(ELE(0,i)+1,1)=xy(1,ele(1,i))
          exy(ELE(0,i)+1,2)=xy(2,ele(1,i))
        else
          exy(1,1)=xy(1,ele(1,i))
          exy(1,2)=xy(2,ele(1,i))
          exy(2,1)=xy(1,ele(2,i))
          exy(2,2)=xy(2,ele(2,i))
          exy(3,1)=xy(1,ele(4,i))
          exy(3,2)=xy(2,ele(4,i))
          exy(4,1)=xy(1,ele(5,i))
          exy(4,2)=xy(2,ele(5,i))
          exy(5,1)=xy(1,ele(6,i))
          exy(5,2)=xy(2,ele(6,i))
          exy(6,1)=xy(1,ele(3,i))
          exy(6,2)=xy(2,ele(3,i))
          exy(7,1)=xy(1,ele(1,i))
          exy(7,2)=xy(2,ele(1,i))
        endif
               
        do j=1,ELE(0,i)+1
          exy(j,1)=(exy(j,1)+offx)*scale
          exy(j,2)=(exy(j,2)+offy)*scale
        enddo
        
        write (1,'(f6.1,1x,f6.1,1x,a1)') exy(1,1),exy(1,2),'M'
        do j=2,ELE(0,i)+1
          write (1,'(f6.1,1x,f6.1,1x,a1)') exy(j,1),exy(j,2),'L'
        enddo
      enddo
      
C     ---------- Element numbers and labels
      do i=1,NELE                          
        exy(1,1)=0.0
        exy(1,2)=0.0
        do j=1,ELE(0,i)
          exy(1,1)=exy(1,1)+xy(1,ele(j,i))
          exy(1,2)=exy(1,2)+xy(2,ele(j,i))
        enddo
                
        exy(1,1)=(exy(1,1)/ELE(0,i)+offx)*scale
        exy(1,2)=(exy(1,2)/ELE(0,i)+offy)*scale
        
        if (flags(1)) then
          write (1,'(f6.1,1x,f6.1,1x,a1)') exy(1,1),exy(1,2),'M'
          write (1,'(a3,I4,a21)') '0 (',i,') stringwidth pop add'   
          write (1,'(a1,I4,a6)') '(',i,') show'
        endif
        if (flags(2)) then
          write (1,'(f6.1,1x,f6.1,1x,a1)') exy(1,1),exy(1,2)-85,'M'
          write (1,'(a3,I4,a21)') '0 (',ELAB(i),') stringwidth pop add'   
          write (1,'(a1,I4,a6)') '(',ELAB(i),') show'
        endif
      enddo

c     ---------- Node Numbers and Labels      
      do i=1,NNODE
        exy(1,1)=(xy(1,i)+offx)*scale
        exy(1,2)=(xy(2,i)+offy)*scale
        if (flags(3)) then
          write (1,'(f6.1,1x,f6.1,1x,a1)') exy(1,1),exy(1,2),'M'
          write (1,'(a3,I4,a21)') '0 (',i,') stringwidth pop add'   
          write (1,'(a1,I4,a6)') '(',i,') show'
        endif
        if (flags(4)) then
          write (1,'(f6.1,1x,f6.1,1x,a1)') exy(1,1),exy(1,2)-85,'M'
          write (1,'(a3,I4,a21)') '0 (',NLAB(i),') stringwidth pop add'   
          write (1,'(a1,I4,a6)') '(',NLAB(i),') show'
        endif                         
      enddo  
        
C     ---------- Edge numbers and labels
      do i=1,NELE                       
        if (ELE(0,i).lt.5) then   
          exy(1,1)=0.0
          exy(1,2)=0.0
          do j=1,ELE(0,i)
            exy(j,1)=xy(1,ele(j,i))
            exy(j,2)=xy(2,ele(j,i))
          enddo
          exy(ELE(0,i)+1,1)=xy(1,ele(1,i))
          exy(ELE(0,i)+1,2)=xy(2,ele(1,i))
                          
          do j=1,ELE(0,i)+1
            exy(j,1)=(exy(j,1)+offx)*scale
            exy(j,2)=(exy(j,2)+offy)*scale
          enddo

          do j=1,ELE(0,i)          
            if (flags(5) .and. spig(j,i).gt.0) then
              write (1,'(f6.1,1x,f6.1,1x,a1)') 
     &              (exy(j,1)+exy(j+1,1))/2.0,
     &              (exy(j,2)+exy(j+1,2))/2.0, 'M'
              write (1,'(a3,I4,a21)') '0 (',spig(j,i),
     &              ') stringwidth pop add'   
              write (1,'(a1,I4,a6)') '(',spig(j,i),') show'
            endif
            if (flags(6).and.spig(j,i).gt.0) then                             
              write (1,'(f6.1,1x,f6.1,1x,a1)') 
     &              (exy(j,1)+exy(j+1,1))/2.0,
     &              (exy(j,2)+exy(j+1,2))/2.0-85, 'M'
              write (1,'(a3,I4,a21)') '0 (',slab(spig(j,i)),
     &              ') stringwidth pop add'   
              write (1,'(a1,I4,a6)') '(',slab(spig(j,i)),') show'
            endif
          enddo  
        endif
      enddo

      write(1,*) 'stroke'
      write(1,*) 'grestore'
      write(1,*) 'end'
      write(1,*) 'showpage'
      write(1,*) '%%Trailer'
      write(1,*) '%%Pages: 1'
      
      RETURN 
      END
