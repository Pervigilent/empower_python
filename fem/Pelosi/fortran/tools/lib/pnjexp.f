C  =====================================================================
      complex function Pjn_expa(n,j,a)
C  =====================================================================
c     Computes the Integral in [0,1] of the product Pj(n)(x)*exp(a*x)
c     being:
c        Pj(n) the interpolating polynomials of order n-1 with
c              unitary value in x=(j-1)/2.
c              It is 1<j<n.
c        a     a complex constant
C     QUICK_FEM (C) 1997 PELOSI - COCCIOLI - SELLERI
C  =====================================================================
      implicit none

      integer n,j
      complex a,a2,a3,a4,a5

      a2 = a * a
      a3 = a2 * a

      if ( abs(a) .gt. 1.E-06 )
     &     goto (1,2,3,4,5,6,7,8,9,10,11,12,13,14), n*(n-1)/2-1+j

      if ( abs(a) .le. 1.E-06 )
     &   goto (15,16,17,18,19,20,21,22,23,24,25,26,27,28), n*(n-1)/2-1+j

c     Exact expression of the sought integral
0001  Pjn_expa = ( exp(a) - a - 1. )/a2
      goto 0030
0002  Pjn_expa = ( exp(a)*(a-1.) +1. )/a2
      goto 0030
0003  Pjn_expa = -exp(a)*(a-4.)/a3-(a2+3.*a+4.)/a3
      goto 0030
0004  Pjn_expa = 4.*exp(a)*(a-2.)/a3+4.*(a+2.)/a3
      goto 0030
0005  Pjn_expa = exp(a)*(a2-3.*a+4.)/a3-(a+4.)/a3
      goto 0030
0006  a4 = a3 * a
      Pjn_expa = exp(a)*(a2-9.*a+27.)/a4-
     &                  (2.*a3+11.*a2+36.*a+54.)/(2.*a4)
      goto 0030
0007  a4 = a3 * a
      Pjn_expa = 9.*(a2-5.*a+9.)/a4-9.*exp(a)*(a2-8.*a+18.)/(2.*a4)
      goto 0030
0008  a4 = a3 * a
      Pjn_expa = 9.*exp(a)*(a2-5.*a+9.)/a4-9.*(a2+8.*a+18.)/(2.*a4)
      goto 0030
0009  a4 = a3 * a
      Pjn_expa = exp(a)*(2.*a3-11.*a2+36.*a-54.)/(2.*a4)+
     &                                       (a2+9.*a+27.)/a4
      goto 0030
0010  a4 = a3 * a
      a5 = a4 * a
      Pjn_expa =-exp(a)*(3.*a3-44.*a2+288.*a-768.)/(3.*a5)-
     &               (3.*a4+25.*a3+140.*a2+480.*a+768.)/(3.*a5)
      goto 0030
0011  a4 = a3 * a
      a5 = a4 * a
      Pjn_expa =16*exp(a)*(a3-14.*a2+84.*a-192.)/(3.*a5)+
     &            16.*(3.*a3+26.*a2+108.*a+192.)/(3.*a5)
      goto 0030
0012  a4 = a3 * a
      a5 = a4 * a
      Pjn_expa = -4.*exp(a)*(3.*a3-38.*a2+192.*a-384.)/a5
     &                     -4.*(3.*a3+38.*a2+192.*a+384.)/a5
      goto 0030
0013  a4 = a3 * a
      a5 = a4 * a
      Pjn_expa = 16.*exp(a)*(3.*a3-26.*a2+108.*a-192.)/(3.*a5)
     &                     +16.*(a3+14.*a2+84.*a+192.)/(3.*a5)
      goto 0030
0014  a4 = a3 * a
      a5 = a4 * a
      Pjn_expa = exp(a)*(3.*a4-25.*a3+140.*a2-480.*a+768.)/(3.*a5)
     &                        -(3.*a3+44.*a2+288.*a+768.)/(3.*a5)
      goto 0030
c     Taylor approximationof the above expressions which must be used
c     when the complex parameter a is near to zero
0015  Pjn_expa = a3/120.+a2/24.+a/6.+1./2
      goto 0030
0016  Pjn_expa =  a3/30.+a2/8.+a/3.+1./2
      goto 0030
0017  Pjn_expa = -a3/360.-a2/120.+1./6
      goto 0030
0018  Pjn_expa = a3/45.+a2/10.+a/3.+2./3
      goto 0030
0019  Pjn_expa = a3/45.+3.*a2/40.+a/6.+1./6
      goto 0030
0020  Pjn_expa = a3/840.+a2/240.+a/60.+1./8
      goto 0030
0021  Pjn_expa = -a3/280.+3.*a/40.+3./8
      goto 0030
0022  Pjn_expa = a3/35.+9.*a2/80.+3.*a/10.+3./8
      goto 0030
0023  Pjn_expa = 13.*a3/840.+a2/20.+13.*a/120.+1./8
      goto 0030
0024  Pjn_expa = -a3/1512.-a2/504.+7./90
      goto 0030
0025  Pjn_expa = 4.*a3/945.+2.*a2/105.+4.*a/45.+16./45
      goto 0030
0026  Pjn_expa = -a3/315.+a2/210.+a/15.+2./15
      goto 0030
0027  Pjn_expa = 4.*a3/135.+34.*a2/315.+4.*a/15.+16./45
      goto 0030
0028  Pjn_expa = 11.*a3/945.+31.*a2/840.+7.*a/90.+7./90
      goto 0030
0030  return
      end
