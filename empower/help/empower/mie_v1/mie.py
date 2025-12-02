import mpmath as mp
import numpy as np
import matplotlib.pyplot as plt

SHOW_PLOTS = False

mp.mp.dps = 50  # high precision recommended for Mie series computations

def mie_abcd(m, x):
    """
    Computes Mie coefficients a_n, b_n, c_n, d_n,
    of order n=1 to nmax, complex refractive index m=m'+im'',
    and size parameter x=k0*a, where k0 = wave number
    in the ambient medium, a = sphere radius;
    p. 100, 477 in Bohren and Huffman (1983) BEWI:TDD122
    Translation of MATLAB code by C. Mätzler, June 2002
    """
    # Maximum order
    nmax = round(2 + x + 4 * x**(1/3))
    n = [i for i in range(1, nmax + 1)]
    nu = [i + 0.5 for i in n]

    z = m * x
    m2 = m * m

    # Helper scaling factors
    sqx = mp.sqrt(0.5 * mp.pi / x)
    sqz = mp.sqrt(0.5 * mp.pi / z)

    # Riccati–Bessel functions (element wise)
    bx = [mp.besselj(v, x) * sqx for v in nu]
    bz = [mp.besselj(v, z) * sqz for v in nu]
    yx = [mp.bessely(v, x) * sqx for v in nu]
    hx = [bx[i] + 1j*yx[i] for i in range(nmax)]

    # Shifted values
    b1x = [mp.sin(x)/x] + bx[:nmax-1]
    b1z = [mp.sin(z)/z] + bz[:nmax-1]
    y1x = [-mp.cos(x)/x] + yx[:nmax-1]
    h1x = [b1x[i] + 1j*y1x[i] for i in range(nmax)]

    # Axial functions
    ax  = [x * b1x[i] - n[i] * bx[i] for i in range(nmax)]
    az  = [z * b1z[i] - n[i] * bz[i] for i in range(nmax)]
    ahx = [x * h1x[i] - n[i] * hx[i] for i in range(nmax)]

    # Mie coefficients
    an = [(m2*bz[i]*ax[i] - bx[i]*az[i])/(m2*bz[i]*ahx[i] - hx[i]*az[i]) for i in range(nmax)]
    bn = [(bz[i]*ax[i] - bx[i]*az[i])/(bz[i]*ahx[i] - hx[i]*az[i]) for i in range(nmax)]
    cn = [(bx[i]*ahx[i] - hx[i]*ax[i])/(bz[i]*ahx[i] - hx[i]*az[i]) for i in range(nmax)]
    dn = [m*(bx[i]*ahx[i] - hx[i]*ax[i])/(m2*bz[i]*ahx[i] - hx[i]*az[i]) for i in range(nmax)]

    # Output as list of lists (same structure: 4 × nmax)
    result = [an, bn, cn, dn]
    return result

def mie(m, x):
    """
    Computation of Mie Efficiencies for given
    complex refractive-index ratio m=m'+im''
    and size parameter x=k0*a, where k0= wave number in ambient
    medium, a=sphere radius, using complex Mie Coefficients
    an and bn for n=1 to nmax,
    s. Bohren and Huffman (1983) BEWI:TDD122, p. 103,119-122,477.
    Result: m', m'', x, efficiencies for extinction (qext),
    scattering (qsca), absorption (qabs), backscattering (qb),9
    asymmetry parameter (asy=<costeta>) and (qratio=qb/qsca).
    Uses the function "mie_abcd" for an and bn, for n=1 to nmax.
    C. Mätzler, May 2002.
    """
    # If x == 0: singularity case (same as MATLAB)
    if x == 0:
        result = np.array([m.real, m.imag, 0, 0, 0, 0, 0, 0, 1.5], dtype=float)
        return result
    # Normal case (x > 0)
    nmax = int(round(2 + x + 4 * x ** (1/3)))
    n = np.arange(1, nmax + 1)
    cn = 2 * n + 1
    c1n = n * (n + 2) / (n + 1)
    c2n = cn / (n * (n + 1))
    x2 = x * x

    # Retrieve a_n and b_n coefficients
    f = mie_abcd(m, x)  # expects shape (2, nmax)  
    f = np.array(f, dtype=object)
    f = f.astype(complex)

    an = f[0, :]
    bn = f[1, :]

    # Real/imaginary parts
    anp = an.real
    anpp = an.imag
    bnp = bn.real
    bnpp = bn.imag

    # g1 shifted index family for asymmetry parameter
    g1 = np.zeros((4, nmax))
    g1[0, :-1] = anp[1:]
    g1[1, :-1] = anpp[1:]
    g1[2, :-1] = bnp[1:]
    g1[3, :-1] = bnpp[1:]

    # Efficiency calculations
    dn = cn * (anp + bnp)
    qext = 2 * np.sum(dn) / x2

    en = cn * (anp**2 + anpp**2 + bnp**2 + bnpp**2)
    qsca = 2 * np.sum(en) / x2

    qabs = qext - qsca

    # Backscattering
    fn = (an - bn) * cn
    gn = (-1)**n
    back = np.sum(fn * gn)
    qb = (back * np.conj(back)).real / x2

    # Asymmetry parameter
    asy1 = c1n * (anp * g1[0] + anpp * g1[1] + bnp * g1[2] + bnpp * g1[3])
    asy2 = c2n * (anp * bnp + anpp * bnpp)
    asy = (4 / x2) * np.sum(asy1 + asy2) / qsca

    qratio = qb / qsca

    result = np.array([m.real, m.imag, x, qext, qsca, qabs, qb, asy, qratio], dtype=float)
    return result

def mie_s12(m, x, u):
    """
    Computation of Mie Scattering functions S1 and S2
    for complex refractive index m=m'+im'',
    size parameter x=k0*a, and u=cos(scattering angle),
    where k0=vacuum wave number, a=sphere radius;
    s. p. 111-114, Bohren and Huffman (1983) BEWI:TDD122
    C. Mätzler, May 2002
    """

    # Ensure u is array for vectorized operations
    u = np.atleast_1d(u)

    # Order limit
    nmax = int(round(2 + x + 4 * x ** (1/3)))

    # Mie coefficients (shape: (2, nmax))
    abcd = Mie_abcd(m, x)
    an = abcd[0, :]   # shape (nmax,)
    bn = abcd[1, :]

    # Angular functions π_n and τ_n (shape: (2, nmax, len(u)))
    pt = Mie_pt(u, nmax)
    pin = pt[0, :, :]  # π_n(u)
    tin = pt[1, :, :]  # τ_n(u)

    # n = 1...nmax
    n = np.arange(1, nmax + 1)
    n2 = (2 * n + 1) / (n * (n + 1))

    # Apply normalization factor to angular functions
    pin = pin * n2[:, None]
    tin = tin * n2[:, None]

    # Compute S1 and S2 via vectorized summation over n
    S1 = np.sum(an[:, None] * pin + bn[:, None] * tin, axis=0)
    S2 = np.sum(an[:, None] * tin + bn[:, None] * pin, axis=0)

    result = np.vstack((S1, S2))
    return result

def mie_xscan(m, nsteps, dx):
    """
    Computation and plot of Mie Efficiencies for given
    complex refractive-index ratio m=m'+im''
    and range of size parameters x=k0*a,
    starting at x=0 with nsteps increments of dx
    a=sphere radius, using complex Mie coefficients an and bn
    according to Bohren and Huffman (1983) BEWI:TDD122
    result: m', m'', x, efficiencies for extinction (qext),
    scattering (qsca), absorption (qabs), backscattering (qb),
    qratio=qb/qsca and asymmetry parameter (asy=<costeta>).
    C. Mätzler, May 2002.
    """

    # Create array of x values starting at 0
    x = np.arange(nsteps) * dx  # shape: (nsteps,)

    # Allocate result array
    results = np.zeros((nsteps, 9))

    # Compute Mie results at each x
    for j in range(nsteps):
        results[j, :] = mie(m, x[j])

    # Plot results (columns 3→8 correspond to qext...qratio)
    plt.figure()
    plt.plot(results[:, 2], results[:, 3:9])
    plt.legend(['Qext', 'Qsca', 'Qabs', 'Qb', '<cosθ>', 'Qb/Qsca'])
    plt.title(f"Mie Efficiencies, m = {m.real:.3g} + {m.imag:.3g}i")
    plt.xlabel("Size parameter x")
    plt.grid(True)
    plt.tight_layout()
    plt.show()

    return results

def mie_tetascan(m, x, nsteps):
    """
    Computation and plot of Mie Power Scattering function for given
    complex refractive-index ratio m=m'+im'', size parameters x=k0*a,
    according to Bohren and Huffman (1983) BEWI:TDD122    
    Computation and polar plot of Mie Power Scattering function for:
      m = complex refractive-index ratio
      x = size parameter
      nsteps = number of angular steps
    Translation from MATLAB code by C. Mätzler (2002)
    """

    m1 = np.real(m)
    m2 = np.imag(m)

    # Create theta array
    teta = np.linspace(0, np.pi, nsteps)
    
    SL = np.zeros(nsteps)
    SR = np.zeros(nsteps)
    
    for j in range(nsteps):
        u = np.cos(teta[j])
        a = mie_s12(m, x, u)
        SL[j] = np.real(a[0] * np.conj(a[0]))
        SR[j] = np.real(a[1] * np.conj(a[1]))
    
    # Mirror data like MATLAB code
    y_theta = np.concatenate((teta, teta + np.pi))
    y_vals = np.concatenate((SL, SR[::-1]))

    y = np.column_stack((y_theta, y_vals))

    # Polar Plot
    plt.figure()
    ax = plt.subplot(111, projection='polar')
    ax.plot(y[:, 0], y[:, 1])
    ax.set_title(f"Mie angular scattering: m={m1}+{m2}i, x={x}")
    ax.set_xlabel("Scattering Angle")
    plt.show()

    result = y
    return result

import numpy as np

def mie_pt(u, nmax):
    """
    pi_n and tau_n, -1 <= u= cosθ <= 1, n1 integer from 1 to nmax
    angular functions used in Mie Theory
    Bohren and Huffman (1983), p. 94 - 95
    
    Compute the angular functions pi_n and tau_n used in Mie theory.
    Translated from MATLAB version by C. Mätzler (2002)
 
    Parameters:
        u : float  (cos(theta), must be in [-1, 1])
        nmax : int
        
    Returns:
        result : ndarray shape (2, nmax)
                 result[0, :] = pi_n
                 result[1, :] = tau_n
    """

    p = np.zeros(nmax)
    t = np.zeros(nmax)

    # Initial conditions
    p[0] = 1.0
    t[0] = u
    if nmax > 1:
        p[1] = 3 * u
        t[1] = 3 * np.cos(2 * np.arccos(u))

    # Recurrence
    for n1 in range(3, nmax + 1):
        n = n1 - 1  # MATLAB indexing shift
        p1 = (2*n1 - 1)/(n1 - 1) * p[n - 1] * u
        p2 = n1/(n1 - 1) * p[n - 2]
        p[n] = p1 - p2

        t1 = n1 * u * p[n]
        t2 = (n1 + 1) * p[n - 1]
        t[n] = t1 - t2

    result = np.vstack((p, t))
    return result

def mie_esquare(m, x, nj):
    """
    Computation of nj+1 equally spaced values within (0,x)
    of the mean-absolute-square internal
    electric field of a sphere of size parameter x,
    complex refractive index m=m'+im'',
    where the averaging is done over teta and phi,
    with unit-amplitude incident field;
    Ref. Bohren and Huffman (1983) BEWI:TDD122,
    and my own notes on this topic;
    k0=2*pi./wavelength;
    x=k0.*radius;
    Mean-absolute-square internal electric field inside a sphere (Mie theory)
    Compute and plot mean-square internal electric field |E|^2 inside a sphere.
    
    Parameters:
        m  : complex refractive index ratio
        x  : size parameter (k0 * radius)
        nj : number of radial sample points
    
    Returns:
        een : numpy array of |E|^2 samples, length nj+1
    MATLAB Source: C. Mätzler (2002)
    """

    # Determine max series term
    nmax = int(round(2 + x + 4 * x**(1/3)))
    n = np.arange(1, nmax + 1)
    nu = n + 0.5

    m1 = mp.re(m)
    m2 = mp.im(m)

    # Fetch cn & dn from Mie coefficients
    abcd = mie_abcd(m, x)  # returns NumPy or Python lists convertible to mp
    #cn = np.array(abcd[2, :], dtype=object)
    #dn = np.array(abcd[3, :], dtype=object)    
    cn = np.array(abcd, dtype=object)[2, :]
    dn = np.array(abcd, dtype=object)[3, :]

    cn2 = np.abs(cn)**2
    dn2 = np.abs(dn)**2

    dx = x / nj
    en = np.zeros(nj, dtype=float)

    # Loop over radial steps
    for j in range(1, nj + 1):
        xj = dx * j
        z = m * xj  # complex

        sqz = mp.sqrt(0.5 * mp.pi / z)

        # spherical j_n(z) = sqrt(pi/(2*z)) * J_{n+1/2}(z)
        bz = np.array([mp.besselj(nu[k], z) * sqz for k in range(len(n))], dtype=object)
        bz2 = np.abs(bz)**2

        # j_0(z) = sin(z)/z
        b1z = np.zeros_like(bz, dtype=object)
        b1z[0] = mp.sin(z) / z
        b1z[1:] = bz[:-1]

        az = b1z - (n * bz) / z
        az2 = np.abs(az)**2

        z2 = np.abs(z)**2
        n1 = n * (n + 1)
        n2 = 2 * (2 * n + 1)

        mn = np.real(bz2 * n2)
        nn1 = az2
        nn2 = bz2 * n1 / z2
        nn = n2 * np.real(nn1 + nn2)

        en[j - 1] = 0.25 * (cn2 @ mn + dn2 @ nn)

    # Build radial coordinate array
    xxj = np.linspace(0, x, nj + 1)
    een = np.concatenate(([en[0]], en))

    if SHOW_PLOTS:
        # Plot results
        plt.figure()
        plt.plot(xxj, een)
        plt.title(
            f"Squared |E|² inside Sphere — "
            f"m={float(m1):.4g}+{float(m2):.4g}i, x={float(x):.4g}"
        )
        plt.xlabel('r k')
        plt.ylabel('|E|²')
        plt.grid(True)
        plt.legend(['Radial Dependence of (abs(E))²'])
        plt.show()  

    result = een
    return result

def mie_abs(m, x):
    """
    Absorption efficiency Qabs for a sphere using Mie theory.
    Translation of MATLAB code by C. Mätzler (2002).

    Parameters
    ----------
    m : complex
        Refractive index (relative)
    x : float
        Size parameter

    Returns
    -------
    Qabs : float
        Absorption efficiency
    """

    # Determine number of internal field samples
    nj = 5 * round(2 + x + 4 * x**(1/3)) + 160

    e2 = mp.im(m * m)
    dx = x / nj
    x2 = x * x

    # Radial positions: 0, dx, 2dx, ..., x
    xj = np.linspace(0, x, nj + 1)

    # Compute |E|^2 internal field profile
    en = mie_esquare(m, x, nj)  # assumes Mie_Esquare internally calls Mie_abcd

    # End-term correction
    en1 = 0.5 * en[-1] * x2

    # Compute radial weighted field: en(r) * r^2 - correction
    enx = en * (xj * xj)
    enx = np.sum(enx) -  en1

    # Trapezoidal radial integration
    inte = dx * enx

    # Absorption Efficiency
    Qabs = 4 * e2 * inte / x2
    result = float(mp.re(Qabs))
    
    return result
    
if __name__ == "__main__":
    m = complex(5, 0.4)
    x = 1
    #print(mie_abcd(m, x))
    print(mie_abs(m, x))
    mie_xscan(m, 201, 0.01)


