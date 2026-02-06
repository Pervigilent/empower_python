import numpy as np


def read_nodal_mesh(finname, MAXNODE, MAXNXE, MAXELE):
    """
    Reads a geometry file, extracting:
        - node coordinates
        - node labels
        - element connectivity
        - element labels

    Parameters
    ----------
    finname : str
        Input filename
    MAXNODE : int
        Maximum allowed nodes
    MAXNXE : int
        Maximum allowed nodes per element
    MAXELE : int
        Maximum allowed elements

    Returns
    -------
    XY    : ndarray shape (2, NNODE)
    ELE   : ndarray shape (MAXNXE+1, NELE)
    NLAB  : ndarray shape (NNODE,)
    ELAB  : ndarray shape (NELE,)
    NNODE : int
    NELE  : int
    """

    try:
        with open(finname, 'r') as f:

            # ---------- Reading Header
            _ = f.readline()
            line = f.readline()
            nele = int(line.split()[-1])

            line = f.readline()
            nnode = int(line.split()[-1])

            line = f.readline()
            nspig = int(line.split()[-1])

            # ---------- Size checks
            if MAXNODE < nnode:
                raise ValueError(f"Parameter MAXNODE must be at least {nnode}")

            if MAXELE < nele:
                raise ValueError(f"Parameter MAXELE must be at least {nele}")

            # ---------- Allocate arrays
            XY = np.zeros((2, nnode), dtype=float)
            NLAB = np.zeros(nnode, dtype=int)

            ELE = np.zeros((MAXNXE + 1, nele), dtype=int)
            ELAB = np.zeros(nele, dtype=int)

            # ---------- Reading connection matrix & element labels
            _ = f.readline()

            for _ in range(nele):
                data = list(map(int, f.readline().split()))

                ii = data[0] - 1          # Fortran → Python index
                nn = data[1]

                ELE[0, ii] = nn
                ELE[1:nn+1, ii] = data[2:2+nn]
                ELAB[ii] = data[2+nn]

            # ---------- Reading node coordinates & labels
            _ = f.readline()

            for _ in range(nnode):
                data = f.readline().split()

                ii = int(data[0]) - 1     # Fortran → Python index
                XY[0, ii] = float(data[1])
                XY[1, ii] = float(data[2])
                NLAB[ii] = int(data[3])

    except FileNotFoundError:
        raise RuntimeError(f"****ERROR 001****\nCannot open: {finname}")

    except Exception as e:
        raise RuntimeError(f"Error reading mesh file {finname}:\n{e}")

    return XY, ELE, NLAB, ELAB, nnode, nele

