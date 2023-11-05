import numpy as np

dt = 1
e0 = 1

x_length = 0.1
y_length = 0.1
z_length = 0.1

Nx = 100
Ny = 100
Nz = 100


dx = x_length / Nx
dy = y_length / Ny
dz = z_length / Nz

CEx = np.zeros(shape=(Nx, Ny, Nz))
Ez = np.zeros(shape=(Nx, Ny, Nz))
Ey = np.zeros(shape=(Nx, Ny, Nz))

c0 = 3E8
URxx = np.ones(shape=(Nx, Ny))
URyy = np.ones(shape=(Nx, Ny))

# Compute CEx
for nx in range(0, Nx):
    for ny in range(0, Ny - 1):
        for nz in range(0, Nz - 1):
            CEx[nx][ny][nz] = (Ez[nx][ny + 1][nz] - Ez[nx][ny][nz]) / dy - (Ey[nx][ny][nz + 1] - Ey[nx][ny][nz]) / dz
        CEx[nx][ny][nz] = (Ez[nx][ny + 1][Nz] - Ez[nx][ny][Nz]) / dy - (Ey[nx][ny][0] - Ey[nx][ny][Nz]) / dz
        for nz in range(0, Nz - 1):
            CEx[nx][Ny][nz] = (Ez[nx][0][nz] - Ez[nx][Ny][nz]) / dy - (Ey[nx][Ny][nz + 1] - Ey[nx][Ny][nz]) / dz
        CEx[nx][Ny][Nz] = (Ez[nx][0][Nz] - Ez[nx][Ny][Nz]) / dy - (Ey[nx][Ny][0] - Ey[nx][Ny][Nz]) / dz

# Compute PML parameters
NPML = (2, 2, 2, 2)
Nx2 = 2 * Nx
Ny2 = 2 * Ny

sigx = np.zeros(shape=(Nx2, Ny2))
for nx in range(0, 2 * NPML[0]):
    nx1 = 2 * NPML[0] - nx + 1
    sigx[nx1] = (0.5 * e0 / dt) * (nx / 2 / NPML[0]) ** 3 * np.ones(shape=(Ny2,))
for nx in range(0, 2 * NPML[1]):
    nx1 = Nx2 - 2 * NPML[2] + nx
    sigx[nx1] = (0.5 * e0 / dt) * (nx / 2/ NPML[1]) ** 3 * np.ones(shape=(Ny2,))

sigy = np.zeros(shape=(Nx2, Ny2))
for ny in range(0, 2 * NPML[2]):
    ny1 = 2 * NPML[2] - ny + 1
    for i in range(0, Nx2):
        sigy[i][ny1] = (0.5 * e0 / dt) * (ny / 2 / NPML[2]) ** 3
for ny in range(0, 2 * NPML[3]):
    ny1 = Ny2 - 2 * NPML[3] + ny
    for i in range(0, Nx2):
        sigy[i][ny1] = (0.5 * e0 / dt) * (ny / 2 / NPML[3]) ** 3

# Compute update coefficients
#sigHx = np.zeros(shape=(Nx, Ny))
#sigHy = np.zeros(shape=(Nx, Ny))

sigHx = sigx[0:Nx2:2, 1:Ny2:2]
sigHy = sigy[0:Nx2:2, 1:Ny2:2]
mHx0 = (1 / dt) + sigHy / (2 * e0)
mHx1 = ((1 / dt) - sigHy / (2 * e0))
mHx2 = - c0 / URxx / mHx0
mHx3 = - (c0 * dt / e0) * sigHx / URxx / mHx0

sigHx = sigx[1:Nx2:2, 0:Ny2:2]
sigHy = sigy[1:Nx2:2, 0:Ny2:2]
mHy0 = (1 / dt) + sigHx / (2 * e0)
mHy1 = ((1 / dt) - sigHx / (2 * e0)) / mHy0
mHy2 = - c0 / URyy / mHy0
mHy3 = - (c0 * dt / e0) * sigHy / URyy / mHy0

sigDx = sigx[0:Nx2:2, 0:Ny2:2]
sigDy = sigy[0:Nx2:2, 0:Ny2:2]
mDz0 = (1 / dt) + (sigDx + sigDy) / (2 * e0) + sigDx * sigDy * (dt / 4 / e0 ** 2)
mDz1 = (1 / dt) - (sigDx + sigDy) / (2 * e0) - sigDx * sigDy * (dt / 4 / e0 ** 2)
mDz1 = mDz1 / mDz0
mDz2 = c0 / mDz0
mDz4 = - (dt / e0 ** 2) * sigDx * sigDy / mDz0
