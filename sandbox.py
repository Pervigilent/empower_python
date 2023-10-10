import numpy as np

def division(dividend, divisor):
	output = np.zeros_like(dividend)
	for i in range(0, dividend.size):
		output[i] = dividend[i] / divisor[i]
	return output

c0 = 3E8 # [meters per second]
light_speed = c0
z_size  = 1000 # Nz
z_length = 0.01 # [meters]
time_step = z_length / light_speed / z_size # dt [seconds]
STEPS = 1000

dz = z_length / z_size

# Initialize materials to free space
relative_permittivity = np.ones(z_size + 2) # ER
relative_permeability = np.ones(z_size + 2) # UR

# Compute update coefficients
m_ey = division(np.full_like(relative_permittivity, (light_speed * time_step)),
	relative_permittivity) # mEy (update coefficient)
m_hx = division(np.full_like(relative_permeability, (light_speed * time_step)),
	relative_permeability) # mHY (update coefficient)

h_x = np.zeros(z_size + 2) # Hx
e_y = np.zeros(z_size + 2) # Ey

# Main FDTD loop
for t in range(0, STEPS): # t is T
	# Update H from E
	for nz in range(1, z_size + 1):
		h_x[nz] = h_x[nz] + m_hx[nz] * (e_y[nz + 1] - e_y[nz]) / dz
	# Update E from H
		e_y[nz] = e_y[nz] + m_ey[nz] * (h_x[nz] - h_x[nz - 1]) / dz
