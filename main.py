import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import math

def division(dividend, divisor):
	output = np.zeros_like(dividend)
	for i in range(0, dividend.size):
		output[i] = dividend[i] / divisor[i]
	return output


STEPS = 250
c0 = 3E8 # [meters per second]
light_speed = c0
z_size  = STEPS # Nz
z_length = 0.01 # [meters]
time_step = z_length / light_speed / z_size # dt [seconds]

dz = z_length / z_size

# Initialize materials to free space
relative_permittivity = np.ones(z_size + 2) # ER
relative_permeability = np.ones(z_size + 2) # UR

def source(time, tau):
	# tau = 1
	time_0 = 0
	g = math.exp(-((time - time_0) / tau) ** 2)
	return g

def calculate_field(step):
	# STEPS = 500
	LAMBDA = 600E-9
	NRES = 100
	NDRES = NRES

	z_length = 0.01
	nzsrc = int(STEPS / 2)

	c0 = 3E8  # [meters per second]
	light_speed = c0
	frequency_max = light_speed / LAMBDA

	tau = 0.5 / frequency_max

	# Initialize materials to free space
	relative_permittivity = np.ones(z_size)  # ER
	relative_permeability = np.ones(z_size)  # UR

	nmax = max(relative_permittivity)

	# Compute default grid resolution
	dz1 = LAMBDA / nmax / NRES
	dz = dz1
	dz = z_length / z_size

	# Compute update coefficients
	m_ey = division(np.full_like(relative_permittivity, (light_speed * time_step)),
					relative_permittivity)  # mEy (update coefficient)
	m_hx = division(np.full_like(relative_permeability, (light_speed * time_step)),
					relative_permeability)  # mHY (update coefficient)
	h_x = np.zeros(z_size)  # Hx
	e_y = np.zeros(z_size)  # Ey

	h1 = h_x[0]
	e1 = e_y[0]
	h2 = h1
	e2 = e1
	# Main FDTD loop
	for t in range(0, step):  # t is T
		# Update H from E (perfect boundary conditions)
		h2 = h1
		h1 = h_x[0]
		for nz in range(0, z_size - 1):
			h_x[nz] = h_x[nz] + m_hx[nz] * (e_y[nz + 1] - e_y[nz]) / dz
		h_x[z_size - 1] = h_x[z_size - 1] + m_hx[z_size - 1] * (e2 - e_y[z_size - 1]) / dz
		# Update E from H (perfect boundary conditions)
		e2 = e1
		e1 = e_y[z_size - 1]
		e_y[0] = e_y[0] + m_ey[0] * (h_x[0] - h2) / dz
		for nz in range(1, z_size):
			e_y[nz] = e_y[nz] + m_ey[nz] * (h_x[nz] - h_x[nz - 1]) / dz
		# Inject soft source
		e_y[nzsrc] = e_y[nzsrc] + source(t, 25)

	x = np.linspace(0, z_length, z_size)
	myPlot.set_xdata(x)
	myPlot.set_ydata(e_y[:z_size])
	if not step % 50:
		print("Iteration: " + str(step) + ", Maximum: " + str(max(e_y)) +
			  ", Source: " + str(source(step, 25)))
	return myPlot

x = np.linspace(0, z_length, STEPS)
e_0 = np.zeros(z_size)
fig, ax = plt.subplots()
ax.set(ylim=[-1, 1])
myPlot = ax.plot(x, e_0)[0]
ani = FuncAnimation(fig=fig, func=calculate_field, frames=STEPS, interval=50)
plt.show()