import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import math


Z_LENGTH = 1E-5
Z_MINIMUM = 1E-6
Z_SIZE = 201
STEPS = 500
c0 = 3E8 # [meters per second]
REPORT_INTERVAL = 50


def source(time, tau):
	# tau = 1
	time_0 = 6 * tau
	g = math.exp(-((time - time_0) / tau) ** 2)
	return g


def calculate_field(step):
	LAMBDA_MINIMUM = 500E-9 # Smallest wavelength (meters)
	C_0 = 3E8 # Speed of light (meters per second)
	N_MAXIMUM = 1.0 # Maximum refractive index
	N_LAMBDA = 10 # Number of cells to resolve wavelength
	D_MINIMUM = Z_MINIMUM # Smallest feature size (meters)
	N_D = 5 # Number of cells to resolve smallest feature
	DEVICE_LENGTH = Z_LENGTH # (meters)

	# LAMBDA = 600E-9
	# NRES = 100
	# NDRES = NRES

	light_speed = C_0

	lambda_minimum = LAMBDA_MINIMUM
	n_maximum = N_MAXIMUM
	frequency_maximum = C_0 / (lambda_minimum * n_maximum)
	delta_lambda = lambda_minimum / N_LAMBDA # distance resolution based on wavelength

	distance_minimum = D_MINIMUM
	n_distance = N_D
	delta_distance = distance_minimum / n_distance

	distance = DEVICE_LENGTH
	delta = min([delta_lambda, delta_distance])
	n_cells = math.ceil(distance / delta)

	z_length = distance
	z_size = n_cells
	nzsrc = int(z_size / 2)
	delta = z_length / z_size

	# light_speed = C_0
	# tau = 0.5 / frequency_maximum
	tau = z_size / 8

	time_step = z_length / light_speed / z_size  # dt [seconds]

	# Initialize materials to free space
	relative_permittivity = np.ones(z_size)  # ER
	relative_permeability = np.ones(z_size)  # UR

	n_maximum = max(relative_permittivity)

	# Compute default grid resolution
	delta_z = delta

	# Compute update coefficients
	m_ey = np.divide(np.full_like(relative_permittivity, (light_speed * time_step)),
					relative_permittivity)  # mEy (update coefficient)
	m_hx = np.divide(np.full_like(relative_permeability, (light_speed * time_step)),
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
			h_x[nz] = h_x[nz] + m_hx[nz] * (e_y[nz + 1] - e_y[nz]) / delta_z
		h_x[z_size - 1] = h_x[z_size - 1] + m_hx[z_size - 1] * (e2 - e_y[z_size - 1]) / delta_z
		# Update E from H (perfect boundary conditions)
		e2 = e1
		e1 = e_y[z_size - 1]
		e_y[0] = e_y[0] + m_ey[0] * (h_x[0] - h2) / delta_z
		for nz in range(1, z_size):
			e_y[nz] = e_y[nz] + m_ey[nz] * (h_x[nz] - h_x[nz - 1]) / delta_z
		# Inject soft source
		e_y[nzsrc] = e_y[nzsrc] + source(t, tau)

	x = np.linspace(0, z_length, z_size)
	myPlot.set_xdata(x)
	myPlot.set_ydata(e_y[:z_size])
	if not step % REPORT_INTERVAL:
		print("Iteration: " + str(step) + ", Maximum: " + str(max(e_y)) +
			  ", Source: " + str(source(step, tau)) + ", Cells: " + str(z_size))
	return myPlot


x = np.linspace(0, Z_LENGTH, Z_SIZE)
e_0 = np.zeros(Z_SIZE)
fig, ax = plt.subplots()
ax.set(ylim=[-1, 1])

myPlot = ax.plot(x, e_0)[0]
ani = FuncAnimation(fig=fig, func=calculate_field, frames=STEPS, interval=25)
plt.show()