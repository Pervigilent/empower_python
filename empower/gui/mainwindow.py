import tkinter as tk
from tkinter import ttk
from enum import Enum
import os
import sys
import subprocess

from empower.gui.utilities import (
	Display,
	ProjectManager,
	Properties,
	MessageManager,
	Progress,
	Status
)

class WindowMode(Enum):
	BASE = 0
	EMPOWER = 1

class MainWindow(tk.Tk):
	def __init__(self):
		super().__init__()
		self.title("Nash Electronics Desktop")
		self.geometry("1080x720")
		self.window_mode = WindowMode.BASE
		self.create_menu()
		self.create_windows()

	def create_menu(self):
		self.menubar = tk.Menu(self)
		self.empower_menubar = tk.Menu(self)
		
		# Base File Menu
		self.file_menu = tk.Menu(self.menubar, tearoff=0)
		self.file_menu.add_command(label='New', command=self.generic_callback)
		self.file_menu.add_command(label='Open...', command=self.generic_callback)
		self.file_menu.add_command(label='Open Examples...', command=self.generic_callback)
		self.file_menu.add_command(label='Close', command=self.generic_callback)
		self.file_menu.add_separator()
		self.file_menu.add_command(label='Save', command=self.generic_callback)
		self.file_menu.add_command(label='Save As...', command=self.generic_callback)
		self.file_menu.add_command(label='Save As Technology File', command=self.generic_callback)
		self.file_menu.add_separator()
		self.file_menu.add_command(label='Archive...', command=self.generic_callback)
		self.file_menu.add_command(label='Restore Archive...', command=self.generic_callback)
		self.file_menu.add_separator()
		self.file_menu.add_command(label='Nash Athena', command=self.generic_callback)
		self.file_menu.add_separator()
		self.file_menu.add_command(label='Page Setup...', command=self.generic_callback)
		self.file_menu.add_command(label='Print Preview', command=self.generic_callback)
		self.file_menu.add_command(label='Print...', command=self.generic_callback)
		self.file_menu.add_separator()
		self.file_menu.add_command(label='Import', command=self.generic_callback)
		self.file_menu.add_command(label='Export', command=self.generic_callback)
		self.file_menu.add_separator()
		self.file_menu.add_command(label='Exit', command=self.generic_callback)
		self.menubar.add_cascade(label='File', menu=self.file_menu, underline=0)
		
		# Empower File Menu
		self.empower_file_menu = tk.Menu(self.empower_menubar, tearoff=0)
		self.empower_file_menu.add_command(label='New', command=self.generic_callback)
		self.empower_file_menu.add_command(label='Open...', command=self.generic_callback)
		self.empower_file_menu.add_command(label='Open Examples...', command=self.generic_callback)
		self.empower_file_menu.add_command(label='Close', command=self.generic_callback)
		self.empower_file_menu.add_separator()
		self.empower_file_menu.add_command(label='Save', command=self.generic_callback)
		self.empower_file_menu.add_command(label='Save As...', command=self.generic_callback)
		self.empower_file_menu.add_command(label='Save As Technology File', command=self.generic_callback)
		self.empower_file_menu.add_separator()
		self.empower_file_menu.add_command(label='Archive...', command=self.generic_callback)
		self.empower_file_menu.add_command(label='Restore Archive...', command=self.generic_callback)
		self.empower_file_menu.add_separator()
		self.empower_file_menu.add_command(label='Nash Athena', command=self.generic_callback)
		self.empower_file_menu.add_separator()
		self.empower_file_menu.add_command(label='Page Setup...', command=self.generic_callback)
		self.empower_file_menu.add_command(label='Print Preview', command=self.generic_callback)
		self.empower_file_menu.add_command(label='Print...', command=self.generic_callback)
		self.empower_file_menu.add_separator()
		self.empower_file_menu.add_command(label='Import', command=self.generic_callback)
		self.empower_file_menu.add_command(label='Export', command=self.generic_callback)
		self.empower_file_menu.add_separator()
		self.empower_file_menu.add_command(label='Exit', command=self.generic_callback)
		self.empower_menubar.add_cascade(label='File', menu=self.empower_file_menu, underline=0)
		
		# Base Project Menu
		self.project_menu = tk.Menu(self.menubar, tearoff=0)
		self.project_menu.add_command(label='Insert Empower Design', command=self.insert_empower_design)
		self.project_menu.add_command(label='Insert Empower 3D Layout Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert 3D Extractors Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert 2D Extractors Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert Circuits Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert Circuits Netlist', command=self.generic_callback)
		self.project_menu.add_command(label='Insert EMITS Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert Heaviside 3D Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert Heaviside 2D Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert RMexpert Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert Heaviside Circuit Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert SimXplorer Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert Heatpack Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert Mechanical Design', command=self.generic_callback)
		self.project_menu.add_command(label='Insert Documentation Design', command=self.generic_callback)
		self.project_menu.add_separator()
		self.project_menu.add_command(label='Analyze All', command=self.generic_callback)
		self.project_menu.add_command(label='Submit Job...', command=self.generic_callback)
		self.project_menu.add_separator()
		self.project_menu.add_command(label='Project Variables...', command=self.generic_callback)
		self.project_menu.add_command(label='Datasets...', command=self.generic_callback)
		self.project_menu.add_command(label='Event Callbacks...', command=self.generic_callback)
		self.menubar.add_cascade(label='Project', menu=self.project_menu, underline=0)		

		# Empower Project Menu
		self.empower_project_menu = tk.Menu(self.empower_menubar, tearoff=0)
		self.empower_project_menu.add_command(label='Insert Empower Design', command=self.insert_empower_design)
		self.empower_project_menu.add_command(label='Insert Empower 3D Layout Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert 3D Extractors Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert 2D Extractors Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert Circuits Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert Circuits Netlist', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert EMITS Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert Heaviside 3D Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert Heaviside 2D Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert RMexpert Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert Heaviside Circuit Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert SimXplorer Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert Heatpack Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert Mechanical Design', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Insert Documentation Design', command=self.generic_callback)
		self.empower_project_menu.add_separator()
		self.empower_project_menu.add_command(label='Analyze All', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Submit Job...', command=self.generic_callback)
		self.empower_project_menu.add_separator()
		self.empower_project_menu.add_command(label='Project Variables...', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Datasets...', command=self.generic_callback)
		self.empower_project_menu.add_command(label='Event Callbacks...', command=self.generic_callback)
		self.empower_menubar.add_cascade(label='Project', menu=self.empower_project_menu, underline=0)
		
		# Empower Draw Menu
		self.empower_draw_menu = tk.Menu(self.empower_menubar, tearoff=0)
		self.empower_arc_menu = tk.Menu(self.empower_draw_menu, tearoff=0)
		self.empower_sweep_menu = tk.Menu(self.empower_draw_menu, tearoff=0)
		self.empower_userdefinedprimitive_menu = tk.Menu(self.empower_draw_menu, tearoff=0)
		self.empower_userdefinedmodel_menu = tk.Menu(self.empower_draw_menu, tearoff=0)
		self.empower_3dcomponentlibrary_menu = tk.Menu(self.empower_draw_menu, tearoff=0)
		self.empower_linesegment_menu = tk.Menu(self.empower_draw_menu, tearoff=0)
		
		self.empower_draw_menu.add_command(label='Line', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Spline', command=self.generic_callback)
		self.empower_draw_menu.add_cascade(label='Arc', menu=self.empower_arc_menu)
		self.empower_draw_menu.add_command(label='Equation Based Curve', command=self.generic_callback)
		self.empower_draw_menu.add_separator()
		self.empower_draw_menu.add_command(label='Rectangle', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Ellipse', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Circle', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Regular Polygon', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Equation Based Surface', command=self.generic_callback)
		self.empower_draw_menu.add_separator()
		self.empower_draw_menu.add_command(label='Box', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Cylinder', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Sphere', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Torus', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Helix', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Spiral', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Bondwire', command=self.generic_callback)
		self.empower_draw_menu.add_separator()
		self.empower_draw_menu.add_command(label='Sweep', command=self.generic_callback)
		self.empower_draw_menu.add_separator()
		self.empower_draw_menu.add_command(label='User Defined Primitive', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='User Defined Model', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='3D Component Library', command=self.generic_callback)
		self.empower_draw_menu.add_separator()
		self.empower_draw_menu.add_command(label='Plane', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='Point', command=self.generic_callback)
		self.empower_draw_menu.add_separator()
		self.empower_draw_menu.add_command(label='Line Segment', command=self.generic_callback)
		self.empower_draw_menu.add_separator()
		self.empower_draw_menu.add_command(label='Region', command=self.generic_callback)
		self.empower_draw_menu.add_command(label='SubRegion', command=self.generic_callback)
		self.empower_menubar.add_cascade(label='Draw', menu=self.empower_draw_menu, underline=0)

		# Empower Help Menu

		#TODO: First 'empower' before help folder should be 'NashEDT'
		self.empower_help_menu = tk.Menu(self.empower_menubar, tearoff=0)
		self.empower_reference_menu = tk.Menu(self.empower_help_menu, tearoff=0)

		self.empower_help_menu.add_command(label='Empower Help', command=self.generic_callback)
		self.empower_help_menu.add_command(label='Empower Scripting Help', command=self.generic_callback)
		self.empower_help_menu.add_command(label='Empower Getting Started Guides', command=self.generic_callback)
		self.empower_help_menu.add_cascade(label='Empower PDFs', menu=self.empower_reference_menu)
		self.empower_help_menu.add_separator()
		self.empower_help_menu.add_command(label='Nash Customer Support', command=self.generic_callback)
		self.empower_help_menu.add_separator()
		self.empower_help_menu.add_command(label='What\' New in this Release...', command=self.generic_callback)
		self.empower_help_menu.add_command(label='3D Geometry Kernel...', command=self.generic_callback)
		self.empower_help_menu.add_command(label='Nash Product Improvement Program...', command=self.generic_callback)
		self.empower_help_menu.add_command(label='Licensing Settings...', command=self.generic_callback)
		self.empower_help_menu.add_separator()
		self.empower_help_menu.add_command(label='Nash Innovation Space', command=self.generic_callback)
		self.empower_help_menu.add_command(label='Nash Learning Hub', command=self.generic_callback)
		self.empower_help_menu.add_separator()		
		self.empower_help_menu.add_command(label='About Nash Electronics Desktop...', command=self.generic_callback)
		self.empower_menubar.add_cascade(label='Help', menu=self.empower_help_menu, underline=0)
		
		self.empower_reference_menu.add_command(label="Mie Scattering v1", command=lambda: self.open_reference("empower/help/empower/mie_v1/mie_scatter_v1.pdf"))
		self.empower_reference_menu.add_command(label="Mie Scattering v2", command=lambda: self.open_reference("empower/help/empower/mie_v2/mie_scatter_v2.pdf"))

		self.config(menu=self.menubar)
		
	def create_windows(self):
		self.upper_frame = ttk.Frame(self) # Ribbon
		self.lower_frame = ttk.Frame(self) # Display
		self.left_frame = ttk.Frame(self.lower_frame)
		self.right_frame = ttk.Frame(self.lower_frame)
		self.upper_left_frame = ProjectManager(self.left_frame) # Project Manager - Treeview
		self.lower_left_frame = Properties(self.left_frame) # Properties - Table
		self.upper_right_frame = Display(self.right_frame) # Display / Work area
		self.lower_right_frame = Status(self.right_frame) # Status
		
		self.left_display_frame = ttk.Frame(self.upper_right_frame) # Object hierarchy - Treeview
		self.right_display_frame = ttk.Frame(self.upper_right_frame) # Drawing window
		self.left_status_frame = ttk.Frame(self.lower_right_frame) # Message Manager - Treeview
		self.right_status_frame = ttk.Frame(self.lower_right_frame) # Progress - stacked Progressbar
		
		self.upper_frame.pack()
		self.lower_frame.pack()
		self.left_frame.pack(side=tk.LEFT)
		self.right_frame.pack(side=tk.LEFT)
		
		self.upper_left_frame.pack()
		self.lower_left_frame.pack()
		self.upper_right_frame.pack()
		self.lower_right_frame.pack()
		self.left_display_frame.pack(side=tk.LEFT)
		self.right_display_frame.pack(side=tk.LEFT)
		self.left_status_frame.pack(side=tk.LEFT)
		self.right_status_frame.pack(side=tk.LEFT)

	def generic_callback(self):
		pass
		
	def switch_window_mode(self, mode):
		if mode == WindowMode.BASE:
			if self.window_mode is not WindowMode.BASE:
				self.window_mode = WindowMode.BASE
				self.config(menu=self.menubar)
		elif mode == WindowMode.EMPOWER:
			if self.window_mode is not WindowMode.EMPOWER:
				self.window_mode = WindowMode.EMPOWER
				self.config(menu=self.empower_menubar)
			
	def insert_empower_design(self):
		self.switch_window_mode(mode=WindowMode.EMPOWER)
	
	def open_reference(self, filename):
		if os.name == 'nt':  # Windows
			os.startfile(filename)
		elif sys.platform == 'darwin':	# macOS
			subprocess.Popen(['open', filename])
		else:  # Linux
			subprocess.Popen(['xdg-open', filename])


		
	
