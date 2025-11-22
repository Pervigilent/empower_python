import tkinter as tk
from tkinter import ttk

class MainWindow(tk.Tk):
	def __init__(self):
		super().__init__()
		self.title("Nash Electronics Desktop")
		self.geometry("1080x720")
		self.create_menu()

	def create_menu(self):
		self.menubar = tk.Menu(self)
		self.empower_menubar = tk.Menu(self)
		
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
		
		self.config(menu=self.menubar)

	def generic_callback(self):
		pass
