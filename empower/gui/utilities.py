import tkinter as tk
from tkinter import ttk


class RenderWindow(ttk.Frame):
	def __init__(self, parent, controller=None):
		super().__init__(parent)
		self.canvas_width = 480
		self.canvas_height = Display.DISPLAY_HEIGHT
		self.canvas_background = "white"
		self.canvas = tk.Canvas(
			self,
			width=self.canvas_width,
			height=self.canvas_height,
			bg=self.canvas_background
		)
		self.canvas.grid(row=0, column=0, sticky="nsew")


class Browser(ttk.Frame):
	def __init__(self, parent, controller=None):
		super().__init__(parent)
		self.tree = ttk.Treeview(self)
		self.tree.grid(row=0, column=0, sticky="nsew")


class Display(ttk.Frame):
	DISPLAY_HEIGHT = 480
	def __init__(self, parent, controller=None):
		super().__init__(parent)
		self.browser = Browser(self)
		self.render_window = RenderWindow(self)
		
		self.browser.grid(row=0, column=0, sticky="ns")
		self.render_window.grid(row=0, column=1, columnspan=2, sticky="ns")


class ProjectManager(ttk.Frame):
	def __init__(self, parent, controller=None):
		super().__init__(parent)
		self.frame = ttk.Labelframe(self, text="Project Manager")
		self.tree = ttk.Treeview(self.frame)

		self.tree.grid(row=0, column=0, sticky="nsew")
		self.frame.grid(row=0, column=0, sticky="nsew")


class Properties(ttk.Frame):
	def __init__(self, parent, controller=None):
		super().__init__(parent)
		self.columns = ("Name", "Value", "Unit", "Evaluated Value")
		self.frame = ttk.Labelframe(self, text="Properties")
		self.tree = ttk.Treeview(self.frame, column=self.columns, show="headings")
		
		self.tree.column("Name", width=100, anchor="w")
		self.tree.column("Value", width=80, anchor="e")
		self.tree.column("Unit", width=60, anchor="center")
		self.tree.column("Evaluated Value", width=80, anchor="e")
		
		for column in self.columns:
			self.tree.heading(column, text=column)
			
		self.vertical_scroll = ttk.Scrollbar(self.frame, orient="vertical", command=self.tree.yview)
		self.horizontal_scroll = ttk.Scrollbar(self.frame, orient="horizontal", command=self.tree.xview)
		self.tree.configure(yscrollcommand=self.vertical_scroll.set,
			xscrollcommand=self.horizontal_scroll.set)
		
		self.tree.grid(row=0, column=0)
		self.vertical_scroll.grid(row=0, column=1)
		self.horizontal_scroll.grid(row=1, column=0)
		self.frame.grid(row=0, column=0, sticky="nsew")


class MessageManager(ttk.Frame):
	def __init__(self, parent, controller=None):
		super().__init__(parent)
		self.frame = ttk.Labelframe(self, text="Message Manager")
		self.tree = ttk.Treeview(self.frame)
		
		self.tree.pack()
		self.frame.pack()


class Progress(ttk.Frame):
	def __init__(self, parent, controller=None):
		super().__init__(parent)
		self.frame = ttk.Labelframe(self, text="Progress")
		
		self.frame.pack()

class Status(ttk.Frame):
	def __init__(self, parent, controller=None):
		super().__init__(parent)

