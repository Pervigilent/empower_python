import tkinter as tk
from tkinter import ttk

class MainWindow(tk.Tk):
	def __init__(self):
		super().__init__()
		self.title("Nash Electronic Desktop")
		self.geometry("1080x720")
		self.create_menu()

	def create_menu(self):
		pass
