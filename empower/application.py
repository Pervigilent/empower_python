from empower.gui.mainwindow import MainWindow
from empower.ansys import AnsysParser

import xml.etree.ElementTree as ET
import os
import re
import enum
from tkinter import messagebox


class ProjectType(enum.Enum):
    HFSS = 1


class Application:
    def __init__(self):
        self.projects = []
        self._observers = []
        
        self.main_window = MainWindow(self)
        self.main_window.mainloop()
        
    def add_observer(self, function):
        self._observers.append(function)
    
    def get_projects(self):
        return self.projects
    
    def notify(self):
        for function in self._observers:
            function()
        
    def read(self, filename):
        if not os.path.exists(filename):
            messagebox.showerror("Error", "Selected file does not exist.")
            return
        self.projects.append(ProjectFile(filename))
        self.notify()
            
    def save(self):
        pass
        
    def save_as(self, filename):
        active_project = None #TODO: Set active project
        if self.projects:
            active_project = self.projects[0]
            extension = os.path.splittext(filename)[1].lower()
            try:
                if ext == ".emp":
                    active_project.set_filename(filename)
                    active_project.tree.write(filename, encoding="utf-8", xml_declaration=True)
                elif ext == ".aedt":
                    with open(filename, "w") as f:
                        tree = active_project.get_tree()
                        root_element = tree.getroot()
                        root_node = AnsysParser.convert_xml(root_element)                        
                        AnsysParser.write(node=root_node, file=f)
                else:
                    messagebox.showerror("Unsupported file type", "Only supports .emp and .aedt")
            except Exception as e:
                messagebox.showerror("Could not save file", str(e))


class Project:
    def __init__(self, element):
        self.element = element
        self.name = element.get("Name")
        
    def get_name(self):
        return self.name


class ProjectFile:
    def __init__(self, filename):
        self.tree = None
        self.root = None
        self.projects = []
        self.content = None
        self.filename = filename
        basename = os.path.basename(filename)
        self.name, extension = os.path.splitext(basename)

        with open(filename, 'r') as file:
            self.content = file.read()
        try:
            self.tree = ET.parse(filename)
            self.root = self.tree.getroot()
            self.parse(tree=self.tree)
        except ET.ParseError as e:
            messagebox.showerror("File Parse Error", f"Failed to read file:\n{e}")
        except Exception as e:
            messagebox.showerror("Error", f"Unexpected error:\n{e}")

    def create_tree(self): # Create tree from projects?
        self.root = ET.Element('AnsoftProject')
        for project in self.projects:
            ET.SubElement(self.root, 'HFSSModel', {'Name' : project.name})
        self.tree = ET.ElementTree(self.root)

    def get_tree(self):
        self.create_tree()
        return self.tree
        
    def parse(self, tree):
        #self.tree = tree
        #self.root = self.tree.getroot()
        #self.projects = []
        for model in self.root.iter("HFSSModel"):
            self.projects.append(Project(model))
            
    def get_name(self):
        if self.filename is not None:
            basename = os.path.basename(self.filename)
            return os.path.splitext(basename)[0]
            
    def get_filename(self):
        return self.filename
            
    def set_filename(self, filename):
        self.filename = filename
        basename = os.path.basename(filename)
        self.name, extension = os.path.splitext(basename)
    
'''
import xml.etree.ElementTree as ET

# 1. Create the root element
root = ET.Element('person_data')

# 2. Add a child element 'person' to the root
person = ET.SubElement(root, 'person', {'id': '1'}) # Added attribute 'id'

# 3. Add child elements 'name' and 'age' to 'person'
name = ET.SubElement(person, 'name')
age = ET.SubElement(person, 'age')

# 4. Set text content for the child elements
name.text = 'John Doe'
age.text = '30'

# 5. Create the ElementTree instance with the root
tree = ET.ElementTree(root)

# 6. Write the tree to an XML file
# Use xml_declaration=True and encoding='utf-8' for a well-formed XML file
try:
    tree.write('person.xml', encoding='utf-8', xml_declaration=True)
except IOError as e:
    print(f"Error writing file: {e}")

# To print the XML to the console, you can use tostring()
# import xml.etree.ElementTree as ET
# print(ET.tostring(root, encoding='utf-8').decode('utf-8'))

'''

