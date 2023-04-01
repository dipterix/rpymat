from tkinter import Tk
from tkinter.filedialog import askdirectory
Tk().withdraw() # we don't want a full GUI, so keep the root window from appearing

title = "{{ title }}"
if title == "{%s}" % '''{ title }''':
  title = "Select a Folder"

message = "{{ message }}"
if message == "{%s}" % '''{ message }''':
  message = "Select a Folder"

initialdir = "{{ normalizePath(getwd()) }}"
if initialdir == "{%s}" % '''{ normalizePath(getwd()) }''':
  import os
  initialdir = os.getcwd()

path = askdirectory(title=title, message=message, initialdir=initialdir)
print("RPYMAT_RESULT_START")
print(path)
print("RPYMAT_RESULT_END")
