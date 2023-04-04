import os
from tkinter import Tk
from tkinter.filedialog import askdirectory
Tk().withdraw() # we don't want a full GUI, so keep the root window from appearing

title = "{{ title }}"
if title == "{%s}" % '''{ title }''':
  title = "Select a Folder"

message = "{{ message }}"
if message == "{%s}" % '''{ message }''':
  message = "Select a Folder"

initialdir = "{{ initialdir }}"
if initialdir == "{%s}" % '''{ initialdir }''':
  initialdir = "."
else:
  initialdir = os.path.abspath(initialdir)

try:
  if os.name == "nt":
    path = askdirectory(title=title, initialdir=initialdir)
  else:
    path = askdirectory(title=title, message=message, initialdir=initialdir)
except Exception:
  path = askdirectory(initialdir=initialdir)


print("RPYMAT_RESULT_START")
print(path)
print("RPYMAT_RESULT_END")
