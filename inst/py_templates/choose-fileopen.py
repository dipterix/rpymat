from tkinter import Tk
from tkinter.filedialog import askopenfilenames, askopenfilename
Tk().withdraw() # we don't want a full GUI, so keep the root window from appearing

multiple = "{{ multiple }}"
if multiple == "TRUE":
  multiple = True
else:
  multiple = False


title = "{{ title }}"
if title == "{%s}" % '''{ title }''':
  if multiple:
    title = "Select Files"
  else:
    title = "Select a File"

message = "{{ message }}"
if message == "{%s}" % '''{ message }''':
  message = title

initialdir = "."
initialfile = "{{ initialfile }}"
if initialfile == "{%s}" % '''{ initialfile }''' or initialfile == "None":
  initialfile = None
else:
  import os
  initialfile = os.path.abspath(initialfile)
  if os.path.isdir(initialfile):
    initialdir = initialfile
    initialfile = None
  else:
    initialdir = os.path.dirname(initialfile)
    initialfile = os.path.basename(initialfile)


# -defaultextension, -filetypes, -initialdir, -initialfile, -message, -multiple, -parent, -title, -typevariable, or -command
if multiple:
  path = askopenfilenames(title=title, message=message, initialdir=initialdir, initialfile = initialfile)
else:
  path = askopenfilename(title=title, message=message, initialdir=initialdir, initialfile = initialfile)

print("RPYMAT_RESULT_START")
if isinstance(path, str):
  print(path)
else:
  for item in path:
    print(item)
print("RPYMAT_RESULT_END")


