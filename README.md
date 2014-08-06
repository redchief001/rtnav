# rtnav.el

An Emacs extension providing alternate source tree navigation and task list generation
based on commonly used annotations.

## Description:

Please note that this README is a very rough draft as is the tool that it
describes. As everything here is a work in progress, I welcome any and all
suggestions on how to make things better.


This tool is a minor mode for Emacs that enables the user to make use of the
output file generated organically to navigate to annotations within the users
source tree.


When the user attempts to enter eltodo-mode, the mode will prompt the user for a
directory to process (the current one being the default) and will compile a list
of annotations found in source code files along with the accompanying text.


The user can then navigate directly to the list items by invoking a key command
while the point is on one of the list items. Doing this will open the file from
where the annotation originated in a new window with the point at the beginning
of the selected annotation.


Once the user has completed the task to which the annotation referred, they
can delete the annotation and save the buffer containing the source code, which
in turn updates the list contents in the task list file.

## Minor mode keybindings:

All keybindings for rtnav-mode start with `C-c C-l` then the additional keystroke
or combination that follows.

- Navigate to the list item under point -> `C-c C-l n`
- Save task list to file                -> `C-c C-l s`
