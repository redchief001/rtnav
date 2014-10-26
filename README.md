# rtnav.el

An Emacs extension providing alternate source tree navigation and task list generation
based on commonly used annotations.

## Description:

Please note that this README is a very rough draft as is the tool that it
describes. As everything here is a work in progress, I welcome any and all
suggestions on how to make things better.


This tool is a minor mode for Emacs that enables the user to make use of
annotations within comments left by developers as markers and reminders to
navigate to areas of code that may at some point have been a priority. This is just
another alternative to the many excelent option that Emacs users already have for
navigating their source code.


When the user attempts to enter rtnav-mode, the mode will prompt the user for a
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

- Navigate to the list item under point -> `n`
- Save task list to file                -> `s`
