;;; rtnav.el -- an annotation task list compiler and source tree navigator
;;;
;;; Commentary:
;;;
;;; This tool is an Emacs minor-mode that allows source tree navigation based on
;;; annotations left in comments by the developer.  The tool parses an entire
;;; working tree of source code and extracts comments delimited by annotations
;;; ("TODO", "FIXME", "XXXX", and "NOTE") left as reminders, tasks, and
;;; notes by developers in the course of writing code.
;;;
;;; The annotations in a source tree can be compiled into a list that is
;;; displayed in its own buffer and can be used to navigate directly to points
;;; of interest within the source tree.
;;;
;;; Once finished with the task, or upon making a change to the task list, the
;;; saving of the task list buffer automatically updates the list contents.
;;;
;;; Keymap:
;;;
;;; Prefix for all commands               -> C-c C-l
;;; Navigate to the list item under point -> n
;;; Save task list to file                -> s
;;;
;;; Copyright (c) 2014 James Nicholson
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;;;
;;; This code is NOT a part of Emacs!
;;;
;;; Code:


;; Extend rtnav with other types of annotations by adding to this list.
;;;###autoload
(defvar rtnav-valid-annotations (list "\\bTODO\\b" "\\bFIXME\\b" "\\bXXXX\\b" "\\bNOTE\\b")
  "The valid annotations that are parsed by rtnav.  Add to this list to extend."
  )


;;;###autoload
(define-minor-mode rtnav
  "Minor mode for alternate source tree navigation and task list generation"

  :init-value nil
  :lighter " RTNAV"
  :global t
  :keymap (let ((rtnav-map (make-sparse-keymap)))
	    (define-key rtnav-map (kbd "C-c C-l n") 'rtnav-goto-list-item)
	    (define-key rtnav-map (kbd "C-c C-l s") 'rtnav-save-task-list)
	    rtnav-map)
  (if rtnav
      (rtnav-start-setup)))


;;;###autoload
(defun enable-rtnav ()
  "Enable alternate source tree navigation and task organization."
  (interactive)
  (rtnav +1)
  )


;;;###autoload
(defun disable-rtnav ()
  "Disable alternate souce tree navigation and task organization."
  (interactive)
  (rtnav -1)
  )


(defun rtnav-start-setup ()
  "Setup fixture for rtnav.
Set up the task list buffer for display to the user."
  (let ((userInputDirectory
	 (read-file-name
	  (format "Directory to parse (default %s ) : "
		  (file-truename default-directory))))
	(treeRoot)
	(filesList)
	(taskListBuffer)
	(newTaskListWin))
    ;; If the user input is nil...
    (if (string= userInputDirectory "")
	;; Make the default directory treeRoot.
	(setq treeRoot default-directory)
      ;; Make userInputDirectory treeRoot if it is a directory (treeRoot is nil),
      ;; otherwise trow an error.
      (if (file-directory-p userInputDirectory)
	  (setq treeRoot userInputDirectory)
	(disable-rtnav)
	(error "Invalid directory entered!")))
    ;; Set up and open the task list buffer in a new window.
    (setq  taskListBuffer (rtnav-gen-list-buffer))
    (setq newTaskListWin (split-window-horizontally 90))
    (switch-to-buffer-other-window taskListBuffer)
    ;; Call the function that parses the directory tree.
    (setq filesList (rtnav-parse-tree treeRoot))
    ;; For each file returned, call the file parsing function.
    (dolist (fileListItem filesList)
      ;; Go to the end of the last entry.
      (goto-char (point-max))
      ;; For each annotation...
      (dolist (listItem (rtnav-search-file-for-annot fileListItem))
	;; For each token in the annotation...
	(insert fileListItem)
	(insert "  ")
	(dolist (itemElement listItem)
	  (insert itemElement)
	  (insert "  "))
	(newline)))
    (rtnav-uniquify-all-lines-buffer)
    (rtnav-kill-blank-lines)
    (rtnav-sort-lines)))


(defun rtnav-uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
	  (progn
	    (goto-char start)
	    (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
	(replace-match "\\1\n\\2")))))


(defun rtnav-uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (rtnav-uniquify-all-lines-region (point-min) (point-max)))


(defun rtnav-kill-blank-lines ()
  "Remove blank lines from the task list buffer."
  (interactive)
  (goto-char (point-min))
  (delete-blank-lines))



(defun rtnav-sort-lines ()
  "Sort the lines in the buffer based on the file and line number."
  (interactive)
  (goto-char (point-min))
  (sort-numeric-fields 3 (point-min) (point-max)))


(defun rtnav-goto-list-item ()
  "Go to the list item under point.
This function makes use of rtnav-get-file-and-line function to open the source
code file corresponding to the task list item in the other window for editing."
  (interactive)
  (let (infoList
	fName
	lNumber)
    ;; Call the get-file-and-line function and assign the list returned to INFOLIST.
    ;; XXXX: more random ass shit...
    (setq infoList (rtnav-get-file-line))
    (print infoList)
    ;; Assign each element to the respective variables.
    (setq fName (pop infoList))
    (setq lNumber (pop infoList))
    ;; Open the target file in the other window.
    (find-file-other-window fName)
    (set-buffer fName)
    (goto-char (point-min))
    (forward-line (1- lNumber))))


(defun rtnav-get-file-line ()
  "Grabs a file name and a line number from the line at the mark.
Takes no arguments and returns a list containing a file name and a line number
if these are found in the line under point.  TODO: make this work for all of the
text in a 'paragraph' or block of text for different screen sizes."
  (interactive)
  (save-excursion
    (save-restriction
      (let (fileName
	    lineNo
	    fileAndLine)
        ;; Narrow the region to the current line.
        (narrow-to-region (line-beginning-position)
                          (line-end-position))
        ;; Each search will be from the begining of the line. First search for
        ;; the file name, then the line number. The format of the list file has
        ;; the file name at the beginning of the line followed by the line no.
        (goto-char (point-min))
        (if (not (search-forward-regexp "^.+?\\b"))
	    (error "Content not found!")
	  (goto-char (match-beginning 0))
	  (setq fileName (thing-at-point 'filename))
	  (goto-char (match-end 0))
	  ;; (search-forward-regexp "\\b[0-9]*\\b")
	  (forward-char 7)
	  (setq lineNo (current-word))
	  ;; push the fileName and lineNo onto a list and return it.
	  (setq fileAndLine
		(append
		 (list fileName (string-to-number lineNo)) fileAndLine)))))))


(defun rtnav-save-task-list ()
  "Save the current task list to a file."
  (interactive)
  ;; TODO: implement this...
  )


(defun rtnav-gen-list-buffer ()
  "Create a new buffer to hold the parsed annotations and meta-data."
  (interactive)
  (let (targetBuffer)
    (setq targetBuffer
          (get-buffer-create "Todo.list"))
    (set-buffer targetBuffer)))


(defun rtnav-search-file-for-annot (fileName)
  "Search the passed file for annotations returning results list.
Takes FILENAME as an argument and searches that file for annotations within
comments.  This function uses \"font-lock-mode\" to determine which annotations
are inside comments and parses those into the returned structure."
  (interactive)
  (let ((masterAnnotList)
	(lineNo)
	(annotText)
	(listEntry)
	(inComment))
    ;; Open the file passed in if it is a regular file.
    (cond
     ((file-regular-p fileName)
      ;; Here is where the problem with the font lock is XXXX: loop over the files
      ;; and open each of them in their own buffer. These buffers are where the work
      ;; will take place for each file.
      (with-current-buffer (find-file-noselect fileName)
	;; Ensure that font-lock scans the whole file.
	(font-lock-fontify-buffer)
	(goto-char (point-min))
	;; Find each occurrence of the annotations in list of valid
	;; annotations.
	(dolist (annot rtnav-valid-annotations)
	  ;; For each of the annotations, search for, and collect each
	  ;; note, along with it's line number.
	  (setq case-fold-search nil) ;; Set case sensitive searching off.
	  (while (search-forward-regexp annot nil t 1)
	    ;; Check the text properties for a comment.
	    (setq inComment (nth 4 (syntax-ppss)))
	    ;; If the char at point is inside a comment...
	    (if inComment
		(progn (setq lineNo '())
		       (setq annotText '())
		       (setq listEntry '())
		       ;; Go to the beginning of the last match.
		       (goto-char (match-beginning 0))
		       ;; Grab the line number and the text from the line.
		       (setq lineNo (cons (what-line) lineNo))
		       (setq annotText (cons (thing-at-point 'line) annotText))
		       ;; Push the line number and text onto the list.
		       (setq listEntry (append  lineNo annotText))
		       (setq masterAnnotList (cons listEntry masterAnnotList))
		       (goto-char (match-end 0))
		       ;; reset the comment flag.
		       (setq inComment nil)))))))
     (t
      (error "Invalid file name!")))
    ;; Delete the buffer as cleanup.
    (kill-buffer fileName)
    ;; Return the list of annotations for the passed file.
    ;; TODO: sort the contents of the list.
    masterAnnotList))


(defun rtnav-parse-tree (&optional sourceTreeRoot)
  "Parse the current working directory searching for source code files.
Takes the root of the project tree as argument SOURCETREEROOT and returns a
list of absolute paths to all possible source code files in the working tree
ignoring dot files, backups, and other such trash."
  (interactive)
  (let ((fileNames)
	(treeRoot))
    ;; Set the treeRoot variable based on the argument
    (if sourceTreeRoot
	(progn
	  (setq treeRoot sourceTreeRoot)
	  (cd treeRoot))
      (setq treeRoot (pwd)))
    ;; Use the directory-files-recursive function to retrieve all of the file
    ;; names and save them in the fileNames list.
    ;; NOTE: the fucntion used takes mandatory arguments... may need to make
    ;; some changes to it so that it can work in this case. Question... are
    ;; the filenames retuned byt the function absolute paths? I think that
    ;; will need absolute paths for this function to be useful, so make that
    ;; happen!
    (append fileNames
	    (directory-files-recursive sourceTreeRoot "^\\..*$"))))


(defun rtnav-open-file-other-window (targetFile)
  "Open the file that is passed in as an argument.
Takes the name of the file to open as TARGETFILE.  When called, this function
opens the specified file.  If the file is already open in an existing buffer,
the function switches to that buffer."
  (interactive "p")
  ;; Check to see if there is a buffer with the name passed in and if not
  ;; create it.
  (let (targetBuffer)
    ;; Check to see if the target file exists and if it does check to see if it
    ;; already belongs to a buffer. If so, then pop to that buffer. If not
    ;; create it in another window.
    (if (file-exists-p targetFile)
        (if (buffer-live-p targetFile)
	    (set-buffer targetFile)
          (progn (find-file-other-window targetFile)
                 (set-buffer targetFile)))
      (message "Error: file not found!"))))


(defun rtnav-move-mark-to-loc (targetBuffer markLOC)
  "Move the mark in the specified buffer to the desired location.
Takes the name of TARGETBUFFER and the desired location as MARKLOC.
If the specified buffer exists, the mark is moved to the point passed in."
  (interactive)
  (if (buffer-live-p targetBuffer)
      (with-current-buffer targetBuffer
	(goto-char markLOC))
    (message "The target buffer does not exist!")))


(defun directory-files-recursive (&optional treeRoot &rest excludes)
  "Return a list of absolute path file names recursively down a directory tree.
Takes TREEROOT and EXCLUDE as arguments and recursively gathers all file names
in TREEROOT (absolute paths) excluding file names that do not match EXCLUDES
and returns them in a list to the caller.  TODO: add the exclude functionality."
  (interactive)
  (let ((myFileList)
        (myDirectories)
        (myAllFiles)
        (projectRoot))
    (if treeRoot
        (progn (setq myFileList (directory-files treeRoot))
               (setq projectRoot treeRoot))
      (progn
        (setq myFileList (directory-files default-directory))
        (setq projectRoot default-directory)))
    (cd projectRoot)
    (dolist (fileItem myFileList)
      (catch 'skip
	(dolist (excludeItem excludes)
	  (if (string-match-p excludeItem fileItem)
	      (throw 'skip nil)))
        (cond
         ((string= fileItem ".")
          (message "Skipping .")
          (throw 'skip nil))
         ((string= fileItem "..")
          (message "Skipping ..")
          (throw 'skip nil))
         ((file-directory-p fileItem)
          (setq myDirectories (cons fileItem myDirectories)))
         (t
          (setq myAllFiles (cons fileItem myAllFiles))))))
    (let (mySubDirFiles)
      (dolist (myDirectory myDirectories)
        (setq mySubDirFiles (directory-files-recursive myDirectory))
        (setq myAllFiles (append myAllFiles mySubDirFiles))))
    myAllFiles))


(provide 'rtnav)
;;; rtnav.el ends here
