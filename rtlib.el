;;;  scratchpad.el --- incubation for rp tool functions
;;;
;;; Commentary:
;;;
;;;  These are some functions that could be used in the rp tool.
;;;
;;;  None of this code is part of Emacs!
;;;
;;; code:

(defun rp-gen-list-buffer ()
  "Create a new buffer to hold the parsed annotations and meta-data."
  (interactive)
  (let (targetBuffer)
    (setq targetBuffer
          (get-buffer-create "Todo.list"))
    (set-buffer targetBuffer)))

;; ------------

(defun rp-search-file-for-annot ()
  "Search the passed file for annotations returning a two item results list.

Takes a file name as an argument and searches that file for annotations within
comments.  The comment deliminators are determined based on file extension.
Returns a list that contains sub-lists that have an annotations start line and
its full text."
  )

;; ------------

(defun rp-parse-tree (&optional sourceTreeRoot)
  "Parse the current working directory searching for source code files.

Takes the root of the project tree as argument SOURCETREEROOT and returns a
list of absolute paths to all possible source code files in the working tree
ignoring dot files, backups, and other such trash."
  (interactive)
  (let (fileNames '() treeRoot)
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
       (add-to-list 'fileNames (directory-files-recursive ))
       ;;FIXME: fix the above function call!
       ))

;; ------------

(defun rp-get-file-line ()
  "Grabs a file name and a line number from the line at the mark.

Takes no arguments and returns a list containing a file name and a line number
if these are found in the line under point."
  (interactive)
  (save-excursion
    (save-restriction
      (let (fileName lineNo)
        ;; Narrow the region to the current line.
        (narrow-to-region (line-beginning-position)
                          (line-end-position))
        ;; Each search will be from the begining of the line. First search for
        ;; the file name, then the line number. The format of the list file has
        ;; the file name at the beginning of the line followed by the line no.
        (goto-char (point-min))
        (if (not (search-forward-regexp ".+?\\..+?\\b"))
          (message "Error: content not found!")
          (progn
            (setq fileName (thing-at-point 'filename))
            (goto-char (point-min))
            (search-forward-regexp "[0-9]")
            (setq lineNo (thing-at-point 'filename))
            ;; push the fileName and lineNo onto a list and return it.
            (setq fileAndLine (list fileName lineNo))))))))

;; ------------

(defun rp-open-file-other-window (targetFile)
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

(defun rp-move-mark-to-loc (targetBuffer markLOC)
  "Move the mark in the specified buffer to the desired location.

Takes the name of TARGETBUFFER and the desired location as MARKLOC.
If the specified buffer exists, the mark is moved to the point passed in."
  (interactive)
  (if (buffer-live-p targetBuffer)
      (with-current-buffer targetBuffer
           (goto-char markLOC))
    (message "The target buffer does not exist!")))


;; ------------------ Utility code ----------------------

;; This is my attempt at adding some more (and specialized) funcionality to the
;; already provided text editing framework. Some of this might be worth putting
;; int the init file later.


(defun directory-files-recursive (&optional treeRoot exclude)
  "Return a list of absolute path file names recursively down a directory tree.

Takes TREEROOT and EXCLUDE as arguments and recursively gathers all file names
in TREEROOT (absolute paths) excluding file names that do not match EXCLUDE
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


;;; scratchpad.el ends here
