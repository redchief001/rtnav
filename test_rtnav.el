;;; test_rtnav.el --- The rtnav-mode test suite
;;;
;;; Commentary:
;;;
;;;
;;; This is the test suite for the eltodo tool and associated functions.  The
;;; test fixtures for the more complex tests are at the bottom in a labeled
;;; section.
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
;;; This code is not part of Emacs!
;;;
;;; Code:

(ert-deftest rtnav-test-gen-list-buffer ()
  "Tests the generation of the list buffer."
  (rp-gen-list-buffer)
  (should
   (get-buffer "Todo.list"))
  (should (equal
           (buffer-name (current-buffer)) "Todo.list")))

(ert-deftest rtnav-test-parse-tree ()
  "Tests the function of the tree parser.

This tests to ensure that the the rp-parse-tree function actually grabs the
file names of just source code files in the working tree. The list that gets
generated by the rp-parse-tree function is composed individual lists that
contain the name of the file that the annotations come from, the line number
of the annotation, and the text of the annotation as a string in that order."
  ;; First things first!
  (rp-dummy-tree-setup)
  (let (annotationList)
    (cd "testDirectory")
    (setq annotationList (rp-parse-tree))
    ;; Fail if the list that is returned is empty.
    (should (annotationList))
    ;; The entry from the dotfile should not be captured in the returned list.
    ;; Since we know the order that the items will go by in the list, we can
    ;; cheat a little and compare the first item to a regex.
    (dolist (list annotationList)
      (should-not (string-match "\..*" (car item))))))

(ert-deftest rtnav-test-get-file-line ()
  "Test if the list that returns is populated with two values."

  (let (string1 string2 returnVal1 returnVal2)
    (setq string1
          "This is a line with a Todo.list file name and a 89 line number")
    (setq string2 "This is a line without the file name and line number")
    (with-temp-buffer
      (insert string2)
      (insert "\n")
      (insert "\n")
      (insert string1)
      ;; TODO: figure out how to test the failure of the search.
      ;;(goto-char (point-min))
      ;; On this line nothing should be in the returned list.
      ;;(setq returnVal1 (rp-get-file-line))
      ;;should-not (returnVal)
      (goto-char (point-max))
      (beginning-of-line)
      ;; On this line the returned list should not be empty.
      (setq returnVal2 (rp-get-file-line))
      (should returnVal2)
      ;; Test that the return values are correct.
      (should (string= (pop returnVal2) "Todo.list"))
      (should (string= (pop returnVal2) "89")))))


(ert-deftest rtnav-test-open-file-other-window ()
  "Tests that the correct file was actually opened in a new window.

This test needs to determine if a new buffer was actually created, if the name
of the new buffer matches up to the argument name, and if only one buffer
with a name that matches the file is open even after several calls to the
function with the same filename argument. If any of these three condtions is
false, then the test should fail."
  (interactive)
  (let (targetBufferName targetBuffer)
    (setq targetBufferName "temp_file")
    (setq targetBuffer (get-buffer-create targetBufferName))
    (unless (buffer-live-p targetBuffer)
      (progn
        (set-buffer targetBuffer)
        (if (file-exists-p targetBufferName)
            (with-current-buffer
                (find-file-noselect targetBufferName))
          (with-current-buffer
              (find-file-noselect targetBufferName))))
      (set-buffer targetBuffer))
    (should (buffer-live-p targetBuffer)) ;; It should exist.
    (should (buffer-name targetBuffer)) ;; It should be the correct buffer.
    ;; This is the teardown for the test fucntion.
    (kill-buffer targetBuffer)))

(ert-deftest rtnav-test-move-mark-to-loc ()
  "Tests whether the mark in the buffer is in the right location.

This test sets a buffer name and an integer and checks the location of the
mark in that specific buffer to ensure it is at the pre-specified location."
  (interactive)
  (let (targetBuffer targetLocation loopSentry testString)
    (setq targetLocation 15)
    (setq testString "This is a test! ")
    (setq loopSentry 0)
    (setq targetBuffer (get-buffer-create "targetBuffer"))
    (with-current-buffer targetBuffer
      ;; Loop over testString, repeatedly inserting it in the buffer.
      (while (< loopSentry 30)
        (progn
          (insert testString)
          (insert "\n")
          (setq loopSentry (+ loopSentry 1))))
      ;; Place the point back at the start of the buffer.
      (goto-char (point-min))
      ;; Now call the code under test with the desired arguments.
      (rp-move-mark-to-loc targetBuffer targetLocation)
      ;; Test if the point is in the right location.
      (should (= (point) targetLocation)))
    ;; Kill the target buffer when finished.
    (kill-buffer targetBuffer)))

;; -------------- Test fixtures --------------- ;;

;; This is a collection of functions that create a working tree with dummy files
;; and content for the purposes of testing. This code was written to cut down on
;; the clutter in the test cases themselves.

(defun rtnav-dummy-tree-setup ()
  "This is a test fixture that creates a dummy working tree with source files."
  (interactive)
  (let (origin targetDir targetSubdir)
    (setq origin default-directory)
    (setq targetDir "testDirectory")
    (setq targetSubdir "testSubdirectory")
    (if (file-exists-p targetDir)
        (delete-directory targetDir 1))
    (if (make-directory targetDir)
        (message "Root test directory successfully created!")
      (message "There was a problem creating the root directory!"))
    (cd targetDir)
    (if (make-directory targetSubdir)
        (message "Test sub-directory successfully created!")
      (message "There was a prblem crating the sub-directory!"))
    (cd "..")
    ;; Call the rp-place-files-in-tree fucntion here.
    (if (rp-place-files-in-tree targetDir targetSubdir)
        (progn
          (message "Test directory generated successfully")
          (rp-populate-files-with-dummy-data targetDir targetSubdir))
      (message "There was a problem generating the test tree!"))
    (cd origin)))

(defun rtnav-place-files-in-tree (dir1 dir2)
  "This test fixture places test files with test data in the passed in directories.

This function takes a directory as a paramater and places files of various types
at all of the levels of the tree to simulate a project source tree."
  (interactive)
  (cd dir1)
  (shell-command "touch Class.cpp && touch Class.h")
  (cd dir2)
  (shell-command "touch somescript.sh && touch .somedotfilerc")
  (cd "..")
  (cd ".."))

(defun rtnav-populate-files-with-dummy-data (dir1 dir2)
  "This test fixture places annotations and other text in files in treeRoot.

This function can be called from any other fixture and adds annotations and
other text content to files in the given directories.  The fixture uses a temp
buffer to write the text to each file one after the other."
  (interactive)
  (let (string1 string2 string3 string4)
    (setq string1 "/* NOTE: here is some annotation text. */")
    (setq string2 "// TODO: here is some more annotation text. ")
    (setq string3 "# FIXME: here is still more annotation text.")
    ;; 3 & 4 are together!
    (setq string4 "# This is a continuation of annotation text.")
    (setq string5 "# TODO: this should not be captured! ")
    (cd dir1)
    (with-temp-buffer
      (insert string1)
      (write-file "Class.cpp")
      (erase-buffer)
      (insert string2)
      (write-file "Class.h")
      (erase-buffer)
      (cd dir2)
      (insert string3)
      (newline)
      (insert string4)
      (write-file "somescript.sh")
      (erase-buffer)
      (insert string5)
      (write-file ".somedotfilerc"))
    (cd "..")))

(defun rtnav-dummy-tree-teardown (dir)
  "This fixture tears down the test DIR."
  (interactive)
  (if (delete-directory dir)
      (message "Cleanup successful!")
    (message "There was a problem with cleanup!")))


;;; test_rtnav.el ends here
