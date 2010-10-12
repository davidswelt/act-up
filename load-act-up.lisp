;; ACT-UP load code
;; Adapted from code written by Dan Bothell, Carnegie Mellon University   (load-act-r-6.lisp)

;; (load "load-act-up.lisp")





;;; The Windows version of SBCL doesn't properly handle wild cards in the
;;; directory command so this hacks around that for now sufficiently to load
;;; the ACT-R files...

#+(and :sbcl :win32) 
  (defpackage "COMMON-LISP-USER" (:shadow "DIRECTORY"))
#+(and :sbcl :win32) 
(eval-when (:load-toplevel :execute)
  (defun directory (pathname &key)
    ;(format t "Calling the new directory for ~S~%" pathname)
    (if (not (string= (pathname-type pathname) "*"))
        (let* ((new-path (make-pathname :host (pathname-host pathname)
                                        :device (pathname-device pathname)
                                        :directory (pathname-directory pathname)
                                        :defaults "*.*"))
               (new-val (cl::directory new-path))
               (res (remove-if-not (lambda (x) 
                                     (string-equal (pathname-type x) (pathname-type pathname)))
                                   new-val)))
          ;(format t "Returning ~S from directory of new-path: ~s which returns ~s.~%" res new-path new-val)
          res)
      (cl::directory pathname))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create the logical host "ACT-UP1" relative to the current location

#+:allegro (setf (logical-pathname-translations "ACT-UP1")
             (list (list "**;*.*" (let ((name (namestring *load-truename*))
                                        (file (file-namestring *load-truename*)))
                                    (subseq name 0 (- (length name) (length file)))))))


#+:digitool (setf (logical-pathname-translations "ACT-UP1")
  (list (list "**;*.*" (concatenate 'string
                         (host-namestring *load-truename*)
                         (directory-namestring *load-truename*) "**:"))))

#+:openmcl (setf (logical-pathname-translations "ACT-UP1")
  (list (list "**;*.*" (concatenate 'string
                         (host-namestring *load-truename*)
                         (directory-namestring *load-truename*) "**/"))))

#+:lispworks (setf (logical-pathname-translations "ACT-UP1")
               (list (list "**;*.*" 
                           (concatenate 'string
                             (format nil "~A" (make-pathname
                                               :host 
                                               (pathname-host *load-truename*)
                                               :directory 
                                               (pathname-directory 
                                                *load-truename*))) 
                             "**/*.*"))))

;; just copied the lispworks one for now...
#+:cmu (setf (logical-pathname-translations "ACT-UP1")
               (list (list "**;*.*" 
                           (concatenate 'string
                             (format nil "~A" (make-pathname
                                               :host 
                                               (pathname-host *load-truename*)
                                               :directory 
                                               (pathname-directory 
                                                *load-truename*))) 
                             "**/*.*"))))

#+(or :clisp :sbcl) (setf (logical-pathname-translations "ACT-UP1")
                      `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" *load-truename*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define the file extension (the pathname type) for compiled and source files
;;; in the currently supported systems

(unless (boundp '*.lisp-pathname*)
  (defvar *.lisp-pathname* 
      (make-pathname :type "lisp")))

(unless (boundp '*.fasl-pathname*)
  (defvar *.fasl-pathname* 
      (let ((type (pathname-type (compile-file-pathname "dummy.lisp"))))
        (if (and type (not (string-equal type "lisp"))) 
          (make-pathname :type type)
        
        ;; If it can't figure it out automatically resort to predefined value
        
        #+:allegro (make-pathname :type "fasl")
        #+:sbcl (make-pathname :type "fasl")
        #+:clisp (make-pathname  :type "fas")
        #+(and :linux :cmu) (make-pathname :type "x86f")
        #+(and :ppc :cmu) (make-pathname :type "ppcf")
        #+(and :lispworks :win32 (not :lispworks5)) (make-pathname :type "fsl")
        #+(and :lispworks :win32 :lispworks5) (make-pathname :type "ofasl")
        #+(and :lispworks :unix (not :macosx) (not :lispworks5)) (make-pathname :type "ufsl")
        #+(and :lispworks :unix (not :macosx) :lispworks5) (make-pathname :type "ufasl")
        #+(and :lispworks :macosx (not :x86)) (make-pathname :type "nfasl")
        #+(and :lispworks :macosx :x86) (make-pathname :type "xfasl")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define some functions for compiling and loading files

;;; compile-and-load (pathname)
;;;
;;; pathname a file pathname (or pathname string) if the file already
;;;          has a type specified, then it is ignored and the defaults
;;;          of lisp for source and system-dependent binary types are
;;;          used.
;;; 
;;; If a source file (.lisp) exists for the specified pathname then if there
;;; is no binary file (determined by *.fasl-pathname*), the binary is
;;; older than the source file, or the feature :act-r-recompile is set then 
;;; compile the source file into a binary and load it.  
;;;
;;; Based on the smart-load function from the ACT-R loader.


;;; Specific loader for the newer MCL 5/5.1

#+(and :ccl-4.3.5 :ccl-5.0) 
(defun compile-and-load (pathname)
  (when (pathname-type pathname) ;; throw away the type to allow for
                                 ;; the merging with a binary type
    (if (string-equal (pathname-type pathname) "lisp")
        (setf pathname (make-pathname :host (pathname-host pathname)
                                      :directory (pathname-directory pathname)
                                      :device (pathname-device pathname)
                                      :name (pathname-name pathname)))
      (error "To compile a file it must have a .lisp extension")))
  
  (let* ((srcpath (merge-pathnames pathname *.lisp-pathname*))
         (binpath (merge-pathnames pathname *.fasl-pathname*)))
    (unless (probe-file srcpath)
      (error "File ~S does not exist" srcpath))
    (when (or (member :actr-recompile *features*)
              (not (probe-file binpath))
              (> (file-write-date srcpath) (file-write-date binpath)))
      (compile-file srcpath :output-file binpath :external-format :unix))
    (load binpath)))
  
#-(and :ccl-4.3.5 :ccl-5.0) 
(defun compile-and-load (pathname)
  (when (pathname-type pathname) ;; throw away the type to allow for
                                 ;; the merging with a binary type
    (if (string-equal (pathname-type pathname) "lisp")
        (setf pathname (make-pathname :host (pathname-host pathname)
                                      :directory (pathname-directory pathname)
                                      :device (pathname-device pathname)
                                      :name (pathname-name pathname)))
      (error "To compile a file it must have a .lisp extension")))
  
  (let* ((srcpath (merge-pathnames pathname *.lisp-pathname*))
         (binpath (merge-pathnames pathname *.fasl-pathname*)))
    (unless (probe-file srcpath)
      (error "File ~S does not exist" srcpath))
    (when (or (member :actr-recompile *features*)
              (not (probe-file binpath))
              (> (file-write-date srcpath) (file-write-date binpath)))
      (compile-file srcpath :output-file binpath))
    (load binpath)))
  


;;; SMART-LOAD      [Function]
;;; Date        : 99.12.21
;;; Description : Loads binary version of a specified file.  Of course, the 
;;;             : said binary version might not exist or be older than the 
;;;             : source version, in which case the source file is compiled 
;;;             : before loading.
;;;             : Updated to add an option parameter to determine whether
;;;             : to just warn of a missing file or to throw an error.


(defun smart-load (this-files-dir file &optional (error? nil))
  "Loads binary <file> in directory <this-files-dir> or compiles and loads 
   source version"
  (let* ((srcpath (merge-pathnames  
                   (merge-pathnames file *.lisp-pathname*)
                   this-files-dir))
         )
    (if (not (probe-file srcpath))
        (if error? 
            (error "File ~S does not exist" srcpath)
          (format *error-output* "File ~S does not exist" srcpath)))
    (compile-and-load srcpath)))


;;; require-compiled (code-module pathname)
;;;
;;; code-module is a string that designates some code that needs to be loaded
;;;             which should have a corresponding (provide code-module) in it
;;; pathname is the pathname to where that code-module can be found (including
;;;          the file's name).
;;;
;;; Similar to the function require this will determine if the requested
;;; code-module has been loaded and if not will compile and load the file
;;; specified by pathname.  This differs from the normal require function
;;; in that the pathname is mandatory and it does not search through any
;;; implementation defaults to find the code-module.  However, it does still
;;; depend on a provide call existing in the code-module file so that
;;; it only loads the necessary file the first time it is required.

(defmacro require-compiled (code-module pathname)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (member ,code-module *modules* :test #'string=)
       (compile-and-load (translate-logical-pathname ,pathname)))))                       

(defun actup-load (file &optional (dir "core"))
  (format t "Loading ~s.~%" file)
  (load (merge-pathnames file (translate-logical-pathname (format nil "ACT-UP1:~a;" dir)))))
(export '(actup-load))

(actup-load "act-up.lisp")

(actup-load "actr-stats.lisp" "util")

;;(require "act-up" "/act-up.lisp")

(use-package :act-up)



(format t "~%######### Loading of ACT-UP is complete #########~%")



#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
