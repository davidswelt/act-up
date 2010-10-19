
;;; push-last
;;;
;;; (defmacro push-last (item place)
;;; 
;;; item anything
;;; place a Lisp place
;;;
;;; push-last postpends item to the list that is stored in place and stores the 
;;; resulting list in place.
;;;
;;; returns place. 
;;;

(defmacro push-last (item place)
  `(setf ,place (nconc ,place (list ,item))))



;;; These are the correlation and deviation functions.
;;; Unless *actr-stats-show-results* is non-nil, they DO NOT print
;;; the results - they just return them.

(defparameter *actr-stats-show-results* t)

(defstruct data labels array)

(defmacro /-safe (number &rest dividers)
  `(/ ,number ,@(let ((max nil))
                  (dolist (divider dividers max)
                    (push-last `(if (zerop ,divider) 1 ,divider) max)))))

(defun numbers-list (structure)
  (let ((list nil))
    (when (data-p structure) (setf structure (data-array structure)))
    (cond ((arrayp structure)
           (dotimes (i (array-total-size structure))
             (let ((data (row-major-aref structure i)))
               (when (numberp data) (push data list)))))
          ((listp structure)
           (dolist (data structure)
             (cond ((listp data)
                    (setf list (append (nreverse (numbers-list data)) list)))
                   ((numberp data)
                    (push data list)))))
          ((numberp structure)
           (push structure list))
          (t (format t "~&UNKNOWN DATA FORMAT ~S NOT COMPATIBLE WITH NUMBERS LIST.~%"
                     structure)))
    (nreverse list)))

(defun square-data (x)
  (* x x))

(defun sum-list (list)
  (let ((sum 0.0))
    (dolist (data list sum)
      (incf sum data))))

(defun square-list (list)
  (let ((sum 0.0))
    (dolist (data list sum)
      (incf sum (square-data data)))))

(defun product-list (list1 list2)
  (let ((sum 0.0))
    (loop
      (when (or (null list1) (null list2)) (return sum))
      (incf sum (* (pop list1) (pop list2))))))

(defun mean-deviation (results data &key (output t))
  (let* ((results-list (numbers-list results))
         (data-list (numbers-list data))
         (n (min (length results-list) (length data-list)))
         (opened nil))
    (cond ((or (stringp output) (pathnamep output))
           (setf output (open output :direction :output :if-exists :append
                              :if-does-not-exist :create))
           (setf opened t))
          ((not (or (streamp output) (null output) (eq output t)))
           (format t "~&OUTPUT ARGUMENT ~S TO MEAN-DEVIATION IS NOT VALID.~%"
             output)
           (format t "IT MUST BE A STRING, PATHNAME, STREAM, T OR NIL.~%")
           (setf output t)))
    
    (unless (= (length results-list) (length data-list))
      (format t "~&ERROR: ~S AND ~S DO NOT HAVE THE SAME NUMBER OF NUMBERS.~%"
              results data))
    (let ((result (sqrt (/ (+ (square-list results-list) (square-list data-list)
                              (* -2.0 (product-list results-list data-list)))
                           n))))
      (if *actr-stats-show-results* (format output "~&MEAN DEVIATION: ~6,3F~%" result))
      (when opened (close output))
      
      result)))

(defun correlation (results data &key (output t))
  (let* ((results-list (numbers-list results))
         (data-list (numbers-list data))
         (n (min (length results-list) (length data-list)))
         (average-results (/-safe (sum-list results-list) n))
         (average-data (/-safe (sum-list data-list) n))
         (opened nil))
    (cond ((or (stringp output) (pathnamep output))
           (setf output (open output :direction :output :if-exists :append
                              :if-does-not-exist :create))
           (setf opened t))
          ((not (or (streamp output) (null output) (eq output t)))
           (format t "~&OUTPUT ARGUMENT ~S TO CORRELATION IS NOT VALID.~%"
             output)
           (format t "IT MUST BE A STRING, PATHNAME, STREAM, T OR NIL.~%")
           (setf output t)))
    (unless (= (length results-list) (length data-list))
      (format t "~&ERROR: ~S AND ~S DO NOT HAVE THE SAME NUMBER OF NUMBERS.~%"
              results data))
    (let ((result (/-safe (- (/-safe (product-list results-list data-list) n)
                       (* average-results average-data))
                    (* (sqrt (- (/-safe (square-list results-list) n)
                                (square-data average-results)))
                       (sqrt (- (/-safe (square-list data-list) n)
                                (square-data average-data)))))))
      (if *actr-stats-show-results* (format output "~&CORRELATION: ~6,3F~%"
					    result))
    (when opened (close output))
      result)))


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
