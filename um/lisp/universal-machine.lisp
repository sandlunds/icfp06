;;;; An implementation of the Universal Machine.
;;;; Specification: http://www.boundvariable.org/um-spec.txt

(defpackage #:universal-machine
  (:use #:cl)
  (:export #:start))

(in-package #:universal-machine)

;; We are dealing with 32-bit unsigned integers.
(deftype uint32 () '(unsigned-byte 32))

(defmacro wrap-around (n)
   `(logand #xFFFFFFFF ,n))

(defun load-program (filename)
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (when (/= 0 (mod (file-length in) 4))
      (error "The file does not contain an even number of 32-bit words."))
    ;; Read the program big endian style.
    (let ((program (make-array (/ (file-length in) 4) :element-type 'uint32)))
      (do ((index 0 (1+ index)))
          ((>= index (length program)))
        (let* ((b1 (read-byte in))
               (b2 (read-byte in))
               (b3 (read-byte in))
               (b4 (read-byte in)))
          (setf (elt program index)
                (logior (ash b1 24)
                        (ash b2 16)
                        (ash b3 8)
                        b4))))
      program)))

(defun main-loop (allocated-mem regs free-indices pc)
  (declare (optimize (speed 3) (safety 0)))
  (declare (uint32 pc)
           ((simple-array uint32) regs)
           ((simple-array (simple-array uint32 1) 1) allocated-mem))
  (macrolet ((mem (index) `(aref allocated-mem ,index))
             (reg (index) `(aref regs ,index)))
    (loop 
       (block continue 
         (let* ((instr (aref (aref allocated-mem 0) pc))
                (a (logand (ash instr -6) 7))
                (b (logand (ash instr -3) 7))
                (c (logand instr 7))
                (opcode (ash instr -28)))
           (declare (uint32 instr))
           (case opcode
             ;; conditional move
             (0 (when (/= 0 (reg c))
                  (setf (reg a) (reg b))))
             ;; array index
             (1 (setf (reg a)
                      (aref (mem (reg b)) (reg c))))
             ;; array amendment
             (2 (setf (aref (mem (reg a)) (reg b))
                      (reg c)))
             ;; add
             (3 (setf (reg a)
                      (wrap-around (+ (reg b) (reg c)))))
             ;; mul
             (4 (setf (reg a)
                      (wrap-around (* (reg b) (reg c)))))
             ;; div
             (5 (setf (reg a)
                      (floor (reg b) (reg c))))
             ;; not-and
             (6 (setf (reg a)
                      (wrap-around (lognot (logand (reg b) (reg c))))))
             ;; halt
             (7 (return))
             ;; allocation
             (8 (let ((new-array (make-array (reg c) :element-type '(unsigned-byte 32))))
                  (if (not (null free-indices))
                      (let ((index (car free-indices)))
                        (setf (mem index) new-array)
                        (setf (reg b) index)
                        (setf free-indices (cdr free-indices)))
                      (let ((index (length allocated-mem)))
                        (setf allocated-mem (array-extend allocated-mem))
                        (setf (aref allocated-mem index) new-array)
                        (setf (reg b) index)
                        (setf free-indices (loop for i from (1+ index) to (1- (length allocated-mem))
                                              collect i))))))
             ;; abandon (deallocation)
             (9 (setf free-indices (cons (reg c) free-indices)))
             ;; output
             (10 (princ (code-char (reg c))))
             ;; input
             (11 (setf (reg c) (char-code (read-char))))
             ;; load program
             (12 (progn (when (/= 0 (reg b))
                          (setf (mem 0)
                                (copy-seq (mem (reg b)))))
                        (setf pc (reg c))
                        (return-from continue)))
             ;; orthography
             (13 (let ((a (logand (ash instr -25) 7)))
                   (setf (reg a)
                         (logand instr #x1FFFFFF))))
             (otherwise (error "Unknown opcode encountered.")))
           (incf pc))))))

(defun array-extend (array)
  "Create a copy of ARRAY that is twice as long."
  (let ((new-array (make-array (* 2 (length array)))))
    (loop for x across array and i upfrom 0 
       do (setf (aref new-array i) x))
    (format t "DEBUG: length of array: ~a~%" (length new-array))
    new-array))

(defun start (filename)
  (let ((regs (make-array 8 :element-type 'uint32))
        (allocated-mem (make-array 1000))
        (free-indices (loop for i from 1 to 999 collect i)))
    (setf (elt allocated-mem 0) (load-program filename))
    (main-loop allocated-mem regs free-indices 0)))
