(defpackage :avr-learning
  (:use :cl))
(in-package :avr-learning) 
 
;; Lots of adress definitions are found in m2560def.inc
;; avs assembly names are written in CAPITAL LETTERS.
;; Lispy lisp code is written according to lispy conventions like *earmuffs* and so on.

;; Types defined here for assuring "avr CPU" is only doing things that an avr CPU can actually do.
;; Nice about doing it with type definitions instead of macros/if-statements is:
;;   * That SBCL can potentially generate really fast code.
;;       Use profiler (sb-prof).
;;       Turn off type checks locally within a function with (declare (optimize (safety 0))). Very unlispy.
;;       (declare (optimize (speed 3))) can sometimes give faster code.
;;       Use (disassemble) to check resulting asm if aggressivly optimizing speed and safety...
;;   * SBCL does type inference, so (describe 'some-function-with-declared-types) or
;;       (describe 'some-function-using-return-values-of-function-which-has-declared-types)
;;       directly gives detailed info about output (for example max and min values)
;;   * Saves a lot of explicit typing. Typeing.
;; Not-so-nice things about type definitions:
;;   * Crashes hard. Expecially if speed and safety are also declared optimized.
;;   * Sometimes-static typing makes us want to write two functions: One without type checking, and one with it.
;;       This can lead to even trickier than tricky debugging...
(deftype register-nr () '(integer 0 31)) ; [0, 31] Including 31
(deftype high-register-nr () '(integer 16 31))
(deftype register-low-byte-nr () '(or (integer 26 26) (integer 28 28) (integer 30 30)))
(deftype octet () '(integer #b00000000 #b11111111))

; The atmega 2560 has 4 KiB of EEPROM
(defconstant *eeprom-size* (* 1024 4))
(defconstant EEPROMEND (- *eeprom-size* 1))
; Let initial contents be random
(defparameter *eeprom* (make-array *eeprom-size*
                                   :element-type 'octet
                                   :initial-contents (loop repeat *eeprom-size* collect (random 256))))

; Have called the ram/sram data-memory and say that sram is a part of data-memory placed after all registers and stuff.
; So to find sram you need the data memory and start/stop adress.
; The atmega 2560 has 32 general purpose registers, 86 i/o ports and 8 KiB of (s)RAM
(defconstant *data-memory-size* (+ 32 86 (* 1024 8)))
(defconstant RAMEND (- *data-memory-size* 1))
; The registers r0, r1, ..., r31 occupy the first 32 addresses (0x0000 to 0x001F) of ram?
(defparameter *data-memory* (make-array *data-memory-size*
                                        :element-type 'octet
                                        :initial-element 0))

; Let initial contents of flash be random.
; There will be a bootloader and stuff residing there.
; Allows only 10 000 write-cyckles before dead.
; The atmega 2560 has 256 kiB of flash
; 1 kiB is used for bootloader.
(defconstant *flash-size* (* 1024 256))
(defconstant FLASHEND (- *flash-size* 1))
(defparameter *flash* (make-array *flash-size*
                                  :element-type 'octet
                                  :initial-element (loop repeat *flash-size* collect (random 256))))

; Most functions here work only on registers because of type declaration
; debug-versions without the strict types could be useful for reading
; *eeprom*, *data-memory* and *flash* ...
(defun print-register (r &optional (out t))
  (declare (type register-nr r))
  (format out "value: ~8,'0b" (val r)))

; Returns the value of a register r
(defun val (r)
  (declare (type register-nr r))
  (elt *data-memory* r))

; Makes it possible to overwrite register r like
; (setf (val r) #b10101010)
(defsetf val (r) (new-val)
  `(setf (elt *flash* ,r) ,new-val))

; Returns value of a 16-bit register, treating r as the low byte
; and r+1 as the high byte
(defun val-16 (r)
  (declare (type register-nr r))
  (+ (elt *flash* r) (ash (elt *flash* (+ r 1)) 8)))

; Makes it possible to overwrite registers r and r+1 (with r being the low byte) like
; (setf (val-16 r) #b0000111101010101)
(defsetf val-16 (r) (new-val)
  `(setf (elt *flash* ,r)            (logand ,new-val #b0000000011111111)
         (elt *flash* (+ ,r 1)) (ash (logand ,new-val #b1111111100000000) -8)))

;; Defining the registers...
;; Just mentioning conventions in comments
(defconstant r0   0)
(defconstant r1   1) ; If you're doing math, better place numbers in r1, ..., r15
(defconstant r2   2)
(defconstant r3   3)
(defconstant r4   4)
(defconstant r5   5)
(defconstant r6   6)
(defconstant r7   7)
(defconstant r8   8)
(defconstant r9   9)
(defconstant r10 10)
(defconstant r11 11)
(defconstant r12 12)
(defconstant r13 13)
(defconstant r14 14)
(defconstant r15 15)
(defconstant r16 16) ; If you're storing flags, better use r16, ..., r23
(defconstant r17 17)
(defconstant r18 18)
(defconstant r19 19)
(defconstant r20 20)
(defconstant r21 21)
(defconstant r22 22)
(defconstant r23 23)
(defconstant r24 24) ; 16-bit counters often located in r24:r25
(defconstant r25 25)
(defconstant r26 26) ; If you need pointer access (data-memory or flash) place pointers in r26, ..., r31
(defconstant r27 27)
(defconstant r28 28)
(defconstant r29 29)
(defconstant r30 30)
(defconstant r31 31)
; Pointer convention is so strong that assembler (m2560def.inc) defines names X, Y and Z like this
(defparameter xl r26) 
(defparameter xh r27)
(defparameter yl r28)
(defparameter yh r29)
(defparameter zl r30)
(defparameter zh r31)
(defparameter x  xl)
(defparameter y  yl)
(defparameter z  zl)

(defparameter carry-flag 0)

;; This first increases x, then return x's previous value.
;; Don't know if this is the order in which avr assembler actually does things
(define-symbol-macro x+ 
  (progn (setf (val-16 x) (+ (val-16 x) 1)) 
         (- (val-16 x) 1)))

; setf returns the new value
(define-symbol-macro -x
  (setf (val-16 x) (- (val-16 x) 1)))

(define-symbol-macro y+ 
  (progn (setf (val-16 y) (+ (val-16 y) 1)) 
         (- (val-16 y) 1)))

(define-symbol-macro -y
  (setf (val-16 y) (- (val-16 y) 1)))

(define-symbol-macro z+ 
  (progn (setf (val-16 z) (+ (val-16 z) 1)) 
         (- (val-16 z) 1)))

(define-symbol-macro -z
  (setf (val-16 z) (- (val-16 z) 1)))

(defun ldi (r x)
  (declare (type octet x)
           (type high-register-nr r))
  (setf (val r) x))

; TODO: guessed a bit on subi's behaviour here
(defun subi (r K)
  (declare (type octet K)
           (type high-register-nr r))
  (setf (val r) (max 0 (- (val r) K))))

(defun clr (r)
  (declare (type register-nr r))
  (setf (val r) #b00000000))

(defun ser (r)
  (declare (type high-register-nr r))
  (setf (val r) #b11111111))

(defun sbr (r M)
  (declare (type octet M)
           (type high-register-nr r))
  (setf (val r) (logior (val r) M)))

(defun sbci (r K)
  (declare (type octet K)
           (type high-register-nr r))
  (setf (val r) (- (val r) K carry-flag)))

(defun cpi (r K)
  (declare (type octet K)
           (type high-register-nr r))
  (= (val r) K))

(defun cbr (r M)
  (declare (type octet M)
           (type high-register-nr r))
  (setf (val r) (logand (val r) (logxor #b11111111 M))))

(defun andi (r K)
  (declare (type octet K)
           (type high-register-nr r))
  (setf (val r) (logand (val r) K)))

(defun mov (dst src)
  (declare (type register-nr dst src))
  (setf (val src) (val dst)))

;; The val-16 gotten from loading [xl xh] or [yl yh] or [zl zh]
;; is an adress in data-memory where a byte is stored
;; put that byte into register r
(defun ld (dst xyz)
  (declare (type register-low-byte-nr xyz)
           (type register-nr dst))
  (setf (val dst) (elt *data-memory* (val-16 xyz))))

;; Store the byte in src in data-memory at the adress found in the
;; (val-16 of) the x, y or z register
(defun st (xyz src)
  (declare (type register-low-byte-nr xyz)
           (type register-nr src))
  (setf (elt *data-memory* (val-16 xyz)) (val src)))

(defun bin (n)
  (declare (type (integer #b00000000 #b11111111) n))
  (format nil "~8,'0b" n))
