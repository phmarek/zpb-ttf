;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; Utility functions, mostly for reading data out of the input-stream
;;; of a font-loader.
;;;
;;; $Id: util.lisp,v 1.9 2006/02/18 23:13:43 xach Exp $

(in-package #:zpb-ttf)

;;; Reading compound MSB values from an '(unsigned-byte 8) stream

(defun read-uint32 (stream)
  (loop repeat 4
        for value = (read-byte stream)
        then (logior (ash value 8) (read-byte stream))
        finally (return value)))

(defun read-uint16 (stream)
  (loop repeat 2
        for value = (read-byte stream)
          then (logior (ash value 8) (read-byte stream))
        finally (return value)))


(defun read-uint8 (stream)
  (read-byte stream))

(defun read-int8 (stream)
  (let ((result (read-byte stream)))
    (if (logbitp 7 result)
        (1- (- (logandc2 #xFF result)))
        result)))

(defun read-int16 (stream)
  (let ((result (read-uint16 stream)))
    (if (logbitp 15 result)
        (1- (- (logandc2 #xFFFF result)))
        result)))

(defun read-fixed (stream)
  (read-uint32 stream))

(defun read-fword (stream)
  (read-int16 stream))

(defun read-fixed2.14 (stream)
  (let ((value (read-uint16 stream)))
    (let ((integer (ash value -14))
          (fraction (logand #x3FFF value)))
      (when (logbitp 1 integer)
        (setf integer (1- (- (logandc2 #b11 integer)))))
      (+ integer (float (/ fraction #x3FFF))))))


;;; Writing compound MSB values from an '(unsigned-byte 8) stream

(defvar *ttf-write-output* nil)

(defun write-uint8 (value &optional (stream *ttf-write-output*))
  ;; Accepts either a stream or an array with a fill-pointer.
  (check-type value (integer 0 255))
  (cond
    ((streamp stream)
     (write-byte value stream))
    ((arrayp stream)
     (vector-push value stream))
    (t
     (error "unsupported thing to write into: ~s" stream))))

(defun write-uint32 (value &optional (stream *ttf-write-output*))
  (check-type value (integer 0 #xffffffff))
  (write-uint8 (ldb (byte 8 24) value) stream)
  (write-uint8 (ldb (byte 8 16) value) stream)
  (write-uint8 (ldb (byte 8  8) value) stream)
  (write-uint8 (ldb (byte 8  0) value) stream)
  value)

(defun write-uint16 (value &optional (stream *ttf-write-output*))
  (check-type value (integer 0 65535))
  (write-uint8 (ldb (byte 8  8) value) stream)
  (write-uint8 (ldb (byte 8  0) value) stream)
  value)

(defun write-int8 (value &optional (stream *ttf-write-output*))
  (check-type value (integer -128 127))
  (if (minusp value)
      (write-uint8 (+ #x100 value) stream)
      (write-uint8 value stream))
  value)

(defun write-int16 (value &optional (stream *ttf-write-output*))
  (if (minusp value)
      (write-uint16 (+ #x10000 value) stream)
      (write-uint16            value  stream))
  value)

(defun write-fixed (value &optional (stream *ttf-write-output*))
  (write-uint32 stream))

(defun write-fword (value &optional (stream *ttf-write-output*))
  (write-int16 stream))

(defun write-fixed2.14 (value &optional (stream *ttf-write-output*))
  (check-type value (float 0.0 4.0))
  (write-uint16 (round value (/ 1 #x4000)))
  value)

(defun write-pstring (string &optional (stream *ttf-write-output*))
  "Write a Pascal-style length-prefixed string."
  (let* ((l (length string)))
    (write-uint8 l stream)
    (dotimes (i l)
      (write-uint8 (char-code (aref string i)) stream))
    string))

(defun write-sequence/ttf (seq &optional (stream *ttf-write-output*))
  (loop for e across seq
        do (write-uint8 e stream))
  seq)


(defun advance-file-position (stream n)
  "Move the file position of STREAM ahead by N bytes."
  (let ((pos (file-position stream)))
    (file-position stream (+ pos n))))

(defun bounded-aref (vector index)
  "Some TrueType data vectors are truncated, and any references beyond
the end of the vector should be treated as a reference to the last
element in the vector."
  (aref vector (min (1- (length vector)) index)))

(defun (setf bounded-aref) (new-value vector index)
  (setf (aref vector (min (1- (length vector)) index)) new-value))
