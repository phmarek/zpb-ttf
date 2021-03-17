(in-package #:zpb-ttf)

(defun put-table-info (tbl size pos)
  (write-uint32 (tag tbl))
  (write-uint32 0) ; ((checksum font))
  (write-uint32 pos)
  (write-uint32 size))

(defun write-raw-table-bytes (font tag store-fn pos)
  (let ((tbl (table-info tag font)))
    (when tbl
      (put-table-info tbl
                      (length (raw-bytes tbl))
                      pos)
      (funcall store-fn (raw-bytes tbl)))))


(defun write-post-table (font chars store-fn pos)
  (let ((tbl (table-info "post" font)))
    (funcall store-fn (make-array 100 :initial-element 4))
    (put-table-info tbl
                    5
                    pos)))

(defun write-loca-table (font chars store-fn pos)
  (let ((tbl (table-info "loca" font)))
    (funcall store-fn (make-array 100 :initial-element 4))
    (put-table-info tbl
                    10
                    pos)))

(defun write-cmap-table (font chars store-fn pos)
  (let ((tbl (table-info "cmap" font))
        (header (make-array 20 :fill-pointer 0
                            :element-type '(unsigned-byte 8)))
        (cmap (make-array 200 :fill-pointer 1
                            :element-type '(unsigned-byte 8))))
    (funcall store-fn header)
    (let ((*ttf-write-output* header))
      (write-uint16 0) ; version
      (write-uint16 1) ; subtable-count
      ;;
      (write-uint16 +microsoft-platform-id+)
      (write-uint16 +microsoft-unicode-bmp-encoding-id+)
      ;; offset to _after_ that offset number
      (write-uint32 (+ 4 (fill-pointer header))))
    (funcall store-fn cmap)
    (write-unicode-cmap font chars cmap)
    (put-table-info tbl
                    (+ (length header)
                       (length cmap))
                    pos)))

(defun write-glyf-table (font chars store-fn pos)
  (let ((tbl (table-info "glyf" font)))
    (funcall store-fn (make-array 100 :initial-element 4))
    (put-table-info tbl
                    20
                    pos)))

(defun font-writer (font output &key characters write-name?)
  "Writes FONT to OUTPUT, optionally only writing the definitions for CHARACTERS.
  TODO"
  (let ((*ttf-write-output* output))
    (write-uint32 #x00010000) ; header
    (write-uint16 (table-count font))
    (write-uint16 (search-range font))
    (write-uint16 (entry-selector font))
    (write-uint16 (range-shift font))
    (let ((glyph-count (count 1 characters))
          (table-data ())
          (next-table-pos (+ 12 (* 16 ; TODO if not all
                                   (hash-table-count (tables font))))))
      (format *trace-output* "~d characters to write~%" glyph-count)
      (flet ((remember (bytes)
               (push bytes table-data)
               (incf next-table-pos (length bytes)))
             (location ()
               next-table-pos))
        ;; Only write table information and collect bytes
        (write-raw-table-bytes font "maxp" #'remember next-table-pos)
        (write-raw-table-bytes font "head" #'remember next-table-pos)
        (when write-name?
          (write-raw-table-bytes font "name" #'remember next-table-pos))
        (write-raw-table-bytes font "fpgm" #'remember next-table-pos)
        (write-raw-table-bytes font "cvt " #'remember next-table-pos)
        (write-raw-table-bytes font "hhea" #'remember next-table-pos)
        (write-raw-table-bytes font "vhea" #'remember next-table-pos)
        (write-raw-table-bytes font "prep" #'remember next-table-pos)
        (write-raw-table-bytes font "hmtx" #'remember next-table-pos) ;; could be shortened?
        (write-raw-table-bytes font "vmtx" #'remember next-table-pos) ;; ??
        (write-post-table font characters  #'remember next-table-pos)
        (write-loca-table font characters  #'remember next-table-pos)
        (write-cmap-table font characters  #'remember next-table-pos)
        (write-glyf-table font characters  #'remember next-table-pos)
        ;(write-kern-table font characters  #'remember next-table-pos) ; not needed?
        ;; Write actual bytes
        (dolist (seq table-data)
          (write-sequence/ttf seq *ttf-write-output*))))
    output))
