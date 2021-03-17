(in-package :zpb-ttf)


(with-font-loader (l #P"/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf"
                     :raw-bytes t)
  (let ((which (make-array 2000 
                           :element-type 'bit
                           :initial-element 0))
        (output (make-array 200000
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0)))
    (setf (aref which (char-code #\A)) 1)
    (font-writer l output
                 :characters which)
    (with-open-file (s #P"/tmp/a.ttf"
                       :element-type '(unsigned-byte 8) 
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (write-sequence output s)))
  l)
