;;; Basic array data structure (2d list, rows, cols)
(define-record-type <array> (%make-array num-rows num-cols cell-data)
  array?
  (num-rows array-num-rows)
  (num-cols array-num-cols)
  (cell-data %array-data))

(define (array-ref array r c)
  (let ((data (%array-data array))
        (nrows (array-num-rows array))
        (ncols (array-num-cols array)))
    (if (not (and (<= 0 r (- nrows 1))
                  (<= 0 c (- ncols 1))))
        (error "Invalid args for array-ref"
               (list r c))
        (let ((row (list-ref data r)))
          (list-ref row c)))))

(define (make-empty-array num-rows num-cols)
  (let ((cell-data (%init-array-data num-rows num-cols)))
    (%make-array num-rows num-cols cell-data)))

(define (%init-array-data num-rows num-cols)
  (make-initialized-list
   num-cols
   (lambda (c)
     (make-initialized-list
      num-rows
      (lambda (r) (make-empty-cell))))))

;;; Grid cells
(define-record-type <cell> (%make-cell content)
  cell?
  (content cell-content set-cell-content!))

(define *empty-cell-content*
  (list '*empty-cell-content*))

(define (empty-cell? cell)
  (eq? (cell-content cell) *empty-cell-content*))

(define (make-empty-cell)
  (%make-cell *empty-cell-content*))
