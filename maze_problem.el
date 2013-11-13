;; West London hack night problem - create a maze generator and display a maze

;; This was written in a hurry so is longer and less efficient than it could be.


(defun* draw-maze (&optional (side 0) (size 30))
  (let* (maze (startx (case side
                        (0 (random size))
                        (1 (1- size))
                        (2 (random size))
                        (3 0)))
              (starty (case side
                        (0 0)
                        (1 (random size))
                        (2 (1- size))
                        (3 (random size)))))
    (dotimes (i size) (setq maze (vconcat (make-vector size 88) maze)))
    (setq maze (remove-char maze startx starty))
    (draw-path maze startx starty size)
    (dotimes (y size)
      (dotimes (x size)
        (insert (char-to-string (elt maze (+ x (* y size))))))
      (insert "\n"))))

(defun remove-char (maze x y)
  (setf (elt maze (+ x (* y size))) 32)
  maze)

(defun draw-path (maze x y size)
  (let ((neighbours (get-neighbours maze x y size)))
    (dolist (n neighbours)
      (if (pos-valid-p maze (first n) (second n) size)
          (progn
            (setq maze (remove-char maze (first n) (second n)))
            (case (third n)
              (0 (setq maze (remove-char maze x (1- y))
                       maze (draw-path maze (first n) (second n) size)))
              (1 (setq maze (remove-char maze (1- x) y)
                       maze (draw-path maze (first n) (second n) size)))
              (2 (setq maze (remove-char maze x (1+ y))
                       maze (draw-path maze (first n) (second n) size)))
              (3 (setq maze (remove-char maze (1+ x) y)
                       maze (draw-path maze (first n) (second n) size))))))))
  maze)

(defun pos-valid-p (maze x y size)
  "Check if position at X,Y is visited."
  (unless (or (< x 1)
              (>= x (1- size))
              (< y 1)
              (>= y (1- size)))
    (if (= (elt maze (+ x (* y size))) 88) t)))

(defun get-neighbours (maze x y size)
  "Return list of unvisited neighbours to position X,Y in random order."
  (let* (perm (r (random 4)) newx newy retval)
    (dotimes (i 3)
      (while (member r perm)
        (setq r (random 4)))
      (setq perm (cons r perm)))
    (dolist (p perm)
      (case p
        (0 (setq newx x newy (- y 2)))
        (1 (setq newx (- x 2) newy y))
        (2 (setq newx x newy (+ y 2)))
        (3 (setq newx (+ x 2) newy y)))
      (if (pos-valid-p maze newx newy size)
          (setq retval (cons (list newx newy p) retval))))
    retval))


