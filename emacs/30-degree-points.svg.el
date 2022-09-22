;;; 30-degree-points.svg --- not a package.
;;; Commentary:
;;
;;; Code:

(require 'dash)
(require 's)
(require 'f)

(defun point-on-circle-at-angle-radius (radius angle)
 "Return a cons of X,Y at RADIUS, ANGLE."
 (cons (* radius (cos (degrees-to-radians angle)))
       (* radius (sin (degrees-to-radians angle)))))

(let* ((template (f-read (read-file-name "SVG Template: ")))
       (radius (read-number "Radius: "))
       (angles (number-sequence 30 360 30))
       (coords (--map (point-on-circle-at-angle-radius radius it) angles))
       (out    (s-join "\n" (-flatten
                             (--map (let* ((x  (car it))
                                           (y  (cdr it))
                                           (id (format "%i:%i" x y)))
                                      (format template id x y))
                                  coords)))))
   (f-write-text out 'utf-8 (read-file-name "Output to: ")))
