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

(defun hour-to-degrees (hour)
  "HOUR to degrees.
0,360 is at 00:00"
  (* 30 (% hour 12)))

(defun minute-to-degrees (minute)
  "MINUTE to degrees.
0,360 is at 00:00"
  (* 30 (% (/ minute 5) 12)))

(defun hands-at-time-svg (hour hour-radius minute minute-radius svg-template)
  "Draw hands at HOUR & MINUTE with SVG-TEMPLATE.
Using HOUR-RADIUS & MINUTE-RADIUS."
  (let* ((hour-point (point-on-circle-at-angle-radius
                      hour-radius (hour-to-degrees hour)))
         (minute-point (point-on-circle-at-angle-radius
                        minute-radius (minute-to-degrees minute))))
    ;; Template requires: x-hour y-hour x-minute y-minute
    (format svg-template
            hour
            minute
            (car hour-point)
            (cdr hour-point)
            (car minute-point)
            (cdr minute-point))))

(let* ((template (f-read (read-file-name "SVG template: ")))
       (hour-radius (read-number "Hour hand radius: "))
       (minute-radius (read-number "Minute hand radius: "))
       (hours (number-sequence 0 11))
       (minutes (number-sequence 0 55 5))
       (out (s-join "\n"
               (-flatten
                (mapcar
                  (lambda (hour)
                    (mapcar
                       (lambda (minute)
                         (hands-at-time-svg hour hour-radius
                                            minute minute-radius
                                            template))
                     minutes))
                 hours)))))
  (f-write-text out 'utf-8
                (read-file-name
                 "Output to: " default-directory "test")))

;;; generate-clock-hands.svg.el ends here
