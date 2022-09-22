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

(defun index-to-hour-minute (index)
  "INDEX 0..144 as hour minute."
  (let ((index (% index 144))
        (hour  (/ index 12))
        (minute (* 5 (% index 12))))
    (format "clock_%02i_%02i" hour minute)))

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

(defun generate-clock-faces (&optional options)
  "Generate set of clock face svg images using OPTIONS.

OPTIONS plist:

:clock-template-filename - a template filename
should be a svg clock face,
with a printf string slot to insert hands

:hands-template-filename - a template filename
should ba a svg path template,
with printf endpoints for minute hand and hour hand.

:hour-radius
Integer for hour hand length

:minute-radius
Integer for hour minute length

:name-prefix
String filename/path prefix"
 (plist-bind (clock-template-filename
              hands-template-filename
              hour-radius
              minute-radius
              name-prefix)
             options
   (let* ((clock-template (f-read clock-template-filename))
          (hands-template (f-read hands-template-filename))
          (hours (number-sequence 0 11))
          (minutes (number-sequence 0 55 5))
          (time-paths (-flatten
                       (mapcar
                        (lambda (hour)
                          (mapcar
                           (lambda (minute)
                             (hands-at-time-svg hour hour-radius
                                                minute minute-radius
                                                hands-template))
                           minutes))
                        hours)))
          (name-prefix    (or name-prefix (read-string "Name prefix: "))))
      (--each-indexed time-paths
         (f-write-text (format clock-template it) 'utf-8
                       (format "%s%s.svg" name-prefix
                               (index-to-hour-minute it-index)))))))

;;; generate-clock-hands.svg.el ends here
