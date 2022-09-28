;;; Generate-clock-font --- Create SVG clock fonts
;;; Commentary:
;;
;; Note this package is to be used on a Mac with Inkscape installed at
;; /Applications/Inkscape.app
;;
;; It is single purpose and intended for internal use only.
;; (it also depends on custom functions which are not provided here.)
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

(defun generate-clock-faces (options)
  "Generate set of clock face svg images using OPTIONS.

OPTIONS plist: (all required)

:clock-face-template-filename - template filename
Should be an svg clock face,
with a printf string slot to insert hands.

:hands-template-filename - template filename
Should be an svg path template,
with printf endpoints for minute hand and hour hand.

:hour-radius
Integer for hour hand length

:minute-radius
Integer for hour minute length

:name-prefix
String filename/path prefix"
 (plist-bind (clock-face-template-filename
              hands-template-filename
              hour-radius
              minute-radius
              name-prefix)
             options
   (let* ((clock-template (f-read clock-face-template-filename))
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
        (let ((output-filename (format
                                "%s%s.svg"
                                name-prefix
                                (index-to-hour-minute it-index))))
         (message "Writing: %s" output-filename)
         (f-write-text (format clock-template it) 'utf-8 output-filename))))))

(defun strokes-to-combined-path (svg-file output-file)
  "Use inkscape to convert strokes in SVG-FILE to a single combined path OUTPUT-FILE."
  (let ((inkscape-actions
         "--actions=\"select-all:groups;object-stroke-to-path;path-combine;export-do\"")
        (inkscape-command "/Applications/Inkscape.app/Contents/MacOS/inkscape"))))


(defun generate-clock-font (options)
 "Generate a clock font from glyphs.

OPTIONS all required

:font-name-suffix
Font name suffix, e.g.
Regular, Solid, Rect, RectSolid

:glyph-svg-directory
Directory containing SVG Glyphs

:font-template-filename
Font template file

:font-glyph-template-filename
Font glyph template file

:output-filename
Font file to create"
  (plist-bind (font-name-suffix
               sglyph-svg-directory
               font-template-filename
               font-glyph-template-filename
               output-filename)
              options
    (let* ((glyph-filenames (directory-files glyph-svg-directory t ".*svg"))
           (font-template (f-read-text font-template-filename))
           (glyph-template (f-read-text font-glyph-template-filename))
           (glyphs (s-join
                    "\n"
                    (-map-indexed
                     (lambda (index glyph-svg-filename)
                        (let ((glyph-svg (f-read-text glyph-svg-filename))
                              (glyph-name (file-name-base glyph-svg-filename))
                              (glyph-path (s-match " d=\"\\(.*?\\)\"" glyph-svg)))
                          (format glyph-template
                                  glyph-name
                                  (+ #xF0000 index)
                                  glyph-path)))
                     glyph-filenames)))
           (font (format font-template font-name-suffix glyphs)))
        (f-write-text font 'utf-8 output-filename))))

;;; generate-clock-font.el ends here
