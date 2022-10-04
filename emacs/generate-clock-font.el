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

:glyph-directory
Glyph output directory"
 (plist-bind (clock-face-template-filename
              hands-template-filename
              hour-radius
              minute-radius
              glyph-directory)
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
          (glyph-directory (or glyph-directory (read-directory-name "Glyph output directory: "))))
      (unless (file-directory-p glyph-directory)
        (make-directory glyph-directory t))
      (--each-indexed time-paths
        (let ((output-path-filename (format
                                     "%s/%s.svg"
                                     glyph-directory
                                     (index-to-hour-minute it-index))))
         (message "Writing: %s" output-path-filename)
         (f-write-text (format clock-template it) 'utf-8 output-path-filename))))))

(defun convert-glyphs-for-ttf (folder &optional solid frame-id)
  "Convert glyphs in FOLDER for truetype.

When SOLID is nil, we combine the paths created by inkscape stroke-to-path, into a single path.

SOLID non-nil, create clock using hands path subtracted from face path.
FRAME-ID must be supplied when using SOLID."
  (--each (f--entries folder (string-match-p ".*svg$" it))
    (if solid
        (progn
         (unless frame-id (user-error "No frame-id for SVG path differene"))
         (strokes-to-path-difference it frame-id))
     (strokes-to-combined-path it))))

(defun strokes-to-combined-path (svg-file)
  "Use inkscape to convert strokes in SVG-FILE to a single combined path."
  (let ((inkscape-actions
         "--actions=\"select-all:groups;object-stroke-to-path;path-combine;export-do\"")
        (inkscape-command "/Applications/Inkscape.app/Contents/MacOS/inkscape"))
     (shell-command (s-join " " `(,inkscape-command ,inkscape-actions ,svg-file)))))

(defun strokes-to-path-difference (svg-file)
  "Use inkscape to convert strokes in SVG-FILE to a single combined path."
  (let ((inkscape-actions "--actions=\"select-all:groups;
                        object-stroke-to-path;
                        select-clear;
                        select-by-id:frame;
                        selection-ungroup;
                        path-union;
                        selection-bottom;
                        select-all:all;
                        selection-ungroup;
                        selection-ungroup;
                        path-difference;
                        export-do\"")
        (inkscape-command "/Applications/Inkscape.app/Contents/MacOS/inkscape"))
     (shell-command (s-join " " `(,inkscape-command ,inkscape-actions ,svg-file)))))

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
               glyph-svg-directory
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

(defun cleanup-glyph-folder (folder)
  "Clean up after successful inkscape run,
Remove original construction glyphs from FOLDER."
  (--each
      (f--entries folder (string-match-p "clock_[01][0-9]_[0-5][05][.]svg" it))
    (f-delete it))
  (--each
      (f--entries folder (string-match-p ".*_out[.]svg" it))
    (f-move it (replace-regexp-in-string "_out[.]svg" ".svg" it))))

(defun generate-font-options (variant)
  "Use VARIANT name to generate options"
  (list :font-name-suffix variant
        :glyph-svg-directory (format "../ClockFace%s-glyphs/" variant)
        :font-template-filename "./font.template"
        :font-glyph-template-filename "./font-glyph.template"
        :output-filename (format "../ClockFace%s-Regular.ttf" variant)))

(when nil
 (generate-clock-faces
  '(:clock-face-template-filename "./clockface-solid.template"
    :hands-template-filename "./hands.template"
    :hour-radius 140
    :minute-radius 210
    :glyph-directory "../ClockFaceFatHandsSolid-glyphs/"))

 (generate-clock-faces
  '(:clock-face-template-filename "./clockface-square.template"
    :hands-template-filename "./hands-square.template"
    :hour-radius 150
    :minute-radius 230
    :glyph-directory "../ClockFaceSquare-glyphs/"))

 (progn
  (generate-clock-font (generate-font-options "FatHands"))
  (generate-clock-font (generate-font-options "FatSquare"))
  (generate-clock-font (generate-font-options "Square")))

 (progn
  (convert-glyphs-for-ttf "../ClockFaceFatHands-glyphs/")
  (convert-glyphs-for-ttf "../ClockFaceSquare-glyphs/")
  (convert-glyphs-for-ttf "../ClockFaceFatSquare-glyphs/"))

 (progn
  (cleanup-glyph-folder "../ClockFaceFatHands-glyphs/")
  (cleanup-glyph-folder "../ClockFaceSquare-glyphs/")
  (cleanup-glyph-folder "../ClockFaceFatSquare-glyphs/"))

 (generate-clock-faces
  '(:clock-face-template-filename "./clockface-fat-square.template"
    :hands-template-filename "./hands-fat.template"
    :hour-radius 150
    :minute-radius 230
    :glyph-directory "../ClockFaceFatSquare-glyphs/")))

;;; generate-clock-font.el ends here
