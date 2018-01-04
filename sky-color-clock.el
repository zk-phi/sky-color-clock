(require 'cl-lib)
(require 'color)

(defvar sky-color-clock-format "%d %H:%M")
(defvar sky-color-clock-enable-moonphase-emoji t)

;; ---- sky color

;; TODO: weather and temperature
;; - lower saturation and less contrast for cloudy days ?
;; - gradiant with temperature (like Solar weather app)
;; - add purple "magic hour" color

;; simpler than solar.el, lunar.el

(defvar sky-color-clock--daytime-length nil)
(defvar sky-color-clock--sunrise nil)
(defvar sky-color-clock--sunset nil)

;;;###autoload
(defun sky-color-clock-initialize (latitude)
  "Initialize sky-color-clock with LATITUDE (in degrees)."
  (let* ((day-of-year                   ; day of year (0-origin)
          (1- (time-to-day-in-year (current-time))))
         (sun-declination               ; declination of the sun
          (degrees-to-radians
           (* -23.44 (cos (degrees-to-radians (* (/ 360 365.0) (+ day-of-year 10)))))))
         (sunset-hour-angle             ; the "Sunrise equation"
          (* (acos (- (* (tan (degrees-to-radians latitude)) (tan sun-declination))))))
         (sunset-time-from-noon         ; rad -> hours
          (* 24 (/ (radians-to-degrees sunset-hour-angle) 360))))
    (setq sky-color-clock--daytime-length (* sunset-time-from-noon 2)
          sky-color-clock--sunrise        (- 12 sunset-time-from-noon)
          sky-color-clock--sunset         (+ 12 sunset-time-from-noon))))

(defun sky-color-clock--blend-colors (basecolor mixcolor &optional fraction)
  "Blend to colors. FRACTION must be between 0.0 and 1.0,
otherwise result may be broken."
  (cl-destructuring-bind (r g b) (color-name-to-rgb basecolor)
    (cl-destructuring-bind (rr gg bb) (color-name-to-rgb mixcolor)
      (let* ((x (or fraction 0.5)) (y (- 1 x)))
        (color-rgb-to-hex (+ (* r y) (* rr x)) (+ (* g y) (* gg x)) (+ (* b y) (* bb x)))))))

;; TODO: Refactor me (and make me configurable)
(defun sky-color-clock--pick-bg-color (time)
  "Corner cases are not supported for now: daytime-length must be
larger than 5 hrs, sunrise time must be smaller than sunset
time (unlike sunrise 23:00 sunset 19:00), sun must rise and
set (no black/white nights) in a day."
  (cl-destructuring-bind (sec min hour . _) (decode-time time)
    (let ((time (+ (/ (+ (/ sec 60.0) min) 60.0) hour))
          (rise sky-color-clock--sunrise)
          (set sky-color-clock--sunset)
          (daytime sky-color-clock--daytime-length))
      (cond
       ;; sunrise colors -> #111111 #4d548a #c486b1 #ee88a0 #ff7d75 #f4eeef
       ((< time (- rise 5)) "#111111")
       ((< time (- rise 4)) (sky-color-clock--blend-colors "#4d548a" "#111111" (- rise 4 time)))
       ((< time (- rise 3)) (sky-color-clock--blend-colors "#c486b1" "#4d548a" (- rise 3 time)))
       ((< time (- rise 2)) (sky-color-clock--blend-colors "#ee88a0" "#c486b1" (- rise 2 time)))
       ((< time (- rise 1)) (sky-color-clock--blend-colors "#ff7d75" "#ee88a0" (- rise 1 time)))
       ((< time rise)       (sky-color-clock--blend-colors "#f4eeef" "#ff7d75" (- rise time)))
       ;; daytime colors -> gradient from #f4eeef to #5dc9f1
       ((< time (- set 5))  (sky-color-clock--blend-colors "#5dc9f1" "#f4eeef" (/ (- set 5 time) daytime)))
       ;; sunset colors -> #5dc9f1 #003888 #f1b17c #bb4504 #200858 #111111
       ((< time (- set 4))  (sky-color-clock--blend-colors "#003888" "#5dc9f1" (- set 4 time)))
       ((< time (- set 3))  (sky-color-clock--blend-colors "#f1e17c" "#003888" (- set 3 time)))
       ((< time (- set 2))  (sky-color-clock--blend-colors "#bb4504" "#f1e17c" (- set 2 time)))
       ((< time (- set 1))  (sky-color-clock--blend-colors "#200858" "#bb4504" (- set 1 time)))
       ((< time set)        (sky-color-clock--blend-colors "#111111" "#200858" (- set time)))
       (t                   "#111111")))))

(defun sky-color-clock--pick-fg-color (color)
  (cl-destructuring-bind (r g b) (color-name-to-rgb color)
    (if (> (+ r g b) 1.5) "#111111" "#eeeeee")))

;; ---- emoji moonphase

(defconst sky-color-clock--newmoon 6.8576
  "A new moon (1970/01/08 05:35) in days since the epoch.")

(defconst sky-color-clock--cycle 29.5306
  "Eclipse (synodic month) cycle in days.")

(defun sky-color--emoji-moonphase (time)
  (let* ((time-in-days (/ (float-time time) 60 60 24))
         (phase (mod (- time-in-days sky-color-clock--newmoon) sky-color-clock--cycle)))
    (cond ((<= phase  1.84) "🌕")
          ((<= phase  5.53) "🌖")
          ((<= phase  9.22) "🌗")
          ((<= phase 12.91) "🌘")
          ((<= phase 16.61) "🌑")
          ((<= phase 20.30) "🌒")
          ((<= phase 23.99) "🌓")
          ((<= phase 27.68) "🌔")
          (t                "🌕"))))

;; ---- the clock

(defun sky-color-clock (&optional time)
  "Generate a fontified time string according to
`sky-color-clock-format' and
`sky-color-clock-enable-moonphase-emoji'."
  (let* ((time (or time (current-time)))
         (bg (sky-color-clock--pick-bg-color time))
         (fg (sky-color-clock--pick-fg-color bg))
         (str (concat " " (format-time-string sky-color-clock-format time) " ")))
    (when sky-color-clock-enable-moonphase-emoji
      (setq str (concat " " (sky-color--emoji-moonphase time) str)))
    (propertize str 'face `(:background ,bg :foreground ,fg))))

(provide 'sky-color-clock)
