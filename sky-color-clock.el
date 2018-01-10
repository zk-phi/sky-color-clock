(require 'cl-lib)
(require 'color)
(require 'json)

(defvar sky-color-clock-format "%d %H:%M")
(defvar sky-color-clock-enable-moonphase-emoji t)
(defvar sky-color-clock-enable-temperature-indicator t)

;; TODO:
;; - weather and temperature
;;   - lower saturation and less contrast for cloudy days ?
;;   - gradiant with temperature (like Solar weather app)
;; - solar.el, lunar.el has more accurate algorithm

;; ---- color utilities

(defun sky-color-clock--make-gradient (&rest color-stops)
  "Make a function which takes a number and returns a color
according to COLOR-STOPS, which is a sorted list of the
form ((NUMBER . COLOR) ...)."
  (unless color-stops
    (error "No color-stops are specified."))
  (let* ((first-color (pop color-stops))
         (last-color first-color))
    `(lambda (x)
       (cond ((<= x ,(car first-color)) ,(cdr first-color))
             ,@(mapcar (lambda (next-color)
                         (prog1
                             `((<= x ,(car next-color))
                               (sky-color-clock--blend-colors
                                ,(cdr last-color) ,(cdr next-color)
                                (/ (- x ,(car last-color)) ,(float (- (car next-color) (car last-color))))))
                           (setq last-color next-color)))
                       color-stops)
             (t ,(cdr last-color))))))

(defun sky-color-clock--blend-colors (basecolor mixcolor &optional fraction)
  "Blend to colors. FRACTION must be between 0.0 and 1.0,
otherwise result may be broken."
  (cl-destructuring-bind (r g b) (color-name-to-rgb basecolor)
    (cl-destructuring-bind (rr gg bb) (color-name-to-rgb mixcolor)
      (let* ((x (or fraction 0.5)) (y (- 1 x)))
        (color-rgb-to-hex (+ (* r y) (* rr x)) (+ (* g y) (* gg x)) (+ (* b y) (* bb x)))))))

;; ---- openwethermap api

(defvar sky-color-clock--openweathermap-api-key nil)
(defvar sky-color-clock--openweathermap-city-id nil)
(defvar sky-color-clock--openweathermap-cache   nil)

(defun sky-color-clock--update-weather ()
  "Fetch current weather via openwethermap API and update
`sky-color-clock--openweathermap-cache'."
  (url-retrieve
   (format "http://api.openweathermap.org/data/2.5/weather?id=%s&appid=%s"
           sky-color-clock--openweathermap-city-id
           sky-color-clock--openweathermap-api-key)
   (lambda (_)
     (search-forward "\n\n" nil t)
     (let ((json-object-type 'hash-table)
           (json-key-type 'symbol)
           (json-array-type 'list))
       (setq sky-color-clock--openweathermap-cache
             (json-read-from-string (buffer-substring (point) (point-max))))))))

(defun sky-color-clock--cloudiness ()
  "Get current cloudiness in percent from
`sky-color-clock--openweathermap-cache', or nil."
  (when sky-color-clock--openweathermap-cache
    (gethash 'all (gethash 'clouds sky-color-clock--openweathermap-cache))))

(defun sky-color-clock--temperature ()
  "Returns current temperature in kelvin from
`sky-color-clock--openweathermap-cache', or nil."
  (when sky-color-clock--openweathermap-cache
    (gethash 'temp_max (gethash 'main sky-color-clock--openweathermap-cache))))

(defun sky-color-clock-start-openwethermap-client (api-key city-id &optional interval)
  "Initialize openwethermap client with API-KEY to fetch weather
of city specified with CITY-ID every INTERVAL minutes. INTERVAL
defaults 30."
  (setq sky-color-clock--openweathermap-api-key api-key
        sky-color-clock--openweathermap-city-id city-id)
  (run-with-timer 0 (* (or interval 30) 60) 'sky-color-clock--update-weather))

;; ---- sky color

(defvar sky-color-clock--bg-color-gradient nil
  "A function which converts a float time (12:30 as 12.5, for
example), to a color.")

;;;###autoload
(defun sky-color-clock-initialize (latitude)
  "Initialize sky-color-clock with LATITUDE (in degrees). Special
cases are not considered for now: sun must rise after 02:00,
daytime length must be longer than 2hrs, and sun must set before
23:30."
  (let* ((day-of-year                   ; day of year (0-origin)
          (1- (time-to-day-in-year (current-time))))
         (sun-declination               ; declination of the sun
          (degrees-to-radians
           (* -23.44 (cos (degrees-to-radians (* (/ 360 365.0) (+ day-of-year 10)))))))
         (sunset-hour-angle             ; the "Sunrise equation"
          (* (acos (- (* (tan (degrees-to-radians latitude)) (tan sun-declination))))))
         (sunset-time-from-noon         ; rad -> hours
          (* 24 (/ (radians-to-degrees sunset-hour-angle) 360)))
         (sunrise (- 12 sunset-time-from-noon))
         (sunset (+ 12 sunset-time-from-noon)))
    (setq sky-color-clock--bg-color-gradient
          (sky-color-clock--make-gradient
           (cons (- sunrise 2.0)          "#111111")
           (cons (- sunrise 1.5)          "#4d548a")
           (cons (- sunrise 1.0)          "#c486b1")
           (cons (- sunrise 0.5)          "#ee88a0")
           (cons sunrise                  "#ff7d75")
           (cons (+ sunrise 0.5)          "#f4eeef")
           (cons (/ (+ sunset sunrise) 2) "#8fd7f1")
           (cons (- sunset  1.5)          "#9eefe0")
           (cons (- sunset  1.0)          "#f1e17c")
           (cons (- sunset  0.5)          "#f86b10")
           (cons sunset                   "#100028")
           (cons (+ sunset  0.5)          "#111111")))))

(defun sky-color-clock--pick-bg-color (time &optional cloudiness)
  "Pick a color from sky-color-clock--bg-color-gradient and
saturate according to CLOUDINESS. CLOUDINESS can be a number from
0 to 100."
  (unless sky-color-clock--bg-color-gradient
    (error "sky-color-clock-initialize is not called."))
  (cl-destructuring-bind (sec min hour . _) (decode-time time)
    (let ((cloudiness (if cloudiness (/ cloudiness 100.0) 0.00))
          (color (funcall sky-color-clock--bg-color-gradient (+ (/ (+ (/ sec 60.0) min) 60.0) hour))))
      (cl-destructuring-bind (h s l) (apply 'color-rgb-to-hsl (color-name-to-rgb color))
        (apply 'color-rgb-to-hex
               (color-hsl-to-rgb h (- s (* s cloudiness)) (- l (* l (/ cloudiness 2)))))))))

(defun sky-color-clock--pick-fg-color (color)
  (cl-destructuring-bind (h s l) (apply 'color-rgb-to-hsl (color-name-to-rgb color))
    (apply 'color-rgb-to-hex
           (color-hsl-to-rgb h s (+ l (if (> l 0.5) -0.5 0.5))))))

(defun sky-color-clock-preview ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*sky-color-clock*"))
  (erase-buffer)
  (let ((sky-color-clock-enable-moonphase-emoji nil)
        (sky-color-clock-format "%H:%M"))
    (dotimes (hour 23)
      (dolist (min '(0 5 10 15 20 25 30 35 40 45 50 55))
        (dolist (cloudiness '(0 30 60 90))
          (dolist (temperature '(273 283 293))
            (insert (sky-color-clock (encode-time 0 min hour 1 7 2018) cloudiness temperature)
                    " ")))
        (insert "\n")))))

;; ---- temperature

(defvar sky-color-clock--temperature-color-gradient
  (sky-color-clock--make-gradient '(243 . "#0000ff") '(283 . "#ffffff") '(323 . "#ff0000")))

(defun sky-color-clock--temperature-indicator (basecolor &optional temperature)
  (if (null temperature) ""
    (let ((color (sky-color-clock--blend-colors
                  basecolor
                  (funcall sky-color-clock--temperature-color-gradient temperature))))
      (propertize " " 'face `(:background ,color)))))

;; ---- emoji moonphase

(defconst sky-color-clock--newmoon 6.8576
  "A new moon (1970/01/08 05:35) in days since the epoch.")

(defconst sky-color-clock--moonphase-cycle 29.5306
  "Eclipse (synodic month) cycle in days.")

(defun sky-color--emoji-moonphase (time)
  (let* ((time-in-days (/ (float-time time) 60 60 24))
         (phase (mod (- time-in-days sky-color-clock--newmoon) sky-color-clock--moonphase-cycle)))
    (cond ((<= phase  1.84) "🌑")
          ((<= phase  5.53) "🌒")
          ((<= phase  9.22) "🌓")
          ((<= phase 12.91) "🌔")
          ((<= phase 16.61) "🌕")
          ((<= phase 20.30) "🌖")
          ((<= phase 23.99) "🌗")
          ((<= phase 27.68) "🌘")
          (t                "🌑"))))

;; ---- the clock

(defun sky-color-clock (&optional time cloudiness temperature)
  "Generate a fontified time string according to
`sky-color-clock-format' and
`sky-color-clock-enable-moonphase-emoji'."
  (let* ((time (or time (current-time)))
         (cloudiness (or cloudiness (sky-color-clock--cloudiness)))
         (temperature (or temperature (sky-color-clock--temperature)))
         (bg (sky-color-clock--pick-bg-color time cloudiness))
         (fg (sky-color-clock--pick-fg-color bg))
         (str (concat " " (format-time-string sky-color-clock-format time) " ")))
    (when sky-color-clock-enable-moonphase-emoji
      (setq str (concat " " (sky-color--emoji-moonphase time) str)))
    (setq str (propertize str 'face `(:background ,bg :foreground ,fg)))
    (when sky-color-clock-enable-temperature-indicator
      (setq str (concat str (sky-color-clock--temperature-indicator bg temperature))))))

(provide 'sky-color-clock)
