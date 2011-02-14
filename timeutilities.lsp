#!/usr/bin/env newlisp

;; @module Time
;; @author cormullion <cormullion@mac.com>
;; @version 2010-09-21 19:54:26 Rewrite of durations code
;; @version 2010-01-14 14:24:48 Updated for new-style FOOP 10.1.8 and above
;; @description Simple time and date utility functions using >= newLISP 10.1.8 FOOProgramming.
;;<h4>About the Time and Duration modules</h4>
;;<p>The Time module provides some basic time/date utility functions, available in a FOOP package. The Duration module provides a few functions for working with periods of time.</p>
;;<p>A 'time object' defines a moment in UTC and the time zone offset in minutes of the user's local time. Time objects can be created using the Time constructor function.</p>
;;<p>These functions work only for dates between 1970 and 2038. I suspect they are very operating-system dependent, but I don't know yet.</p>
;;<p>TO-DO: rewrite all the time zone stuff! It's all wrong :) </p>
;;<p> Lutz kindly updated this for newLISP v10... :) </p>
;;<p><b>Creating time objects</b></p>
;;<p>To create a time object, use the default function Time followed by up to 7 arguments:</p>
;;<pre>
;;(Time)                        the time now 
;;(Time n)                      n is the number of seconds since 1970 
;;(Time 2007 5)                 May [1st] 2007 [00:00:00] 
;;(Time 2008 5 6)               May 6th 2008 [00:00:00] 
;;(Time 2008 5 6 14)            May 6th 2008 14[:00:00] 
;;(Time 2008 12 24 23 59 0 300) a minute before midnight of Christmas Day 2008, for the time zone 5 hours west of UTC.
;;(Time (parse-date "2007.1.3" "%Y.%m.%d")) - (unavailable on Windows) use the specified date format and data 
;;</pre>
;;<p>The default parameters are (1970 1 1 0 0 0) and the default time zone is the user's local zone.
;;<p><b>Methods for displaying and querying time objects</b></p>
;;<p>Various methods are provided for displaying the information stored in a time object. After creating a time object:</p>
;;<pre>(set 'xmas (Time 2008 12 25 0 0 0 300))</pre>
;;<p>you can use the following methods on it:</p>
;;<pre>
;;(:show xmas)          	 returns "Thu Dec 25 05:00:00 2008" 
;;(:data xmas)               returns (2008 12 25 0 0 0) 
;;(:hour xmas)               returns 0 
;;(:year xmas)               returns 2008 
;;(:day-name xmas)           returns "Thursday" 
;;(:month-name xmas)         returns "December" 
;;(:day-of-week xmas)        returns 4
;;(:day-of-year xmas)        returns 360
;;(:days-in-month xmas)      returns 31
;;(:show-time (:utc xmas))   returns "00:00:00"
;;(:leap-year? xmas)         returns true
;;(:dst? xmas)               returns nil, no daylight saving time in force then
;;(:show (:utc xmas))        returns the UTC time "Wednesday December 24 2008 19:00:00"
;;(:week-number-sunday xmas) returns 51
;;(:midnight xmas)           returns time object for midnight on that day
;;(:week-number-monday xmas) returns 51
;;(:to-julian xmas)          returns 2454825.501, the Julian date number
;;(:set-zone xmas 300)       set time zone of xmas to +300 (travel west by 5 hours)
;;</pre>
;;<p><b>Date output</b></p>
;;<pre>
;;(:show xmas)                returns the object formatted using default date format string
;;(:format-date xmas "%Y %m") returns "2008 12" just like newLISP's (date) formatting           
;;(:rfc822 xmas)              returns "Thu, 25 Dec 2008 00:00 GMT"
;;(:iso8601 xmas)             returns "2008-12-25T00:00:00Z"
;;(:iso8601-stamp xmas)       returns "20081224190000"
;;</pre>
;;<p><b>Date calculations</b></p>
;;<p>Some calculations with dates are possible. Some return new objects, others returns lists or numbers.</p>
;;<p>The 'shift' method modifies date objects, so copy them with 'copy' first.</p>
;;<pre>
;;(:period xmas (Time))      returns 107.2556019, the days between the two time objects
;;(:shift xmas -48 "hours")) returns the xmas object moved 48 hours earlier
;;(:show (:shift (copy xmas) 12 "days"))
;;                           returns a new object moved 12 days after xmas
;;</pre>
;;<p>Some functions are not methods...</p>
;; (Duration:list-to-string (:duration->list (Duration (:period (Time) xmas))))
;;                           returns "15d 01h 52m 15s" the period of time from now till Xmas
;;</pre>
;;<p><b>Durations - periods of time</b></p>
;;<p>A Duration object represents a period of time. It consists of two integers: the number of days, and the number of milliseconds in a day.
;;You can create a Duration object using any of the following:</p>
;;<pre>
;;(Duration 0.5) 				    ; 12 hours
;;(Duration 1.5) 					; 1 day and 12 hours
;;(Duration 3 12 30 15) 			; 3 days, 12 hours, 30 minutes, and 15 seconds
;;(Duration 12 30 15) 				; 12 hours, 30 minutes, and 15 seconds
;;(Duration)     					; 1 second (default)
;;(Duration (:period xmas (Time))) 	; time from now till Xmas
;;</pre>
;;<p>Duration methods include:</p>
;;<pre>(:duration-&gt;ms (Duration (:period (Time) xmas)))</pre>
;;<p>which converts a Duration object to millisconds</p>
;;<pre>
;;-&gt; 8413412999
;;(:show (Duration (:period (Time) xmas)))
;;-&gt; "97 days 9 hours, 1 minutes, 41 seconds, 999 ms"
;
;;(:+ d1 d2) 						; adds two duration objects
;;(set 'to-next-xmas     (Duration (:period (Time) (Time 2010 12 25))))
;;(set 'to-previous-xmas (Duration (:period (Time) (Time 2009 12 25))))
;;(:show (:+ to-next-xmas to-previous-xmas))
;;-&gt; "366 days 0 hours, 0 minutes, 0 seconds, 999 ms"
;; ; hmm, that needs thinking about ... :)
;;</pre>
;;<p>Non-methods:</p>
;;<pre><code>(Duration:ms-&gt;list n) ; convert n milliseconds to a list format
;;(Duration:ms-&gt;list (:duration-&gt;ms (Duration (:period (Time) xmas))))
;;-&gt; (97 9 0 21 0) ; days hours minutes seconds milliseconds list
;;</pre>
;;<pre>(:show  (Time:day-to-date 75 2008)) returns "Sunday March 16 2008 00:00:00", day 45 of year 2008
;;(Time:decimalhours-to-hmslist 0.25)
;;                           returns hours/minutes/seconds list of the hours passed: eg (6 0 0), or 06:00
;;(Duration:list-to-string (list 1 22 13 34))
;;                           returns "01d 22h 13m 34s"
;;(Duration:list-to-string (:duration->list (Duration (:period (Time) xmas))))
;;                           returns "106h 04m 13s", the period between now and xmas as a formatted string
;; 
;;</pre>

(new Class 'Time)

(unless extend (define-macro (extend)
  (setf (eval (args 0)) (append (eval (args 0)) (eval (args 1))))))

(define (Time:Time)
;; @syntax (Time)
;; @syntax (Time <int-secs-since-1970>)
;; @syntax (Time <int-year> <int-month>)
;; @syntax (Time <int-year> <int-month> <int-day>)
;; @syntax (Time <int-year> <int-month> <int-day> <int-hour>)
;; @syntax (Time <int-year> <int-month> <int-day> <int-hour> <int-minute>)
;; @syntax (Time <int-year> <int-month> <int-day> <int-hour> <int-minute> <int-second>)
;; @syntax (Time <int-year> <int-month> <int-day> <int-hour> <int-minute> <int-second> <int-zone>)
;; @return Returns a time object, consisting of class-ID/seconds-UTC-since-1970-timezone-offset-minutes-west
;; @example
;; (Time 2008 5 6)
;; =>
;; (Time 1210032000 0)
;;
;; (set 'xmas (Time 2009 12 25))
;; =>
;; (Time 1261699200 0)
  (letn ((time-now (now))
         (current-time-zone (time-now 9))
         ; the time is set relative to your current time zone (offset and dst?)
         ; what does it mean when the user supplies another offset for zone?
         ; any additional supplied time zone is then... ?
         )
    (set 'Time:current-time-zone current-time-zone)
    (cond 
        ; no arguments, create UTC time object for the current time
        ((empty? (args))  
              (list Time (date-value) current-time-zone))
        ; one integer argument is number of seconds, but add info about current time zone
        ((and (= 1 (length (args))) (integer? (first (args))))
              (list Time (first (args)) current-time-zone))
        ; more than one argument is a set of numbers: eg 2008 12 [31 [23 [59 [59 [300]]]]]
        (true
              ; build a provisional list, replacing any missing values in the input with our fallback
           (local (fallback temp without-dst with-dst dst-adjust time-zone-adjust time-difference)
              (set 'fallback (0 3 time-now)) ; fallback is the current year month day...
              (extend fallback '(0 0 0 0))   ; but midnight gmt zone...
              (set 'temp (map (fn (def supplied) (if (nil? supplied) def supplied)) fallback (args)))
              ; work out DST and time zone offsets
              (set 'without-dst (apply date-value temp))
              (set 'with-dst (apply date-value (map (fn (i) (int i 0 10)) (parse (date (apply date-value temp) 0 {%Y %m %d %H %M %S}) { } 0))))              
              (set 'dst-adjust (- with-dst without-dst)) ; in seconds 
              (set 'time-zone-adjust (* -1 60 (last temp))) ; convert minutes to seconds, west to minus
              (set 'time-difference (- time-zone-adjust dst-adjust)) 
              ; we're storing as UTC and offset relative to local computer...
              (list Time without-dst (/ time-difference 60)))
          ))))

; some class variables

(define Time:current-time-zone)
(define Time:month-days '(nil 31 28 31 30 31 30 31 31 30 31 30 31))
(define Time:default-date-format-string "%A %B %d %Y %X") ; not working on BSD?
(define Time:default-time-format-string "%X")

(define (Time:show)
;; @syntax (:show)
;; @return Returns a string version of a time object, converting the UTC time to this 
;; computer's local time-zone and/or zone as stored. 
;; Uses the default format string stored in Time:default-date-format-string.
;; @example
;; (:show  xmas)
;; =>
;; "Thursday December 25 2008 00:00:00"
; date assumes the first arg is in Coordinated Universal Time (UTC; formerly Greenwich Mean Time (GMT)) 
; and converts it according to the local time-zone or the given zone in (self 2). 
   (date (self 1) (self 2) Time:default-date-format-string))

(define (Time:set-zone (z 0))
;; @syntax (:set-zone)
;; @return changes time zone of object to z minutes; returns changed object
;; @example
;; (set 'x (Time 2005 5 26 12 0 0 -300))
;; => (Time 1117123200 -300)
;; (:show x)
;; => "Thursday May 26 2005 12:00:00"
;; (:set-zone x 300)
;; => (Time 1117123200 300)
;; z should be multiple 
   (setf (self 2) z)
   (self))

(define (Time:zone)
;; @syntax (:zone)
;; @return minutes (time zone) slower (west or + ) relative to greenwich Z (UTC)
   (self 2))

(define (Time:dst?)
;; @syntax (:dst?)
;; @return Returns true if daylight savings time was in force for time object
;; @example
;; (:dst? xmas)
;; =>
;; nil
; more than you want to know about UK summertime at http://www.merlyn.demon.co.uk/uksumtim.htm
    (!= 
      (date (apply date-value (:data (self)))) 
      (date (self 1) (self 2))))

(define (Time:show-time t)
;; @syntax (:show-time)
;; @return Returns string representing just the time for the object. Uses the default format string stored in Time:default-time-format-string.
;; @example
;; (:show-time (Time))
;; =>
;; nil
;;(:show-time (:utc (Time)))
;; =>
;; "17:07:18"
;;(:show-time (:shift (Time) 30 "minutes"))
;; =>
;; "18:37:37"
    (date (self 1) (self 2) Time:default-time-format-string))

(define (Time:utc)
;; @syntax (:utc <obj-time>)
;; @return Returns new time object containing the UTC time of the object
;; @example
;; (:show (:utc xmas))
;; =>
;; "Thursday December 25 2008 00:00:00"
  (local (correction)
    (if (:dst? (self))
        (set 'correction (+ 60 Time:current-time-zone)) 
        (set 'correction Time:current-time-zone))
    (list Time (- (self 1) (* correction 60)) 0)))

(define (Time:data)
;; @syntax (:data)
;; @return Returns a list of data representing the time object (Year/Month/Day/Hour/Minute/Second/Zone)
;; @example
;; (:data xmas) 
;; =>
;; (2008 12 25 0 0 0 0)
  (local (results)
    (dolist (d (parse (date (self 1) (self 2) {%Y %m %d %H %M %S %Z}) { } ))
            (push (int d 0 10) results -1))
    results))

(define (Time:unix-time)
;; @syntax (:unix-time)
;; @return Returns time in unix seconds
    (self 1))

(define (Time:year)
;; @syntax (:year )
;; @return Returns year of object as integer. 
;; @example 
;; (:year xmas) 
;; =>
;; 2008
  ((:data (self)) 0))

(define (Time:month t)
;; @syntax (:month)
;; @return Returns month of object as integer
;; @example
;; (:month xmas ) 
;; =>
;; 12
  ((:data (self)) 1))

(define (Time:day) 
;; @syntax (:day)
;; @return Returns day of object as integer 
;; @example
;; (:day xmas ) 
;; =>
;; 25
  ((:data (self)) 2))

(define (Time:hour t)
;; @syntax (:hour)
;; @return Returns hour of object as integer 
;; @example
;; (:hour (Time)) 
;; =>
;; 17
  ((:data (self)) 3))

(define (Time:minute)
;; @syntax (:minute)
;; @return Returns minute of object as integer
;; @example
;; (:minute (Time)) 
;; =>
;; 14
  ((:data (self)) 4))

(define (Time:second)
;; @syntax (:second)
;; @return Returns second of object as integer
;; @example
;; (:second (Time)) 
;; =>
;; 59
  ((:data (self)) 5))

(define (Time:day-name)
;; @syntax (:day-name)
;; @return Returns name of day of object
;; @example
;; (:day-name xmas) 
;; =>
;; "Thursday"
  (date (self 1) (self 2) {%A}))

(define (Time:month-name)
;; @syntax (:month-name)
;; @return Returns name of month of object
;; @example
;; (:month-name xmas) 
;; =>
;; "December"
  (date (self 1) (self 2) {%B}))

(define (Time:day-of-week)
;; @syntax (:day-of-week)
;; @return Returns integer for day of week of object, Sunday being 0
;; @example
;; (:day-of-week xmas ) 
;; =>
;; 4
  (int (date  (self 1) (self 2) {%w}) 0 10))

(define (Time:day-of-year)
;; @syntax (:day-of-year)
;; @return Returns integer for day of the year
;; @example
;; (:day-of-year xmas) 
;; =>
;; 360
  (int (date (self 1) (self 2) {%j}) 0 10))

(define (Time:week-number-sunday)
;; @syntax (:week-number-sunday)
;; @return Returns number of week of object, counting from the week containing the first Sunday in the year. (? Discuss.)
;; @example
;; (:week-number-sunday (Time 2007 1))
;; =>
;; 0
  (int (date (self 1) (self 2) {%U}) 0 10))

(define (Time:week-number-monday)
;; @syntax (:week-number-monday)
;; @return Returns number of week of object, determined by the first Monday.
;; @example
;; (:week-number-monday (Time 2007 1)) 
;; =>
;; 1
  (int (date (self 1) (self 2) {%W}) 0 10))

(define (Time:days-in-month)
;; @syntax (:days-in-month)
;; @return Returns number of days in the month containing the object
;; @example
;; (:days-in-month (Time 2008 2)) 
;; =>
;; 29
;; (:days-in-month (Time 2007 2))
;; 28
  (let ((res (nth (:month (self)) Time:month-days)))
    (if (and (:leap-year? (self))
             (= 2 (:month (self))))
        (inc res 1) 
        res)))

(define (Time:midnight)
;; @syntax (:midnight)
;; @return new time object for the beginning of that day
   (let (tm (apply Time (append (0 3 (:data (self))) (dup 0 3))))
        (list Time (tm 1) 0)))        
        
(define (Time:to-julian)
;; @syntax (:to-julian <obj-time>)
;; @return Returns Julian day number of object
;; @example
;; (:to-julian xmas) 
;; =>
;; 2454825.5 
  (letn 
     ((u (:utc (self)))
      (year (:year u))
      (month (:month u))
      (day (:day u))
      (hour (:hour u))
      (minute (:minute u))
      (second (:second u))
      (a (floor (div (sub 14 month) 12))) 
      (y (sub (add year 4800) a)) 
      (m (sub (add month (mul 12 a)) 3)) 
      (jdn (sub 
                (add day (floor (div (add (mul 153 m) 2) 5)) (mul 365 y) 
                (floor (div y 4)) 
                (floor (div y 400))) 
           (floor (div y 100)) 32045)) 
      (jd (add jdn (div (sub hour 12) 24) (div minute 1440) (div  second 86400))))
  jd
  ))

(define (Time:from-julian jdate)
;; @syntax (from-julian <number-jdate>)
;; @return (Not a method!) Convert a Julian date number in <jdate> to a new time object
;; Unfortunately Julian dates before 1970 or after 2038 probably aren't useful.
;; @example
;; (:show (Time:from-julian 2446113.75))
;; =>
;; "Sunday February 17 1985 06:00:00"
(local (j i f a b c d e g dy mnth yr)
    (set 'j (add 0.5 jdate))
    (set 'i (floor j))
    (set 'f (sub j i))
    (if (> i 2299160)
        (set 'a (floor (div (sub i 1867216.25) 36524.25)) 'b (sub (add i a 1 ) (floor (div a 4))))
        (set 'b i))
    (set 'c (add b 1524))
    (set 'd (floor (div (sub c 122.1) 365.25)))
    (set 'e (floor (mul 365.25 d)))
    (set 'g (floor (div (sub c e) 30.6001)))
    (set 'dy (sub (add f (sub c e)) (floor (mul 30.6001 g))))
    (if (< g 13.5)
        (set 'mnth (- g 1))
        (set 'mnth (- g 13)))
   (if (> mnth 2.5)
        (set 'yr (- d 4716))
        (set 'yr (- d 4715)))
   ; return time object with our data
   (eval (append (list Time yr mnth (int dy)) (Time:decimalhours-to-hmslist (sub dy (floor dy))) '(0)))))

(define (Time:utc-difference-seconds)
;; @syntax (:utc-difference-seconds)
;; @return Returns offset of object from UTC in seconds. 
;; @example
;; (:utc-difference-seconds (Time 2008 12 25 6 0 0 300))
;; =>
;; 18000
    (* (self 2) 60))

(define (Time:zone-minutes)
;; @syntax (:zone-minutes)
;; @return Returns number of minutes of offset of time zone of object
;; @example
;; (:zone-minutes (Time 2008 12 25 6 0 0 300))
;; => 
;; 300
  (div (:utc-difference-seconds (self)) 60))

(define (Time:leap-year?)
;; @syntax (:leap-year? <obj-time>)
;; @return Returns true if object falls in a leap year
;; @example
;; (:leap-year? (Time 2000 1)) 
;; =>
;; true
  (let ((year (int (date (self 1) (self 2) {%Y}))))
    (and (= 0 (% year 4)) 
         (or  (!= 0 (% year 100))  
              (= 0 (% year 400))))))

(define (Time:period t1)
;; @syntax (:period <obj-time>)
;; @return Returns the period of time between the two objects in days and decimal fractions of day
;; @example
;; (:period xmas (Time))
;; =>
;; 106.2833912 
(let ((time1 (nth 1 (:utc (self))))
      (time2 (nth 1 (:utc t1))))
    (div (abs (- time1 time2)) (* 24 60 60))))

(define (Time:shift x s)
;; @syntax (:shift <number-amount> <sym-unit>)
;; @return Modifies and returns time object by shifting it forward or back by <amount> <unit>s.
;; @return Returns modified object.
;; <string-unit> is one of "weeks"/"days"/"hours"/"minutes"/"seconds" (or singular versions)
;; These used to be symbols, but strings are easier and don't get confused by contexts... 
;; Use <copy> if you don't want to modify the object.
;; @example
;; (set 'boxing-day (:shift (copy xmas) 1 "day")) 
;; (:show boxing-day)
;; =>
;; "Friday December 26 2008 00:00:00"
;;
;; (:show (:shift (:shift xmas 2.5 "hours") 1 "day"))
;; =>
;; "Friday December 26 2008 02:30:00"
 (let ((f 0))
  (cond 
   ((find s "weeks")
        (set 'f (mul x (* 7 24 60 60))))
   ((find s "days")
        (set 'f (mul x (* 24 60 60))))
   ((find s "hours")
        (set 'f (mul x (* 60 60))))
   ((find s "minutes")
        (set 'f (mul x (* 60))))
   ((find s "seconds")
        (set 'f x)))
   (setf (self 1) (add f (self 1)))
   (self)))
   
(define (Time:format-date s) 
;; @syntax (:format-date <str-format>)
;; @return Returns string describing object formatted using <str>
;; @example
;; (:format-date xmas "%x %X") 
;; =>
;; "12/25/08 00:00:00"
  (date (self 1) (self 2) s))

(define (Time:rfc822)
;; @syntax (:rfc822)
;; @return Returns string describing object formatted to RFC822 spec
;; @example
;; (:rfc822 xmas) 
;; =>
;; "Thu, 25 Dec 2008 00:00 GMT"
  (:format-date (self) "%a, %d %b %Y %H:%M %Z"))

(define (Time:iso8601) 
;; @syntax (:iso8601)
;; @return Returns string describing object formatted to ISO 8601 spec
;; @example
;; (:iso8601 xmas)
;; =>
;; 2008-12-25T00:00:00Z
  (:format-date (self) "%Y-%m-%dT%H:%M:%SZ"))

(define (Time:iso8601-stamp)
;; @syntax (:iso8601-stamp <obj-time>)
;; @return Returns 14 character string describing object formatted to ISO 8601
;; @example
;; (:iso8601-stamp xmas) 
;; =>
;; "20081225000000"
 (:format-date (:utc (self)) "%Y%m%d%H%M%S"))

(define (Time:day-to-date n y)
;; @syntax (day-to-date <number-day> <number-year>)
;; @return (Not a method!) Returns a new time object for day n of year y
;; @example
;; (:show (Time:day-to-date 233 2008)) 
;; =>
;; "Wednesday August 20 2008 01:00:00"
  (letn ((jd-year-start (:to-julian (Time y 1 0 0 0 0))) ; that 0 for day might be correct...
         (jd-target (add n jd-year-start)))
     (Time:from-julian jd-target)))

(define (Time:decimalhours-to-hmslist n)
;; @syntax (decimalhours-to-hmslist <number-fracday>)
;; @return (Not a method!) Return (h m s) given a fraction of a day (0 < n < 1) in <n>
;; @example
;; (Time:decimalhours-to-hmslist 0.6666666667)
;; => 
;; (16 0 0)
; not a method call!!!
 (letn ((h (mul n 24))
        (m (mul 60 (sub h (floor h))))
        (s (mul 60 (sub m (floor m)))))
       (list (int h 0 10) (% m 60) (% s))))



(new 'Class 'Duration)
; a Duration object consists of two integers: days, and the milliseconds in a part of a day

(constant 'ms-per-day (* 24 60 60 1000))

(define (Duration:Duration)
;; @syntax (Duration <number-duration-days>)
;; @return Returns a new duration object representing <number-duration-days>
;; <p>The Duration class provides a few methods for handling periods of time.</p>
;; @example
;; (Duration) ; 0 length
;; (Duration 1) ; 1 day
;; (Duration 1.5) ; 1 day 12 hours
;; (Duration 12 1 30) ; 12 hours 1 minute and 30 seconds
;; (Duration 2 12 1 30) ; 2 days 12 hours 1 minute and 30 seconds
;; (Duration 2 12 1 30 750 ) ; 2 days 12 hours 1 minute 30 seconds and 750 milliseconds
;; (Duration (:period (Time) xmas)) ; :period is a Time method working on two Time objects...
;; =>
;; (Duration 106.2620139)
;; (:duration->list (Duration (:period (Time) xmas)))
;; =>
;; (106 6 16 38)
;; (set 'till-xmas (apply Duration (:duration->list (Duration (:period (Time) xmas)))))
(local (n d m h s ms)
    (cond    
           ; one argument is days as decimal eg 1.234
         ((= 1 (length (args)))
               (set 'n (int (args 0) 0 10) 'm (sub (args 0) n))
               ; convert m to milliseconds
               (list Duration n (round (mul m 86400 1000)))) ; there's some FP errors somewhere here
         ; 3: h m s where s is integer or decimal seconds
         ((= 3 (length (args))) 
               (set 'h (* (args 0) 3600))
               (set 'm (* (args 1) 60))
               (set 's (args 2))
               
               ; is s an integer or a decimal?
               (if (integer? s) 
                 (set 'ms 0) 
                 (set 'ms (mod s 1) 's (int s)))              
               
               ; h m s are in seconds,  ms is decimal < 1

               (set 'd (/ (+ h m s (* 1000 ms)) 86400))
               (list Duration d (+ (mul 1000 ms) (* (% (+ h m s) 86400) 1000))))
               
         ; d h m s
         ((= 4 (length (args)))
               (set 'h (* (args 1) 3600))
               (set 'm (* (args 2) 60))
               (set 's (args 3))
               
               (if (integer? s) 
                 (set 'ms 0) 
                 (set 'ms (mod s 1) 's (int s)))              
              
               ; h m s are in seconds
               (set 'd (+ (args 0) (/ (+ h m s (* 1000 ms)) 86400)))
               (list Duration d (* (% (+ h m s (* 1000 ms)) 86400) 1000)))                  

     	; d h m s ms
         ((= 5 (length (args)))
               (set 'h (* (args 1) 3600))
               (set 'm (* (args 2) 60))
               (set 's (args 3))
			   (set 'ms (div (args 4) 1000))
               ; h m s are in seconds, ms is in milliseconds, d is in days
               (set 'd (round (add (args 0) (div (add h m s ms) 86400))))
               (list Duration d (round (mul (mod (add h m s ms) 86400) 1000))))                  
         (true
               (list Duration 0 1000)))))
   
(define (Duration:show)
;; @syntax (:show)
;; @return Returns a string describing the duration object in days 
;; @example
;; (:show (Duration (:period (Time) xmas)))
;; =>
;> (set 'xmas (Time 2010 12 25 0 0 0 300))
;(Time 1293235200 -300)
;> (:period (Time) xmas)
;97.66793981
;> (Duration (:period (Time) xmas))
;(97.66793981)
;(Duration 97 57710000)
;> (:duration->ms (Duration (:period (Time) xmas)))
;(97.66793981)
;8438510000
;> (:show (Duration (:period (Time) xmas)))
;(97.66792824)
;"97 days 16 hours, 1 minutes, 48 seconds, 999 ms"
;> 
  (let ((m (Duration:ms->list (:duration->ms (self)))))
      (format "%d days %d hours, %d minutes, %d seconds, %d ms" m)))

(define (Duration:duration->ms)
; method to convert duration to milliseconds
   (+ (* (self 1) 86400000) (self 2)))

(define (Duration:ms->list n)
; convert loads of milliseconds to day hour min sec string
; not a method
; this is the equivalent of:
;   (letn 
;    ((msecs (% n 1000))
;     (x (/ n 1000))
;     (secs (% x 60))
;     (x (/ x 60))
;     (mins (% x 60))
;     (x (/ x 60))
;     (hours (% x 24))
;     (x (/ x 24))
;     (days x))
;   (set 'result (list days hours mins secs msecs)))
;   result)
  (let ((factors '(1000 60 60 24))
        (result '()))
    (map (fn (f) (push (% n f) result) (set 'n (/ n f))) factors)
    ; do days separately
	(push n result)
  result))

(define (Duration:list-to-string l)
;; @syntax (Duration:list-to-string <list-l>)
;; @return (Not a method!) Returns a string version of the list (day hour min sec). All four values are optional. 
;; @example
;; (Duration:list-to-string (list 1 22 13 34))
;; =>
;; "01d 22h 13m 34s"
;; (Duration:list-to-string (:duration->list (Duration (:period (Time) xmas))))
;; =>
;; "106h 04m 13s"
;; (Duration:list-to-string '(56 22))
;; =>
;; "00d 00h 56m 22s"
  (join 
     (map (fn (a b) (format (string "%02d" a) b))
        (list "d" "h" "m" "s" ) 
        (slice (append '(0 0 0 0 ) l) -4))
     { }))

(define (Duration:+ d)
;; @syntax (Duration:+ <duration-1> <duration-2>)
;; @return new Duration object, the result of adding the two durations
;; @example
;; (set 'd1 (Duration 1 0 0))
;; (set 'd2 (Duration 0 1 0))
;; (println (:show (:+ d1 d2)))
;; =>
;; 0 days 1 hours, 1 minutes, 0 seconds, 0 ms
   (letn ((d1 (self 1)) ;  first object day
          (d2 (d 1))    ;  second object day
          (m1 (self 2)) ;  first object ms
          (m2 (d 2))    ;  second object ms  
         ; convert to total milliseconds
          (tm (+ m1 m2))
          (td (+ d1 d2))
          (total-ms (+ (* td ms-per-day) tm)))
   (list Duration (/ total-ms ms-per-day) (% total-ms ms-per-day))))

(define (Duration:- d)
; finds the difference between two duration objects and returns a new duration object
   (letn ((d1 (self 1)) ;  first object day
          (d2 (d 1))    ;  second object day
          (m1 (self 2)) ;  first object ms
          (m2 (d 2))    ;  second object ms  
          ; convert each to milliseconds
          (t1 (+ (* d1 ms-per-day) m1))
          (t2 (+ (* d2 ms-per-day) m2))
		  ; find difference 
          (total-ms (abs (- t1 t2))))
      ; return new object
      (list Duration (/ total-ms ms-per-day) (% total-ms ms-per-day))
    ))

(context MAIN)
; eof