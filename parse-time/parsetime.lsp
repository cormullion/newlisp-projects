#!/usr/bin/env newlisp

;; @module parsetime
;; @description a module for converting date/time strings into dates
;; @version 2011-09-09 16:25:03 
;;          2010-09-22 19:26:07 modified 'cos Lutz broke my code again ('read ' problems)
;; @author cormullion
;; Requires timeutilities.lsp
  
(context 'ParseTime)

(set 'rules-db '(
; the rule database
; consists of patterns of elements, followed by an example, followed by a recipe for converting the 
; pattern into data suitable for use when constructing a new Time object, typically year/month/day/hour/minute/second.
; In the recipe, use the elements in the numbered order.
; For example, this is a rule/recipe:
;   ((number period number period year) ("31.12.1991") (4 2 0)) 
; If a string in the format "31.12.1991" is encountered, elements 4 2 and 0 are selected and used to build a time.
((alphabetic number monthname year number colon number) ("mardi 10 avril 2001 15:51")  (3 (month-number 2) 1 4 6))
((alphabetic number monthname year) ("midnight 18 April 2004") (3 (month-number 2) 1 (zero)))
((alphabetic punctuation alphabetic number monthname punctuation year) ("Mittwoch, den 4 May, 2006") (6 (month-number 4) 3 ))
((alphabetic punctuation number alphabetic punctuation year) ("Mercoledi, 4 Maggio, 2006") (5 (month-number 3) 2))
((alphabetic punctuation number monthname punctuation year) ("Mercoledi, 4 Maggio, 2006")  (5 (month-number 3) 2))
((dayname monthname number number colon number colon number year ) ("Mon Mar 19 20:05:02 2006") (8 (month-number 1) 2 3 5 7)) 
((dayname monthname number punctuation year punctuation number colon number alphabetic period alphabetic period) ("Wednesday August 13, 2009, 3:01 p.m.") (4 (month-number 1) 2 (pm-to-24 6) 8))
((dayname monthname number punctuation year punctuation number colon number colon number pm period) ("Wednesday August 13, 2009, 3:20:07 pm.") (4 (month-number 1) 2 (pm-to-24 6) 8 10))
((dayname monthname number punctuation year punctuation number colon number colon number)  ("Wednesday August 13, 2009, 3:22:08")  (4 (month-number 1) 2 6 8 10))
((dayname monthname number punctuation year punctuation number colon number pm period) ("Wednesday August 13, 2009, 3:02 pm.") (4 (month-number 1) 2 (pm-to-24 6) 8))
((dayname monthname number punctuation year punctuation number colon number pm) ("Wednesday August 13, 2009, 3:03 pm.") (4 (month-number 1) 2 (pm-to-24 6) 8))
((dayname monthname number punctuation year punctuation number colon number) ("Wednesday August 13, 2009, 3:04") (4 (month-number 1) 2 6 8))
((dayname number monthname year number colon number) ("Tuesday 31 August 1971 8:13") (3 (month-number 2) 1 4 6)) 
((dayname number monthname year) ("Tuesday 31 August 1971") (3 (month-number 2) 1))
((dayname number number-suffix monthname year number colon number) ("Tuesday 31st August 1971 8:13") (4 (month-number 3) 1 5 7)) 
((dayname punctuation alphabetic number monthname punctuation year) ("Mittwoch, den 4 May, 2006") (6 (month-number 4) 3))
((dayname punctuation monthname number number-suffix punctuation year ) ("Tuesday, August 31st, 1980") (6 (month-number 2) 3)) 
((dayname punctuation monthname number punctuation year number colon number alphabetic period alphabetic period) ("Wednesday, August 13, 2009 3:05 p.m.") (5 (month-number 2) 3 (pm-to-24 6) 8 (zero)))
((dayname punctuation number monthname punctuation year number colon number colon number) ("Wednesday, 13 August, 2009 10:00:00") (5 2 (month-number 3) 6 8 10 (zero))) 
((dayname punctuation monthname number punctuation year number colon number colon  number alphabetic period alphabetic period)  ("Wednesday, August 13, 2009 3:15:02 p.m.")  (5 (month-number 2) 3 (pm-to-24 6) 8 10))
((dayname punctuation monthname number punctuation year number colon number colon  number pm period)  ("Wed, August 13, 2009 3:15:01 pm.") (5 (month-number 2) 3 (pm-to-24 6) 8 10))
((dayname punctuation monthname number punctuation year number colon number colon  number)  ("Wednesday, August 13, 2009 3:16:03") (5 (month-number 2) 3 (pm-to-24 6) 8 10))
((dayname punctuation monthname number punctuation year number colon number colon number pm) ("Sunday, March 09, 2008 4:05:07 PM") (5 (month-number 2) 3 (pm-to-24 6) 8 10))
((dayname punctuation monthname number punctuation year number colon number pm period) ("Wed, August 13, 2009 3:06 pm.") (5 (month-number 2) 3 (pm-to-24 6) 8 (zero)))
((dayname punctuation monthname number punctuation year number colon number pm) ("Tuesday, April 10, 2001 3:51 PM") (5 (month-number 2) 3 (pm-to-24 6) 8 (zero)))
((dayname punctuation monthname number punctuation year number colon number) ("Wednesday, August 13, 2009 3:17") (5 (month-number 2) 3 6 8 (zero)))
((dayname punctuation monthname number punctuation year punctuation number colon  number colon number pm period) ("Wednesday, August 13, 2009, 3:17:04 pm.") (5 (month-number 2) 3 (pm-to-24 7) 9 11))
((dayname punctuation monthname number punctuation year punctuation number colon  number colon number) ("Wednesday, August 13, 2009, 3:19:06")  (5 (month-number 2) 3 7 9 11))
((dayname punctuation monthname number punctuation year punctuation number colon number alphabetic period alphabetic period) ("Wednesday, August 13, 2009, 3:18 p.m.") (5 (month-number 2) 3 (pm-to-24 7) 9))
((dayname punctuation monthname number punctuation year punctuation number colon number colon number alphabetic period alphabetic period) ("Wednesday, August 13, 2009, 3:18:05 p.m.") (5 (month-number 2) 3 (pm-to-24 7) 9 11))
((dayname punctuation monthname number punctuation year punctuation number colon number pm period) ("Wednesday, August 13, 2009, 3:17 pm.") (5 (month-number 2) 3 (pm-to-24 7) 9 (zero)))
((dayname punctuation monthname number punctuation year punctuation number colon number pm) ("Wednesday, August 13, 2009, 3:18 pm.") (5 (month-number 2) 3 (pm-to-24 7) 9 (zero)))
((dayname punctuation monthname number punctuation year punctuation number colon number) ("Wednesday, August 13, 2009, 3:20") (5 (month-number 2) 3 7 9 (zero)))
((dayname punctuation monthname number punctuation year) ("Tuesday, April 10, 2001") (5 (month-number 2) 3 (zero) (zero) (zero)))
((dayname punctuation monthname number year) ("Wednesday, July 23 2005") (4 (month-number 2) 3 (zero) (zero)(zero)))
((dayname punctuation number hyphen monthname hyphen number number colon number colon number alphabetic) ("Sunday, 06-Nov-94 08:49:37 GMT") ((year2 6) (month-number 4) 2 7 9 11 (zone 12))) 
((dayname punctuation number monthname year number colon number colon number alphabetic) ("Sun, 06 Nov 1994 08:49:37 GMT") (4 (month-number 3) 2 5 7 9 (zone 10))) 
((dayname punctuation number monthname year number colon number colon number hyphen numbermisc) ("Tue, 09 Jan 2002 22:14:02 -0500") (4 (month-number 3) 2 5 7 9 (zone 10 11)))
((dayname punctuation number monthname year number colon number colon number plus numbermisc) ("Thu, 21 Dec 2000 16:01:07 +0200") (4 (month-number 3) 2 5 7 9 (zone 10 11)))
((monthname number number colon number) ("April 26 23:59") ((year-current) (month-number 0) 1 2 4 (zero)))
((monthname number number) ("Jan 01 01") ((year2 2) (month-number 0) 1))
((monthname number punctuation number) ("Mar 4, 05") ((year2 3) (month-number 0) 1)) 
((monthname number punctuation year punctuation number colon number alphabetic period alphabetic period)  ("August 13, 2009, 3:24 p.m.")  (3 (month-number 0) 1 (pm-to-24 5) 7))
((monthname number punctuation year punctuation number colon number colon number alphabetic period alphabetic period)  ("August 13, 2009, 3:24:02 p.m.")  (3 (month-number 0) 1 (pm-to-24 5) 7 9))
((monthname number punctuation year punctuation number colon number colon number pm period)  ("August 13, 2009, 3:23:01 pm.") (3 (month-number 0) 1 (pm-to-24 5) 7 9))
((monthname number punctuation year punctuation number colon number colon number) ("August 13, 2009, 3:25:03") (3 (month-number 0) 1 (pm-to-24 5) 7 9))
((monthname number punctuation year punctuation number colon number pm period) ("August 13, 2009, 3:23 pm.")  (3 (month-number 0) 1 (pm-to-24 5) 7))
((monthname number punctuation year punctuation number colon number pm) ("August 13, 2009, 3:25 p.m.")  (3 (month-number 0) 1 (pm-to-24 5) 7))
((monthname number punctuation year punctuation number colon number) ("August 13, 2009, 3:26") (3 (month-number 0) 1 (pm-to-24 5) 7))
((monthname number punctuation year) ("April 18, 2004") (3 (month-number 0) 1)) 
((monthname number year) ("September 01 2010") (2 (month-number 0) 1)) 
((monthname number) ("April 10") ((year-current) (month-number 0) 1))
((monthname period number punctuation number colon number colon number alphabetic ) ("Dec. 29, 10:32:26 UTC") ((year-current) (month-number 0) 2 4 6 8 (zone 9))) 
((monthname punctuation number punctuation number) ("Apr/04/18") ((year2 2) (month-number 0) 4)) ; ambiguous - year/month or month/year
((monthname punctuation number punctuation number) ("Apr/04/18") ((year2 4) (month-number 0) 2)) ; ambiguous - year/month or month/year
((monthname punctuation year) ("April, 2001") ((year-current) (month-number 0) (one)))
((monthname year) ("April 2001") (1 (month-number 0) (one)))
((number alphabetic period alphabetic period monthname number punctuation year) ("3 a.m. apr 18, 2004") (8 (month-number 5) 6 0)) 
((number colon number alphabetic period alphabetic period) ("3:52 p.m.") ((year-current) (month-current) (day-current) (pm-to-24 0) 2 (zero)))
((number colon number alphabetic) ("3:51") ((year-current) (month-current) (day-current) 0 2 (zero))) 
((number colon number colon number alphabetic) ("23:59:59Z") ((year-current) (month-current) (day-current) 0 2 4 (zone))) 
((number colon number colon number period numbermisc) ("23:59:59.9942") ((year-current) (month-current) (day-current) 0 2 (to-decimal 4 5 6))) 
((number colon number colon number pm alphabetic) ("3:51:01 pm EST") ((year-current) (month-current) (day-current) (pm-to-24 0) 2 4 (zone 6)))
((number colon number colon number pm alphabetic) ("5:46:21 PM EST") ((year-current) (month-current) (day-current) (pm-to-24 0) 2 4 (zone 6)))
((number colon number colon number pm) ("3:51:01 pm") ((year-current) (month-current) (day-current) (pm-to-24 0) 2 4))
((number colon number colon number zulu) ("23:59:59Z") ((year-current) (month-current) (day-current) 0 2 4 (zone)))
((number colon number colon number) ("3:51:01") ((year-current) (month-current) (day-current) 0 2 4))
((number colon number plus number colon number) ("13:00+01:00") ((year-current) (month-current) (day-current) 0 2 (second-current) (zone 3 4 5 6))) 
((number colon number pm) ("3:51 pm") ((year-current) (month-current) (day-current) (pm-to-24 0) 2 (zero)))
((number colon number) ("3:51") ((year-current) (month-current) (day-current) 0 2 (zero))) 
((number hyphen monthname hyphen number number colon number colon number ) ("18-Apr-04 13:58:12") ((year2 4) (month-number 2) 0 5 7 9)) 
((number hyphen monthname hyphen number number colon number pm) ("24-sep-72 8:02pm") ((year2 4) (month-number 2) 0 (pm-to-24 5) 7 (zero)))
((number hyphen monthname hyphen number number colon number) ("24-sep-72 20:02") ((year2 4) (month-number 2) 0 5 7 (zero)))
((number hyphen monthname hyphen number number colon number colon number  pm) ("24-sep-72 8:02:04pm") ((year2 4) (month-number 2) 0 (pm-to-24 5) 7 9))
((number hyphen monthname hyphen number number colon number colon number ) ("24-sep-72 20:02:04") ((year2 4) (month-number 2) 0 5 7 9))
((number hyphen monthname hyphen number) ("29-Jan-02") ((year2 4) (month-number 2) 0))
((number hyphen monthname hyphen year) ("18-Apr-2004") (4 (month-number 2) 0)) 
((number hyphen number hyphen number number colon number colon number) ("13-10-05 10:19:26") ((year2 4) 2 0  5 7 9))
((number hyphen number hyphen number) ("04-05-06") ((year2 4) 2 0)) 
((number hyphen number hyphen number) ("04-05-06") ((year2 4) 0 2)) 
((number hyphen number hyphen year number colon number) ("08-13-2009 14:20") (4 0 2 5 7)) 
((number monthname number colon number colon number year) ("31 Dec 21:59:59 1999")  (7 (month-number 1) 0 2 4 6))
((number monthname number) ("01 Jan 01") ((year2 2) (month-number 1) 0))
((number monthname year) ("18 April 2004") (2 (month-number 1) 0)) 
((number monthname) ("10 April") ((year-current) (month-number 1) 0))
((number number number) ("12 12 12") ((year2 0) 1 2)) ; year first?
((number number number) ("12 12 12") ((year2 2) 0 1)) ; year last...?
((number number year) ("12 12 1992") (2 0 1)) ; ambiguous - US
((number number year) ("12 12 1992") (2 1 0))  ; ambiguous - UK
((number colon number pm alphabetic dayname punctuation number monthname year) ("8:10pm on Sunday, 16 Sept 1973")  (9 (month-number 8) 7 (pm-to-24 0) 2 ))
((number colon number alphabetic dayname punctuation number monthname year) ("8:10 on Sunday, 16 Sept 1973")  (8 (month-number 7) 6 0 2 ))
((number number year) ("12 12 92") ((year2 2) 0 1)) ; ambiguous - US
((number number year) ("12 12 92") ((year2 2) 1 0))  ; ambiguous - UK
((number number-suffix monthname punctuation number colon number alphabetic) ("23rd February, 8:15 am") ((year-current) (month-number 2) 0 4 6 (zero)))
((number number-suffix monthname punctuation number colon number colon number) ("23rd February, 8:15:32") ((year-current) (month-number 2) 0 4 6 8))
((number number-suffix monthname punctuation number colon number pm) ("23rd February, 8:16 pm")  ((year-current) (month-number 2) 0 (pm-to-24 4) 6 (zero)))
((number number-suffix monthname punctuation number colon number) ("23rd February, 8:17") ((year-current) (month-number 2) 0 4 6 (zero)))
((number number-suffix monthname year punctuation number colon number alphabetic)  ("23rd February 2009, 8:18 am")  (3 (month-number 2) 0 5 7 (zero)))
((number number-suffix monthname year punctuation number colon number) ("23rd February 2009, 8:19") (3 (month-number 2) 0 5 7 (zero)))
((number number-suffix monthname year) ("31st January 2012") (3 (month-number 2) 0))
((number oclock alphabetic) ("3 o'clock EST") ((year-current) (month-current) (day-current) 0 (zero) (zero) (zone 2))) 
((number oclock dayname number monthname year) ("4 o'clock Saturday 18 apr 2009") (5 (month-number 4) 3 0)) 
((number oclock dayname number monthname) ("4 o'clock Saturday 18 apr") ((year-current) (month-number 4) 3 0)) 
((number oclock monthname number punctuation year) ("3 o'clock apr 18, 2004") (5 (month-number 2) 3 0 (zero) (zero))) 
((number oclock) ("3 o'clock") ((year-current) (month-current) (day-current) 0 (zero) (zero))) 
((number period monthname period year) ("13.Apr.2006") (4 (month-number 2) 0))
((number period monthname year period) ("31. December 2004.") (3 (month-number 2) 0))
((number period number period year period) ("15.11.2004.") (5 2 0))
((number period number period year) ("31.12.1991") (4 2 0)) 
((number pm) ("3 pm") ((year-current) (month-current) (day-current) (pm-to-24 0) (zero) (zero)))
((number punctuation monthname punctuation number) ("3/Apr/09") ((year2 4) (month-number 2) 0)) 
((number punctuation monthname punctuation year number pm ) ("18/Apr/2004 3 pm") (4 (month-number 2) 0 (pm-to-24 5) (zero) (zero))) 
((number punctuation number punctuation number number colon number colon number pm) ("3/4/05 8:13:25 pm") ((year2 4) 0 2 (pm-to-24 5) 7 9)) 
((number punctuation number punctuation number number colon number colon number) ("3/4/05 8:13:25") ((year2 4) 0 2 5 7 9)) 
((number punctuation number punctuation number number colon number) ("20/8/09 10:15") (4 2 0 5 7))
((number punctuation number punctuation number) ("04/04/04") ((year2 0) 2 4)) 
((number punctuation number punctuation number) ("04/04/04") ((year2 4) 0 2)) 
((number punctuation number punctuation number) ("04/04/04") ((year2 4) 2 0))
((number punctuation number punctuation year number colon number colon number alphabetic) ("11/30/2008 12:00:00 AM") (4 0 2 5 7 9 (zone 10)))
((number punctuation number punctuation year number colon number colon number pm)  ("2/27/2009 12:12:22 PM")  (4 0 2 (pm-to-24 5) 7 9))
((number punctuation number punctuation year number colon number pm) ("3/9/2008 4:05 PM") (4 0 2 (pm-to-24 5) 7 (zero))) ; ambiguous US
((number punctuation number punctuation year number colon number pm) ("3/9/2008 4:05 PM") (4 2 0 (pm-to-24 5) 7 (zero) )) ; ambiguous UK
((number punctuation number punctuation year) ("1/2/2001") (4 0 2)) 
((number punctuation number punctuation year) ("1/2/2001") (4 2 0)) 
((number punctuation number) ("9/11") ((year-current) 0 2)) ; ambiguous US input
((number punctuation number) ("9/11") ((year-current) 2 0)) ; ambiguous UK equivalent
((number) ("3") ((year-current) (month-current) (day-current) 0 (zero) (zero)))
((numbermisc alphabetic) ("2359Z") ((year-current) (month-current) (day-current) (hm-to-h 0) (hm-to-m 0) (second-current) (zone))) 
((numbermisc zulu) ("2359Z") ((year-current) (month-current) (day-current) (hm-to-h 0) (hm-to-m 0) (zero) (zone)))
((year hyphen number hyphen number iso-time number colon number colon number hyphen number colon number) ("1999-12-31T21:59:59-8:00") (0 2 4 6 8 10 (zone 11 12 13 14))) 
((year hyphen number hyphen number iso-time number colon number colon number period numbermisc hyphen number colon number) ("2009-02-27T12:12:22.1020000-08:00") (0 2 4 6 8 (to-decimal 10 11 12) (zone 13 14 15 16)))
((year hyphen number hyphen number iso-time number colon number colon number zulu) ("1994-11-05T13:15:30Z") (0 2 4 6 8 10 (zone)))
((year hyphen number hyphen number iso-time number colon number colon number) ("2001-04-10T15:51:24")  (0 2 4 6 8 10))
((year hyphen number hyphen number number colon number colon number hyphen number colon number) ("1999-12-31 21:59:59 -8:00") (0 2 4 5 7 9 (zone 10 11 12 13))) 
((year hyphen number hyphen number number colon number colon number zulu) ("2001-04-10 15:51:24Z")  (0 2 4 5 7 9 (zone)))
((year hyphen number hyphen number number colon number colon number) ("1984-10-24 15:32:25") (0 2 4 5 7 9))
((year hyphen number hyphen number number colon number) ("2007-02-08 20:12") (0 2 4 5 7))
((year hyphen number hyphen number) ("1991-12-31") (0 2 4)) 
((year hyphen number) ("1995-02") (0 2 (one))) 
((year number number hyphen number number number) ("20061207-064445") (0 1 2 4 5 6)) 
((year number number number number number) ("20061207064445") (0 1 2 3 4 5)) 
((year number number) ("19950204") (0 1 2)) 
((year period number period number period number period number period number) ("2002.01.29.08.36.33") (0 2 4 6 8 10))
))

(dolist (locale '("en_EN" "fr_FR" "de_DE" "it_IT" "ru_RU" "es_ES"))
      (set 'month-res-list '("") 'weekday-res-list '(""))
      (set-locale locale)
      (for (m 1 12) (push (lower-case (date (date-value 2009 m 1) 0 "%B")) month-res-list -1))
      (for (d 0 6)  (push (lower-case (date (date-value 2009 6 d) 0 "%A")) weekday-res-list -1))
      (push month-res-list locale-month-names -1)
      (push weekday-res-list locale-weekday-names -1))
(set-locale {}) ; reset to default

(define (month-name? str)
    (if (month-number str) true))

(define (weekday-name? str)
    (if (weekday-number str) true))
    
; functions called by rules
; some are not really useful other than when called by the rule matching code
; some are here because all numbers in rules are replaced by data elements
; so 'zero' has to be a function - we can't use '0' as it's an index :(

(define (zero) 0) ; because numbers get converted to indexes, have to work round
(define (one) 1)  ; you see...
(define (year2 y)
  ; convert 2 digit year to 4 digit year 
  ; a bad convention really
  (cond
     ((>= y 70)
        (+ y 1900))
     (true
        (+ y 2000))))

(define (zone)
; TODO!
; turn the args into a time zone
; (args) could be
; ("-" 8 ":" 0)
; ("Z")
; ("GMT")
; ("+" 1 ":" 0)
; ("-" 8 ":" 0)
; no args means Z or zulu?
0 ; cop out
)

; various ways of saying "use the current year" etc. - the emptiness is passed onto the Time constructor
(define (year-current))
(define (month-current))
(define (day-current))
(define (hour-current))
(define (minute-current))
(define (second-current))

(define (pm-to-24 n) 
    (if (!= n 12) (+ n 12) n))

(define (hm-to-h n)
    ; input: hours and minutes as four char string, eg "23:57"
    ; return just the hours
    (int (slice (string n) 0 2) 0 10))
    
(define (hm-to-m n)
    ; input hours and minutes as four char string
    ; return just the minutes
    (int (slice (string n) 2 2) 0 10))

(define (to-decimal) 
    (float (apply string (args))))

(define (month-number str)
; translate str to a month number, January = 1
    (let ((month-number nil))
    (dolist (locale locale-month-names (integer? month-number))
      (set 'month-number (find (lower-case str) locale (fn (x y) (regex (string "^" x) y 1)))))
    month-number))

(define (weekday-number str)
; translate str to a weekday number
    (let ((weekday-number nil))
    (dolist (locale locale-weekday-names (integer? weekday-number))
      (set 'weekday-number (find (lower-case str) locale (fn (x y) (regex (string "^" x) y 1)))))
    weekday-number))
    
(define (number-suffix? str)
    (find (lower-case str) '("st" "rd" "th" "nd")))
  
(define (get-next-char)
 (let ((nch ""))
   (if (< *cursor* *source-length*)
       (begin
          (set 'nch (source-string *cursor*))
          (inc *cursor* (utf8len nch)))
       (set 'nch nil))
   nch))

(define (peek-char)
   (let ((pch ""))
       (if (< *cursor* *source-length*)
           (set 'pch (source-string *cursor*))
           (set 'pch nil))))

(define (char-alphabetic? c)
   (find c {'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz}))

(define (char-numeric? c)
   (find c {0123456789}))

(define (char-whitespace? c)
   (find c { \t\n}))
   
(define (char-punctuation? c)
   (find c {.,+-/:"\|<>}))
   
(define (read-whitespace c)
  (let ((res c) (ch ""))
     (while (char-whitespace? (set 'ch (peek-char)))
      (push (get-next-char) res -1))))

(define (read-numeric c)
  (let ((res c) (ch ""))
     (while (char-numeric? (set 'ch (peek-char)))
        (push (get-next-char) res -1))
     (cond 
       ; order important
       ((and (>= (int res -1 10)   1970)
             (<= (int res 9999 10) 2038))
           ; it's a year
           (push (list 'year (int res 1970 10)) D -1))
       ((<= (length res) 2)
           (push (list 'number (int res 1 10)) D -1))
       ((= (length res) 6)
           (read-numeric (slice res 0 2 ))
           (read-numeric (slice res 2 2 ))
           (read-numeric (slice res 4 2 )))
       ((= (length res) 8)  ;; iso yyyy mm dd ?
           (read-numeric (slice res 0 4 ))
           (read-numeric (slice res 4 2 ))
           (read-numeric (slice res 6 2 )))
        ((= (length res) 14)  ;iso 14 digit ?
           (read-numeric (slice res 0 4 ))
           (read-numeric (slice res 4 2 ))
           (read-numeric (slice res 6 2 ))
           (read-numeric (slice res 8 2 ))
           (read-numeric (slice res 10 2))
           (read-numeric (slice res 12 2)))
       (true
           (push (list (sym (string "numbermisc")) (float res)) D -1)))))

(define (read-alphabetic c)
  (let ((res c) (ch ""))
     (while (char-alphabetic? (set 'ch (peek-char)))
      (push (get-next-char) res -1))
    (cond 
      ; order is important here
      ; at least two chars for matching day and month names
      ; months take priority over daynames
      ((and (month-name? res) (>= (length res) 2))
           (push (list 'monthname res) D -1))
      ((and (weekday-name? res) (>= (length res) 2))
           (push (list 'dayname res) D -1))
      ((number-suffix? res)
           (push (list 'number-suffix res) D -1))
      ((= res "T")
           (push (list 'iso-time res) D -1))
      ((= res "Z")
           (push (list 'zulu res) D -1))
      ((ends-with (lower-case res) "clock")
            (push (list 'oclock res) D -1))
      ((find "pm|p\.m\.|pm\.|p\.m" res 1) ; needs work, this one
            (push (list 'pm res) D -1))
      (true
           (push (list 'alphabetic res) D -1)))))

(define (read-punctuation c)
  (let ((res c) (ch ""))
     (while (char-punctuation? (set 'ch (peek-char)))
      (push (get-next-char) res -1))
    (cond
       ((= (first res) ":")
           (push (list 'colon res) D -1))
       ; period + - are ambiguous - both alpha and numeric...?
       ((= (first res) ".")
           (push (list 'period res) D -1))
       ((= (first res) "-")
           (push (list 'hyphen res) D -1))
       ((= (first res) "+")
           (push (list 'plus res) D -1))
       (true
           (push (list 'punctuation res) D -1)))))

(define (read-token)
 (let ((first-char (get-next-char)))
    (if first-char
      (cond 
            ((char-whitespace? first-char)   
               (read-whitespace first-char))
            ((char-numeric? first-char)
               (read-numeric first-char))
            ((char-alphabetic? first-char)
               (read-alphabetic first-char))
            ((char-punctuation? first-char)
               (read-punctuation first-char))
            (true true)))))

(define (read-source source-string)
 (set '*cursor* 0 '*source-length* (utf8len source-string))
 (while (< *cursor* *source-length*)
      (read-token)))

(define (tokenize str)
  (set 'D '())
  (read-source str)
  (map first D))

(define (check-ambiguity lst)
; return list lst checked for ambiguities
; list is '(year month day hour minute second)
  (let ((res lst))  
    (if (and (> (res 1) 12) (< (res 2) 13))
        ; month day need swapping 
        (swap (res 1) (res 2)))
    (if (> (res 1) 12)
        (setf (res 1) (- (res 1) 12)))
    (if (> (res 0) 2028)
        (setf (res 0) 2000))
    res))

(define (parse-time str)
; return a list of possible interpretations of the date/time in str
    (let ((results '())
          (tokenized-input (tokenize str)))
    ; is the pattern present in the rule database?
    (set 'rule-refs (ref-all tokenized-input rules-db match))
    (dolist (rule-ref rule-refs)
        ; with each rule, replace numbers in rule by the elements of the input
        (set 'res '() 'rule (last (rules-db (chop rule-ref))))
        (when rule 
            (set 'data-elements (map last D))
            (dolist (i rule)
              (cond 
                ((number? i) ; eg 2 - find element 2's value
                   (push (nth i data-elements) res -1))    
                ((and (list? i) (= (length i) 1))
                   ; eg (midnight)
                   (push (eval i) res -1))
                ((list? i) ; eg (month-number 2) - change that 2 to element 2's value, eg "April"
                   (dolist (e i)
                      (cond 
                        ((number? e)
                            (setf (i $idx) (nth e data-elements)))))
                  (push (eval i) res -1)))
                ) ; dolist
             (set 'res (check-ambiguity res)))
         (push res results))
    (unique results)))

(define (ParseTime:ParseTime str)
; returns list of date strings that are possible interpretations of str
; for example: (parse-time "12/3/2009")
; => ((2009 3 12) (2009 12 3)) two possible interpretations
  (parse-time str))

(context MAIN)

; eof