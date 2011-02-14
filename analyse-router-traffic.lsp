#!/usr/bin/env newlisp
;; @module analyse-router-traffic
;; @author cormullion@mac.com
;; @version 0.1 2011-02-14 12:18:06
;;
;; This script lets you access a DD-WRT software-based
;; router such as a Linksys using Telnet and get the traffic data from
;; NVRAM, suitable for graphing, analysis, etc.
;; I consider this a hack. :)

(define (get-traffic-data site (user-name "admin") (password "password"))
 ; eg (get-traffic-data "DD-WRT")
 (local (str-buffer nvram-contents telnetSite socket result)
 (set 'str-buffer {} 'nvram-contents {} 'telnetSite (string site))
 (set 'socket (net-connect telnetSite 23))
 (when socket
	 (net-receive socket str-buffer 512 "DD-WRT login:")
	 (net-send socket (string user-name "\r\n"))
	 (net-receive socket str-buffer 2000 "Password:")
	 (net-send socket (string password "\r\n"))       
	 (net-receive socket str-buffer 2000 "root@DD-WRT")
	 (net-send socket (string {nvram show | grep "traff-"} "\r\n"))
	 (net-receive socket nvram-contents 3000 "root@DD-WRT:~# ")
	 (net-send socket (string {exit} "\n"))
	 (net-close socket)   
     ; we now should have some data
     ; tidy up the output of telnet to leave just the traff-01 to traff-12 lines
	 (map (fn (l) (replace {(.*?)(traff-\d\d.*)} l (push $2 result -1) 0))
		(parse nvram-contents "\n"))
	 result)))

(define (add-data line)
    ; line is a line from the traffdata file
    ; it represents a month's worth of data from the DD-WRT traffdata
    (local (l m-data month-number year-number day-number incoming outgoing month-table)
    (set 'l (parse line "[= ]" 0))
    (set 'm-data  (pop l))
    (set 'm-data  (parse m-data "-"))
    (set 'month-number (int (m-data 1)))
    (set 'year-number  (int (m-data 2)))
    (set 'day-number 1)
    (dolist (d (chop l)) ; drop last item, which are totals for month
       ; d contains a pair of numbers eg "0:23"
       (set 'incoming (first (parse d ":")))
       (set 'outgoing (last (parse d ":")))
       (push 
          (list (format {%d %02d %02d} 
             (list year-number month-number day-number)) 
             (map int (list incoming outgoing))) 
          month-table)
       (inc day-number))
    month-table))

(define (totalize the-data (day (date (date-value) 0 {%Y %m %d})) (number-of-days 30))
  ; get the last n days of data
  (println "Usage for the last " number-of-days " days, as at " day)
  (set 'start-ref (ref day the-data))
  (set 'data-slice (slice the-data (first start-ref) number-of-days))
  ; (("2011 02 10" (456 1356)) ("2011 02 09" (852 1951)) ("2011 02 08" (703 2361)) ("2011 02 07" 
  ;  (1251 2223))  ("2011 02 06" (776 776)) 
  (list 
    (apply + (map (fn (f)  (first (last f))) data-slice))
    (apply + (map (fn (f)  (last  (last f))) data-slice))))  

; get lines of data from router
(set 'raw-data (get-traffic-data "DD-WRT" "root" "13142"))

; should check before continuing - have we any data? 

(set 'data-table '())
(map (fn (a) (extend data-table (add-data a))) raw-data)

; sort so that most recent entries are first

(println (sort data-table >))

; I want to know what the most recent 30 days worth of usage is, starting from today
(println (totalize data-table (date (date-value) 0 {%Y %m %d}) 30))

(exit)