#!/usr/bin/env newlisp
;; @module analyse-router-traffic
;; @author cormullion@mac.com
;; @version 0.3 2011-09-09 09:54:35
;;
;; This script lets you access a DD-WRT software-based
;; router such as a Linksys and get the traffic data from
;; NVRAM.
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
    (set 'month-number (int (m-data 1) 0 10)) ; watch out for octal
    (set 'year-number  (int (m-data 2) 0 10))
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

(define (totalize the-data (number-of-days 30) (start-at 0))
  ; add the last number-of-days days of data  
  ; most recent first
  
  (set 'data-slice (slice the-data start-at number-of-days))
        (list (apply + (map (fn (f)  (first (last f))) data-slice))
              (apply + (map (fn (f)  (last  (last f))) data-slice))))

; get data from router
(set 'raw-data (get-traffic-data "DD-WRT" "root" "13142"))
;-> ("traff-03-2011=859:778 555:1526 1974:1273 1901:366 1230:546 831:303 499:383 394:278 1421:430 492:434 1531:439 1275:461 2407:380

; compile data into table
(map (fn (a) (extend data-table (add-data a))) raw-data) ; don't need to create data-table first :)

; data-table is
;   (("2011 02 10" (456 1356)) 
;    ("2011 02 09" (852 1951)) 
;    ("2011 02 08" (703 2361)) etc

; I want the total for the previous 30 days, up to i days ago, starting from today
; there should be at least 30 days' worth of data for this

(sort data-table > ) ; most recent first
; start at today's date and select all previous (there may be blank entries for the rest of the month)
(set 'data-table (slice data-table (first (ref (date (date-value) 0 "%Y %m %d") data-table))))
(set 'number-of-days 30)
(for (days-ago 0 30)
    (print "" days-ago " days ago:\t\t")
    (println (format {%d %d} (totalize data-table number-of-days days-ago))))

; output suitable for spreadsheet
(map (fn (f) (println (date (parse-date (first f) {%Y %m %d})) "\t"  (first (last f)))) (sort data-table <))

(exit)