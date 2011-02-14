#!/usr/bin/env newlisp

;; @module parse-time
;; @description test code for parsetime, a module that parses date/time strings 
;; @version 0.1
;; @author cormullion

(load {parsetime.lsp} {timeutilities.lsp})

(define (parse-time str)
    (let ((res "")
          (answer (ParseTime str)))
     (println " answer is " answer)
     (cond
       ((> (length answer) 1)
          (dolist (d answer) (push (string (:show (apply Time d)) {  }) res -1)))
       ((= (length answer) 1)
          (set 'res (:show (apply Time (first answer)))))
       (true
           (set 'res "unfamiliar format")))
     res))

; usage:     
(println (parse-time "12/3/2009"))

(set 'test-data (map (fn (f) (first (first (rest f)))) ParseTime:rules-db))
(set 'more-test-data '(
"3:52 p.m."
"Wed, August 13, 2009 3:15 pm."
"Wednesday, August 13, 2009 3:15 p.m."
"Wednesday, August 13, 2009 3:16"
"Wednesday, August 13, 2009, 3:17 pm."
"Wednesday, August 13, 2009, 3:18 p.m."
"Wednesday, August 13, 2009, 3:19"
"Wednesday August 13, 2009, 3:20 pm."
"Wednesday August 13, 2009, 3:21 p.m."
"Wednesday August 13, 2009, 3:22"
"Wed, August 13, 2009 3:15:01 pm."
"Wednesday, August 13, 2009 3:15:02 p.m."
"Wednesday, August 13, 2009 3:16:03"
"Wednesday, August 13, 2009, 3:17:04 pm."
"Wednesday, August 13, 2009, 3:18:05 p.m."
"Wednesday, August 13, 2009, 3:19:06"
"Wednesday August 13, 2009, 3:20:07 pm."
"Wednesday August 13, 2009, 3:21 p.m."
"Wednesday August 13, 2009, 3:22:08"
"August 13, 2009, 3:23 pm."
"August 13, 2009, 3:24 p.m."
"August 13, 2009, 3:25"
"August 13, 2009, 3:23:01 pm."
"August 13, 2009, 3:24:02 p.m."
"August 13, 2009, 3:25:03"
"20/8/09 10:15"
"31 st January 2012"
"April 26 23:59"
"23:31"
"12/31/12"
"31/12/12"
"2007-02-08 20:12"
"1978-04-26"
"2003-01-01"
"1984-10-24 15:32:25"
"2003-01-01"
"2001-10-28 17:32:25" 
"2003-01-01 12:00:18"
"11/30/2008 12:00:00 AM"
"12/1/2008 12:00:00 AM"
"13-10-05 10:19:26"
"1994-11-05T08:15:30-05:00"
"1994-11-05T13:15:30Z"
"4/10/2001"
"10/04/2001"
"10.04.2001"
"Tuesday, April 10, 2001"
"3:51:24 PM"
"15:51:24"
"Tuesday, April 10, 2001 3:51 PM"
"mardi 10 avril 2001 15:51"
"Tue, 10 Apr 2001 15:51:24 GMT"
"Tue, 10 Apr 2001 15:51:24 GMT"
"2001-04-10T15:51:24"
"2001-04-10T15:51:24"
"2001-04-10 15:51:24Z"
"2001-04-10 15:51:24Z"
"April 10"
"10 April"
"April, 2001"
"April 2001"
"9/11"
"4:05 PM"
"3 pm"
"3/9/2008"                        
"4:05:07 PM"                      
"Sunday, March 09, 2008"          
"Sunday, March 09, 2008 4:05 PM"  
"Sunday, March 09, 2008 4:05:07 PM"
"3/9/2008 4:05 PM"                
"3/9/2008 4:05:07 PM"             
"March 09"                        
"March, 2008"                     
"Sun, 09 Mar 2008 16:05:07 GMT"   
"2008-03-09T16:05:07"             
"2008-03-09 16:05:07Z"
"5:46:21 PM EST"
" 2/27/2009"
"2. 27. 2009"
"2. 27. 2009."
"Friday, February 27, 2009"
"Friday, February 27, 2009 12:11 PM"
"Friday, February 27, 2009 12:12:22 PM"
"2/27/2009 12:12 PM"
"2/27/2009 12:12:22 PM"
"February 27"
"February 27"
"2009-02-27T12:12:22.1020000-08:00"
"2009-02-27T12:12:22"
"12:12 PM"
"12:12:22 PM"
"2009-02-27 12:12:22Z"
"Friday, February 27, 2009 8:12:22 PM"
"February, 2009"
"February, 2009"
"Thursday, May 4, 2006"
"Donnerstag, den 4 May, 2006"
"Mercoledi, 4 Maggio, 2006"
"Tue, 09 Jan 2002 22:14:02 -0500"
"2002.01.29.08.36.33"
"01/09/02"
"29-Jan-02"
"Saturday, July 23 2005"
"13.Apr.2006"
"Thu, 21 Dec 2000 16:01:07 +0200"
"01 Jan 01"
"Jan 01 01"
"23rd February 2009, 8:15" 
"23rd February 2009, 8:15 am" 
"23rd February, 8:15 am"
"23rd February, 8:15 pm"
"23rd February, 8:15"
"1995-02"
"24 June"
"15.11.2004."
"15. 10. 2004."
"Fri Dec 31 21:59:59 1999"
"31 Dec 21:59:59 1999"
"31 December 2004"
"31. December 2004."
"31 December 04"

"2003-07-30  "
"1972-09-24"
"72-9-24"
"72-09-24"
"24 September 1972"
"24 Sept 72"
"24 Sep 72"
"Sep 24, 1972"
"24-sep-72"
"24sep72"
"24-sep-72 20:02"
"24-sep-72 8:02pm"

"8:10pm on Sunday, 16 Sept 1973"
"8:10 on Sunday, 16 Sept 1973"
))

(dolist (d more-test-data) (push d test-data -1))

(dolist (test-date test-data)
   (set 'possibles-list (ParseTime test-date))
   (if (= (length possibles-list) 0)
        (begin
           (context ParseTime)
           (println (list (ParseTime:tokenize test-date) (list test-date) '()))
           (context MAIN)
           ))
   (dolist (r possibles-list)
       (if (> (length possibles-list) 1) 
            (print  test-date " is ambiguous:\n\t" $idx "\t")
            (print  test-date "\n"))
       (println "\t\t\t\t -> " (:show (apply Time r)))))

(println "dates tested: " (length test-data))
(println "patterns available: " (length ParseTime:rules-db))

; eof