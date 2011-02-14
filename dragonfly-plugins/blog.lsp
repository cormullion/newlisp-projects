; File: plugins-active/blog.lsp


; this is the main routing file for the blog 
; was used by unbalanced-parentheses.nfshost.com
; incoming routing requests are matched against the database (uses nldb not sqlite) and
; sets actions accordingly
; I hope Greg or Marc don't see this code - I bet I'm doing it all wrong :)

(new Route 'Route.Blog)

(context Route.Blog)

(define (matches?)
    (set 'request (clean empty? (parse QUERY_STRING "?")))
    ; load my nldb database of blog posts
    (Dragonfly:load-database "blog.nldb")
    (cond 
      ((empty? request)
             ; nldb:select-rows table selection column-list sort-column sort-function
             ; select all rows, all columns, most recent row first
             (set 'query (nldb:select-rows 'nldb:stories true true 'nldb:story-date >))
             (set 'action 'latest))
              
      ((= "welcome" (first request))
             (set 'query (nldb:select-rows 'nldb:stories true true 'nldb:story-date >))
             (set 'action 'latest))

      ((= "random" (first request))
             (seed (date-value))
             (set 'action 'random-post)
             ; choose row at random
             (set 'query (list (apply amb (nldb:select-rows 'nldb:stories true)))))
      
      ((= "search" (first request))
             (set 'search-request ($POST "inputstring"))
             (set 'action 'search-action))
             
      ((= "twitter-search" (first request))
             (set 'twitter-search-request ($POST "inputstring"))
             (set 'action 'twitter-search-action))
             
      ((= "lambdalator" (first request))
             (set 'action 'lambdalator-action))
      
      ((= "aboutthissite" (first request))
             (set 'action 'a-story)
             ; find the 'about' post
             (set 'query (nldb:select-rows 'nldb:stories '(= nldb:story-id {aboutthissite})))) 
             
      ((= "links" (first request))
             (set 'action 'links))
                   
      ((set 'query (nldb:select-rows 'nldb:stories '(= nldb:story-id (first request))))
             (set 'action 'a-story))

      ((starts-with (first request) "downloads/")
             (set 'f (first request))
             (set 'action 'pass-through))
        
      ; all other possibilities, pass on to next 
      (true  nil)))

(define (run)
 (cond 
    ((= action 'links) 
        (DF:display-view "links"))

    ((= action 'search-action) 
        (DF:display-view "search"))

    ((= action 'lambdalator-action) 
        ;(DF:display-view "lambdalator")
        (DF:include "../lambdalator/index.cgi")
         )

    ((= action 'pass-through)
        (SET_DF_SELF f)
        (DF:include f))

    ((= action 'twitter-search-action)
        (DF:display-view "twitter"))

    ((= action 'latest)
        ; choose the most recent 3 rows
        (set 'stories (0 3 query))
        (DF:display-view "recent"))

    (true
        (set 'story (first query))
        (DF:display-view "welcome"))))

(define (present-story story)
; given a post in story, output it as HTML
   (print {<h1>} (story 2) {</h1>})
   (println {<small><a class="info" href="#">} 
       (story 0) 
       {<span>} 
       (date (parse-date (story 0) {%Y-%m-%dT%H:%M:%SZ}) 0 {%A %d %B, %X}) 
       {</span></a></small>})
   (print (render (last story))))

(define (render t)
    (replace {-{3}(.*?)-{3}}
        t (if (catch
                (eval-string $1) 'result)
            (string result)
            (string {---} t {---})) 
        4))

(define (pie-chart 
    (chs "400x200") ; size 
    (chd "t:60,40") ; date percentages
    (cht "p3")      ; type
    (chl "Hello|World") ; 
    (chalt "Sample Google Chart") ;
    )
 (format {<img src="http://chart.apis.google.com/chart?chs=%s&amp;chd=%s&amp;cht=%s&amp;chl=%s" alt="%s" />} chs chd cht chl chalt))

; to include pie charts in blog pages, use this:
; <p>-- -(Route.Blog:pie-chart "600x200" "t:19.46,71.74,2.58,6.19" "p3" "comments 165384 |stories 609457|newLISP code 21981|Javascript 52618") -- -</p> all on one line ?!?!?!??!

; add the route to the list of routes
(push (Route.Blog) DF:dragonfly-routes)
; 
