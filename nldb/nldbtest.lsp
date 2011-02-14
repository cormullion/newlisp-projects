#!/usr/bin/env newlisp

(change-dir {/Users/me/nldb/})
(load {nldb.lsp})

(context nldb)

(load-db {elements.nldb})

[text]
; If we hadn't already defined a database, we'd do this:

; Create a table, supplying its name and the column names in a list:

(set 'column-names '(No AtomicWeight Name Symbol MP BP Density EarthCrust DiscoveryYear Group IonizationEnergy))

(println "create table \n"
    (create-table 'elements1 (map sym column-names)) ; just a demo, we've already created the table
)

; Now you can add rows. Values should be supplied in the correct order:

(println "add a row \n"
(add-row 'elements1 '(1 1.0079 "Hydrogen" "H"))
)

(println "add a new row with incomplete data\n"
(new-row 'elements1 '((Name "Unobtainium") (Symbol "Ub")))
)
(println
(change-rows 'elements1 '(= 'Name "Unobtainium") 'No '(int 200))
(select-rows 'elements1 '(find "Unobtainium" 'Name))
)

; To see a list of tables:

(println "tables:\n"
tables
)

; To see the columns in a table:

(println "columns in elements are: \n"
(list-columns elements)
)

; To see the whole database, use "show":
(show)

; Use the context prefix if required:
; (context MAIN)
; (nldb:create-table 'nldb:table2  '(nldb:element-name nldb:discoverer))
; (nldb:show)
; (context nldb)

; reset 

(load-db {elements.nldb})

[/text]

(println "common elements discovered before 1900 \n"
  (select-rows 'elements '(and (> EarthCrust 1) (< DiscoveryYear 1900)))
)

(println " elements discovered before 1600 \n"
  (select-rows 'elements '(< DiscoveryYear 1600)))

(println "all elements \n"
  (select-rows 'elements))

;(println "delete less common elements \n"
;  (delete-rows 'elements '(< EarthCrust 2))
;)

(println "change row: reverse symbol \n"     ; table selection-fn field new-value
  (change-rows 'elements '(= Name "Helium") 'Symbol '(reverse Name)))

(println " Helium is now \n"
  (select-rows 'elements '(= Name "Helium")))

(println " Lead is \n"
  (select-rows 'elements '(= Name "Lead")))

(println "change BP of Lead to BP + MP\n"
  (change-rows 'elements 
    '(= Name "Lead") 
    'BP                  ; field to change
    '(add BP MP)))       ; new value - can access any other column

(println " Lead is now \n"
  (select-rows 'elements '(= Name "Lead")))

(println " Hydrogen is \n"
  (select-rows 'elements '(= Name "Hydrogen")))

(println " increase boiling point of hydrogen by 1\n"
    (change-rows 'elements 
      '(= Name "Hydrogen")
      'BP 
      '(inc BP)))             ; user-supplied function

(println " and Hydrogen is now \n"
  (select-rows 'elements '(= Name "Hydrogen")))


(println "all elements sorted by name inversely\n" 
  ; table selection column-list sort-column sort-function
  (select-rows 'elements true true 'Name '<))

(println "all elements sorted by symbol inversely\n"
  (select-rows 'elements true true 'Symbol '>))

(println "all elements sorted by user-def function length of name \n"
  (select-rows 'elements true true 'Name (fn (x y) (< (length x) (length y))))) 

(println "recent elements sorted by user-def function length of name\n"
  (select-rows 'elements '(> DiscoveryYear 1900) true 'Name (fn (x y) (< (length x) (length y))))
)

(println "Some columns from recent elements\n"
  (select-rows 'elements '(> DiscoveryYear 1900) '(Name Symbol)))

; To select all rows in a table, use select-rows:

(println "select all rows \n" 
    (select-rows 'elements))

; You can specify a  function to select one or more rows. Specify a table and a selection function involving the column names and values:

(println "elements discovered after 1900 \n"
    (select-rows 'elements '(>= DiscoveryYear 1900)))

(println "common elements (more than 5% of earth's crust) \n"
    (select-rows 'elements '(>= EarthCrust 5)))

(println "show details of Hydrogen\n"
    (select-rows 'elements '(find "Hydro" Name)))

(println 
     (select-rows 'elements '(ends-with Name "gen") '(Symbol)))
; (("O") ("H") ("N"))

; the selection can be filtered to show only some of the columns

(println "show only certain columns of selection: Name and Earth's crust percentage \n"
    (select-rows 'elements true '(Name EarthCrust))) ; true means all columns

; To combine two or more conditions:
(println "common elements discovered before 1900 \n"
(select-rows 'elements 
    '(and (> EarthCrust 5) (< DiscoveryYear 1900))
    '(Name)))
;-> (("Oxygen") ("Aluminum") ("Silicon") ("Iron"))

; or do a set intersection

(println "set intersection \n\n"
(intersect (select-rows 'elements '(> EarthCrust 5))
           (select-rows 'elements '(< DiscoveryYear 1900))))

(println "Result of (select-rows 'elements '(starts-with Name \"C\" 0))\n\n"
    (select-rows 'elements '(starts-with Name "C" 0)))
; returns list

(println "list of names starting with A or B \n\n"
    (flat (select-rows 'elements '(starts-with Name "A|B" 0) '(Name))))

;("Beryllium" "Boron" "Aluminum" "Argon" "Arsenic" "Bromine" "Antimony" "Barium" "Bismuth"  "Astatine" "Actinium" "Americium" "Berkelium" "Bohrium")

(println "elements starting with A and BP > 2000))\n\n"
    (select-rows 'elements '(and (starts-with Name "A" 0) (> BP 2000))))

; the function is applied to every row in the table:
(println "selecting rows using user-supplied function ium in name\n"
    (select-rows 'elements '(find "ium" Name) '(Name Symbol)))
; prints out all element names


; To delete rows, use the same technique:
;(println "delete man-made elements (discovered after 1945)\n"
; (delete-rows 'elements '(> 'DiscoveryYear 1945))
;)

; finding text
(println "looking for 'Hy'\n" 
   (find-text 'elements "Hy"))

; Making changes
; To change one or more rows of a table, use change-rows with a 
; selection function to find the rows, and supply a column name and a 
; modification function.

(println " ending -gen \n\n" (select-rows 'elements '(ends-with Name "gen")))

(println " change boiling point of -gen elements by 1\n"
    (change-rows 'elements 
    '(ends-with Name "gen")  
    'BP            
    '(inc BP)))

(println (select-rows 'elements '(ends-with Name "gen")))

(println " increase hydrogen discovery date by 20 years\n"
(change-rows 
  'elements 
  '(= Name "Hydrogen")
  'DiscoveryYear 
  '(inc DiscoveryYear 20)))

(println (select-rows 'elements '(ends-with Name "drogen")))

(println " change discovery date of 'ium' elements to 2008\n"
(change-rows 
  'elements 
  '(ends-with Name "ium")
  'DiscoveryYear 
   '(inc DiscoveryYear)))

; Sorting tables

; To sort a table, specify the table, the column, and a function:

(println "elements sorted by Boiling Point\n"
  (sort-table 'elements 'BP >))

; this returns the original table. It has been sorted by boiling point.

; You can also supply your own sorting function:

(println "sorted by length of name, shortest first\n"
  (sort-table 'elements 'Name (fn (x y) (< (length x) (length y)))))

(println "sort by Name, show Name and Symbol, longest name first: "
  (select-rows 'elements true '(Name Symbol) 'Name (fn (x y) (> (length x) (length y)))))

; adding columns; more than one, but one default value for both

(println "add two columns \n"
  (add-columns 'elements '(Price Postage) 0))

; To save the database:
;(save-db "/Users/me/test.nldb")

;That saves just the tables, not the entire nldb context with functions. We can develop the two sides (code and data) separately, even though they both live together.

; To load a database, first load the database code:
; (load ""/Users/me/nldb.nl")
; then load the database:
; (load "/Users/me/test.nldb")

(exit)

