;; @module nldb
;; @description a simple pure-newLISP database
;; @author cormullion (cormullion at mac.com), building on work by (kinghajj at gmail.com)
;; @location http://unbalanced-parentheses.nfshost.com/downloads/nldb.lsp
;; @version 2010-01-14 14:57:28
;;<h4>About the nldb module</h4>
;;<p>This is a simple database module, for those occasions when it's not possible
;; to install or use something better, such as sqlite.
;; Data is stored in a list of lists. The first row is a list of column names, stored as symbols.
;; List elements can be numbers, strings, or lists.
;; </p>

(context 'nldb)

(set 'tables '()) ; this holds the list of table names

(define (create-table table-name (column-list 'column-1))
;; @syntax (create-table <table> <column-list>)
;; @param <table> evaluates to a table name
;; @param <column-list> list of column name symbols
;; Create a new table with columns in <column-list>.
;; @example
;; (set 'column-names '(No AtomicWeight Name Symbol MP BP Density EarthCrust DiscoveryYear Group IonizationEnergy))
;; (create-table 'elements (map sym column-names)) 
  (push table-name tables -1)
  (set table-name (list column-list)))

(define (list-columns table-name)
;; @syntax (list-columns table-name)
;; @param <table-name> unquoted name of table
;; List columns in table. Table isn't quoted.
;; @example
;; (list-columns elements)
 (first table-name))

(define (count-columns table-name)
;; @syntax (count-columns table-name)
;; @param <table-name> unquoted name of table
;; @return Return the number of columns.
;; Count the number of columns in table.
;; @example
;; (count-columns elements)
    (if table-name (length (list-columns table-name))))

(define (add-columns table-name new-column-list (value nil))
;; @syntax (add-columns table-name new-column-list value)
;; @param <table-name> Evaluates to name of table
;; @param <new-column-list> List of columns to be added.
;; @param <value> Initial value for each column,
;; Add new columns to every row in table.
;; You can provide a default value for every field.
;; @example
;; (add-columns 'elements '(Price Postage) 0)
;; (add-columns 'elements '(Uses) '(chemistry physics))
    (letex ((table table-name))
        (let ((columns (first table)))
             ; mustn't duplicate columns
             (if (empty? (intersect new-column-list columns))
                 (setf table
                     (cons (append columns new-column-list)
                         (map  (fn (r) (append r (dup value (length new-column-list)))) 
                               (rest table))))))))

(define (truncate data-list size)
  (0 size (append data-list (dup nil size))))

(define (add-row table-name data)
;; @syntax (add-row table-name data)
;; Add a row to the table. Supply all the fields.
;; @example
;; (add-row 'elements1 '(1 1.0079 "Hydrogen" "H"))
    (if (list? data) 
        (letex ((table table-name))
               (push (truncate data (count-columns table)) table -1))))

(define (new-row table-name assoc-list)
;; @syntax (new-row table-name assoc-list)
;; @param <table-name> Evaluates to name of table. 
;; @param <assoc-list> Association list of some existing column names and values.
;; Add another row to the table. Supply some values for some of the columns.
;; Any missing columns are set to nil. You shouldn't leave them like that!
;; @example
;; (new-row 'elements '((Name "Unobtainium") (Symbol "Ub")))
    (letex ((table table-name))
        (let ((columns (first table)))
            (push (map last 
              (map (fn (c) (or (assoc c assoc-list) 
                               (list c nil))) 
                    columns))
                    table -1))))

(define (select-rows the-table (select-fn true) (column-list true) (sort-column nil) (sort-function nil))
;; @syntax (select-rows the-table (select-fn true) (column-list true) (sort-column nil) (sort-function nil))
;; @param <the-table> evaluates to name of table
;; @param <select-fn> an expression that when evaluated returns true or false to select a row
;; @param <column-list> columns to be returned
;; @param <sort-column> the name of the column on which to sort
;; @param <sort-function> a function or expression that sorts the rows once selected
;; Select rows using the supplied function, returning the columns in column-list, sorted by the sort-column/sort-function.
;; Use true to select all rows/columns when specifying columns and sort.
;; @example 
;; (select-rows 'elements)
;; (select-rows 'elements '(= Name "Helium")) ; the selection function looks for Name = "Helium"
;; (select-rows 'elements '(< DiscoveryYear 1600))
;; (select-rows 'elements true true 'Symbol >) ; all columns of all elements sorted by symbol inversely
;; (select-rows 'elements '(and (> EarthCrust 1) (< DiscoveryYear 1900)))
;; (select-rows 'elements true true 'Name <) ; all elements, sorted by name
;; (select-rows 'elements true true 'Name (fn (x y) (< (length x) (length y)))) ; all sorted by length of name
;; (select-rows 'elements '(> DiscoveryYear 1900) '(Name Symbol)) ; some columns of recent elements, unsorted
   (letex ((table the-table))
     (let ((columns (first table))
           (selection '())
           (cell nil)
           (selection-function nil))
     (dolist (row (rest table))
        ; 'evaluate' the selection function for this row
        ; by replacing any keys in select-fn with actual values from data
        (set 'selection-function select-fn)
        (if (list? selection-function)
            (dolist (s (flat selection-function))
                (when (find s columns) 
                      (set 'data (row (find s columns))) 
                      (if (list? data) (set 'data (eval-string (append "''" (string data)))))
                      (set-ref-all s selection-function data))))
        ; now, does the selection function select this record?
        (if (eval selection-function)
            (push row selection -1)))
        
      ; do we have to sort selection?
      (when sort-function
          (sort selection (fn (x y) (apply sort-function (list (x (find sort-column columns)) (y (find sort-column columns)))))))
      
      ; do we have to filter columns of selection?
      ; if column-list is true, show all columns   
      (if (list? column-list)
          (set 'selection (map (fn (r) (select r (map (fn (c) (find c columns)) column-list))) selection)))
     selection)))
 
(define (modify-row table the-row column-name modify-fn)
;; modify-fn must be a function that evaluates to provide a new value, so, to set 
;; a numeric value you might use "(int 42)"
   (let ((row the-row)
         (cell nil)
         (columns (first table)))
        ; first, 'evaluate' selection function for this row
        ; by replacing any quoted keys in select-fn with actual values from rows
            
        (if (list? modify-fn)
            (dolist (s (flat modify-fn))
                (when (find s columns) 
                      (set 'data (row (find s columns))) 
                      (if (list? data) (set 'data (eval-string (append "''" (string data)))))
                      (set-ref-all s modify-fn data))))
            
        ; does the selection function select this record?
        (setf (nth (find column-name columns) row) (eval modify-fn))
        ; return the modified row
        row))

(define (delete-rows table-name select-fn)
;; @syntax (delete-rows table-name select-fn)
;; @return number of rows selected (and presumably deleted)
;; Delete rows found by select-fn.
;; Obviously the database has to be saved for the deletion to be permanent.
;; @example
;; (delete-rows 'elements '(> DiscoveryYear 1945))
    (let ((table table-name)
          (selection '()))
       (set 'selection (select-rows table-name select-fn))
       (map (fn (row)  (replace row (eval table))) selection)
      ; return length of selection
    (length selection)))

(define (change-rows table-name select-fn column-name modify-fn)
;; @syntax (change-rows table-name select-fn column-name modify-fn)
;; @param <table-name> evaluates to name of table
;; @param <select-fn> an expression that when evaluated returns true or false to select a row
;; @param <column-name> column to be modified
;; @param <modify-fn> a function or expression that specifies a new value
;; Change value of a column in matching rows of table-name found by select-fn, using modify-fn.
;; The modify-fn can access other columns when calculating a new value.
;; @example
;; (change-rows 'elements '(= Name "Helium") 'Symbol '(reverse Name))
;; ; changes symbol of all elements matching "Helium" to reverse of Name field
;; (change-rows 'elements '(= Name "Lead") 'BP '(add BP MP)) 
;; ; changes Boiling Point of all elements matching Lead to sum of BP and MP fields
(let ((table table-name)
       (selection '()))       
       (set 'selection (select-rows table-name select-fn))
       (map (fn (row) 
           ; replace old rows with new: find the original rows
           ; then strip the assoc keys that we used to find the rows
           (replace row 
                    (eval table-name)
                    (modify-row (eval table-name) row column-name modify-fn)))
           selection)
      ; return number of records selected
      (length selection)))

(define (find-text table-name str (regex-option 0))
;; @syntax (find-text <table-name> <str> [<regex-option>])
;; Find the string in the text fields of rows in table.
;; The default is regular expression option 0 - ie case-sensitive!
;; @example
;; (find-text 'elements "e")
	(letex ((table table-name))
		(let ((results '()))
			(map (fn (row)
				(if (find str row regex-option)
					(push row results -1)))
	            table)
      results)))

(define (sort-table table-name column function)
;; @syntax (sort-table table-name column function)
;; Sort table on column using supplied function.
;; @example
;; (sort-table 'elements 'Name (fn (x y) (< (length x) (length y))))
;; (sort-table 'elements 'BP >)
  (letex
    ((table table-name))
    (letn ((columns (first table))
           (col     (find column columns)))
    ; set the table to a new list
    ; keep the first row (column names) intact
    ; allow the caller to specify sort column and function
       (setf table 
          (cons columns 
            (sort (rest table) (fn (x y) (apply function (list (x col) (y col))))))))))

(define (save-db filename)
;; @syntax (save-db filename)
;; Save the database tables in the named file, and a list of tables too. 
(let ((save-list tables))
    (push 'tables save-list)
	    (apply save (cons filename save-list)) 
    	;;(println "saved the database in " filename)
	)
)

(define (load-db filename)
;; @syntax (load-db filename)
;; Replace the curent data tables with ones from <filename>
   (load filename))

(define (show)
;; @syntax (show)
;; Show the database
  (println 
    (dup "_" 60) "\n"
    "Contents of database "  (context)  "\n"
    " " (length tables) " table" (if (> (length tables) 1) "s" "") ": " tables "\n")
  (dolist (table tables)
     (println " Table:  " table)
     (println " Columns " (first (eval table)))
     (println " Rows:   " (length (rest (eval table))))
     (dolist (row (rest (eval table)))
        (println row))
     (println))
  (println (dup "_" 60))
  (sym (date)))

; eof
