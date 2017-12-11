;; Homework 2
;; Shiyu Luo
(require 2htdp/image)
;; Problem 1
;; Nonnegative Number -> Nonnegative Number
;; Converts dollar amounts into euro amounts
(check-expect (convert 1) 0.89)
(check-expect (convert -1) "Please enter a non-negative number")
(define (convert dollar)
  (if (>= dollar 0)
  (* dollar 0.89)
  "Please enter a non-negative number"))

;; Problem 2
;; A CartesianPoint is (make-ca-point Integer Integer)
;; INTERP: represents a cartesian point with its x and y coordinates
(define-struct ca-point (x-coord y-coord))

;; CartesianPoint, CartesianPoint -> Nonnegative Number
;; calculates the distance between two Cartesian points
(define point1 (make-ca-point 3 0))
(define point2 (make-ca-point 0 4))
(check-expect (distance point1 point2) 5)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (ca-point-x-coord p1) (ca-point-x-coord p2)))
           (sqr (- (ca-point-y-coord p1) (ca-point-y-coord p2))))))

;; Problem 3
;; A NumberGrade falls into one of eight intervals:
;; - [0, 60]
;; - (60, 70]
;; - (70, 75]
;; - (75, 80]
;; - (80, 85]
;; - (85, 90]
;; - (90, 94]
;; - (94, 100]

;; A LetterGrade falls is one of:
;; - "E"
;; - "D"
;; - "C-"
;; - "C+"
;; - "B-"
;; - "B+"
;; - "A-"
;; - "A"

;; NumberGrade -> LetterGrade
;; given a number grade, returns its letter grade
(check-expect (convert-grade 50) "E")
(check-expect (convert-grade 65) "D")
(check-expect (convert-grade 73) "C-")
(check-expect (convert-grade 79) "C+")
(check-expect (convert-grade 83) "B-")
(check-expect (convert-grade 87) "B+")
(check-expect (convert-grade 93) "A-")
(check-expect (convert-grade 95) "A")
(define (convert-grade g)
  (cond [(<= g 60) "E"]
        [(and (> g 60) (<= g 70)) "D"]
        [(and (> g 70) (<= g 75)) "C-"]
        [(and (> g 75) (<= g 80)) "C+"]
        [(and (> g 80) (<= g 85)) "B-"]
        [(and (> g 85) (<= g 90)) "B+"]
        [(and (> g 90) (<= g 94)) "A-"]
        [(and (> g 94) (<= g 100)) "A"]))

;; Problem 4
(define-struct student (name id grade))
;; A Student is (make-student String PosInt PosInt)
;; INTER: represents a student with the student's name 
;;        the student's id and the student's grade.
;; selector: student-name
;; signature: Student -> String
;; selector: student-id
;; signature: Student -> Positive Integer
;; selector: student-grade
;; signature: Student -> Positive Integer
;; constructor: make-student
;; signature: String, Positive Integer, Positive Integer -> Student
;; predicate: student?
;; signature: Any Value -> Boolean
#; (define (student-template a-student)
     (student-name a-student)...
     (student-id a-student)...
     (student-grade a-student)...)

(define-struct container (width height depth capacity label))
;; A Container is (make-container PosInt PosInt PosInt PosInt Symbol)
;; INTERP: represents a container with its width, height, depth 
;;         in centimeters, it's capacity in cubic centimeters and 
;;         it's label
;; selector: container-width
;; signature: Container -> Positive Integer
;; selector: container-height
;; signature: Container -> Positive Integer
;; selector: container-depth
;; signature: Container -> Positive Integer
;; selector: container-capacity
;; signature: Container -> Positive Integer
;; selector: container-label
;; signature: Container -> Symbol
;; constructor: make-container
;; signature: Positive Integer, Positive Integer, Positive Integer, Positive Integer, Symbol -> Container
;; predicate: container?
;; signature: Any Value -> Boolean
#; (define (container-template c)
     (container-width c)...
     (container-height c)...
     (container-depth c)...
     (container-capacity c)...
     (container-label c)...)

(define-struct sweater (material size producer))
;; A Sweater is (make-sweater Symbol PosInt String)
;; INTERP: represents a sweater with the sweater's material 
;;         it's size and the name of the manufacturer
;; selector: sweater-material
;; signature: Sweater -> Symbol
;; selector: sweater-size
;; signature: Sweater -> Positive Integer
;; selector: sweater-producer
;; signature: Sweater -> String
;; constructor: make-sweater
;; signature: Symbol, Positive Integer, String -> Sweater
;; predicate: sweater?
;; signature: Any Value -> Boolean
#; (define (sweater-template s)
     (sweater-material s)...
     (sweater-size s)...
     (sweater-producer s)...)

(define-struct game (name min-ram min-graphics-ram online?))
;; A Game is (make-game String PosInt PosInt Boolean)
;; INTERP: represents a game with it's name, the minimum ram 
;;         capacity needed , the minimum graphics 
;;         card memory needed and whether it is an online game or not
;; selector: game-name
;; signature: Game -> String
;; selector: game-min-ram
;; signature: Game -> Positive Integer
;; selector: game-min-graphics-ram
;; signature: Game -> Positive Integer
;; selector: game-oneline?
;; signature: Game -> Boolean
;; constructor: make-game
;; signature: String, Positive Integer, Positive Integer, Boolean -> Game
;; predicate: game?
;; signature: Any Value -> Boolean
#; (define (game-template g)
     (game-name g)...
     (game-mine-ram g)...
     (game-min-graphics-ram g)...
     (game-online? g)...)

;; Problem 5
;; Dollars is a PosInt

;; Cents is a PosInt 
;; WHERE: Cents is greater or equal to 0 
;;        and less or equal to 99

(define-struct amount (dollars cents))
;; An Amount is (make-amount Dollars Cents)
;; INTERP: represents total amount in dollars and cents.

;; Amount, Dollars, Cents -> Amount
;; given an amount, dollars to add and cents to add, returns the amounts after adding
(define example-amt (make-amount 1 60))
(check-expect (add-amount example-amt 8 20) (make-amount 9 80))
(check-expect (add-amount example-amt 8 80) (make-amount 10 40))
(define (add-amount amt dollars cents) 
      (make-amount
       (+ (quotient (+ (amount-cents amt) cents) 100) (amount-dollars amt) dollars)
       (remainder (+ (amount-cents amt) cents) 100)))

;; Problem 6
;;; Data Definitions 
(define-struct official (first middle last title))
;; An OfficialName is (make-official String String String String)
;; INTERP: represents a person's official name with first, middle, last name
;;         and title.

;; Template
#; (define (official-template official-name)
     (official-first official-name)...
     (official-middle official-name)...
     (official-last official-name)...
     (official-title official-name)...)

;;; Data Examples
(define JOHN-OFFICIAL (make-official "John" "D." "Doe" "PhD"))



(define-struct full (first last))
;; A FullName is a (make-full String String)
;; INTERP: represents a person's name with first and last name. 

;; Template
#; (define (full-template full-name)
     (full-first full-name)...
     (full-last full-name)...)

;;; Data Examples
(define JOHN-FULL (make-full "John" "Doe"))

;; An Author is one of
;; - OfficialName
;; - FullName
;; INTERP: represents the name of an author for a publication as either
;;         a full name (first and last) or an offical name (first, middle, last
;;         and title)

;; Template
#; (define (author-template an-author)
     (cond [(official? an-author)...]
           [(full? an-author)...]))

;; A Year is a PosInt
;; WHERE: Year is a 4 digit number
;; INTERP: represents a calendar year. 

;; A Month is one of
;; - 'Jan
;; - 'Feb
;; - 'Mar
;; - 'Apr
;; - 'May
;; - 'Jun
;; - 'Jul
;; - 'Aug
;; - 'Sep
;; - 'Oct
;; - 'Nov
;; - 'Dec
;; INTERP: represents a month in a calendar year. 

;; Template
#; (define (month-template a-month)
     (cond [(symbol=? 'Jan)...]
           [(symbol=? 'Feb)...]
           [(symbol=? 'Mar)...]
           [(symbol=? 'Apr)...]
           [(symbol=? 'May)...]
           [(symbol=? 'Jun)...]
           [(symbol=? 'Jul)...]
           [(symbol=? 'Aug)...]
           [(symbol=? 'Sep)...]
           [(symbol=? 'Oct)...]
           [(symbol=? 'Nov)...]
           [(symbol=? 'Dec)...]))

(define-struct conference (title author cname location month year))
;; A Conference is (make-conference String Author String String Month Year)
;; INTERP: represents a conference paper with title, author, conference name,
;;         conference location, month and year

;;; Data Examples
(define JOHN-FULL-CONF (make-conference "Anatomy of a mouse"
                                        JOHN-FULL
                                        "Animal Anatomy"
                                        "London, UK"
                                        'Jul
                                        2003))
(define JOHN-OFFICIAL-CONF (make-conference "Anatomy of a mouse"
                                            JOHN-OFFICIAL
                                            "Animal Anatomy"
                                            "London, UK"
                                            'Jul
                                            2003))


;; Template
#; (define (conference-template a-conference)
     (conference-title a-conference)...
     (conference-author a-conference)...
     (conference-cname a-conference)...
     (conference-location a-conference)...
     (conference-month a-conference)...
     (conference-year a-conference)...)
     
;; An Issue is a PosInt
;; INTERP: represents a journal's issue number

(define-struct journal (title author jname issue month year))
;; A Journal is (make-journal String Author String Issue Month Year)
;; INTERP: represents a journal paper with title, author, journal name,
;;         month and year.

;;; Data Examples
(define JOHN-FULL-JOURNAL  (make-journal "Anatomy of a mouse"
                                         JOHN-FULL
                                         "Mouse Journal"
                                         23
                                         'Feb
                                         2002))

(define JOHN-OFFICIAL-JOURNAL  (make-journal "Anatomy of a mouse"
                                             JOHN-OFFICIAL
                                             "Mouse Journal"
                                             23
                                             'Feb
                                             2002))

;; Template:
#; (define (journal-template a-journal)
     (journal-title a-journal)...
     (journal-author a-journal)...
     (journal-jname a-journal)...
     (journal-issue a-journal)...
     (journal-month a-journal)...
     (journal-year a-journal)...)


(define-struct techreport (title author tr-id institution year))
;; A TechnicalReport is (make-techreport String Author PosInt String Year)
;; INTERP: represents a technical report with title, author,
;;         technical report id, institution name, month and year.


;;; Data Examples

(define JOHN-FULL-TR (make-techreport "Anatomy of a mouse"
                                      JOHN-FULL
                                      1234
                                      "Mouse University"
                                      2001))


(define JOHN-OFFICIAL-TR (make-techreport "Anatomy of a mouse"
                                          JOHN-OFFICIAL
                                          1234
                                          "Mouse University"
                                          2001))


;; Template
#; (define (techreport-template a-technical)
     (techreport-title  a-technical)...
     (techreport-author  a-technical)...
     (techreport-tr-id  a-technical)...
     (techreport-institution  a-technical)...
     (techreport-year  a-technical)...)


;; A Publication is one of
;; - Conference
;; - Journal
;; - TechnicalReport
;; INTERP: represents a publication as either a conference, journal or
;;         technical report. 

;; Template
#; (define (publication-tempate a-pub)
     (cond [(conference? a-pub)...]
           [(journal? a-pub)...]
           [(technreport? a-pub)...]))


;; TechnicalReport, String, Issue, Month, Year -> Journal
;; given an instance of TechnicalReport, a journal's name, a journal's issue, a month, a year
;; returns a new journal publication that has the same title and author as the technical report
;; and the same journal issue, month and year as the values passed as arguments.
(check-expect (tr->journal JOHN-FULL-TR "Nature" 24 'Feb 2016)
              (make-journal "Anatomy of a mouse" JOHN-FULL "Nature" 24 'Feb 2016))
(define (tr->journal a-tech a-jname an-issue a-month a-year)
  (make-journal
   (techreport-title a-tech)
   (techreport-author a-tech)
   a-jname
   an-issue
   a-month
   a-year))

;; Publication -> Image
;; given a publication, returns a formatted text image with its information
(check-expect (publication->image JOHN-FULL-CONF)
               (beside
               (text/font "\"Anatomy of a mouse\". John Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Animal Anatomy, London, UK. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "Jul, 2003." 20 "black" #false "modern" "normal" "bold" #false)))
(check-expect (publication->image JOHN-OFFICIAL-CONF)
              (beside
               (text/font "\"Anatomy of a mouse\". PhD John D. Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Animal Anatomy, London, UK. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "Jul, 2003."
                          20 "black" #false "modern" "normal" "bold" #false)))
(check-expect (publication->image JOHN-FULL-JOURNAL)
              (beside
               (text/font "\"Anatomy of a mouse\". John Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Mouse Journal, 23. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "Feb, 2002."
                          20 "black" #false "modern" "normal" "bold" #false)))
(check-expect (publication->image JOHN-OFFICIAL-JOURNAL)
              (beside
              (text/font "\"Anatomy of a mouse\". PhD John D. Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Mouse Journal, 23. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "Feb, 2002."
                          20 "black" #false "modern" "normal" "bold" #false)))
(check-expect (publication->image JOHN-FULL-JOURNAL)
              (beside
               (text/font "\"Anatomy of a mouse\". John Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Mouse Journal, 23. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "Feb, 2002."
                          20 "black" #false "modern" "normal" "bold" #false)))
(check-expect (publication->image JOHN-OFFICIAL-JOURNAL)
              (beside
              (text/font "\"Anatomy of a mouse\". PhD John D. Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Mouse Journal, 23. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "Feb, 2002."
                          20 "black" #false "modern" "normal" "bold" #false)))
(check-expect (publication->image JOHN-FULL-TR)
              (beside
               (text/font "\"Anatomy of a mouse\". John Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Mouse University, 1234. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "2001."
                          20 "black" #false "modern" "normal" "bold" #false)))
(check-expect (publication->image JOHN-OFFICIAL-TR)
              (beside
              (text/font "\"Anatomy of a mouse\". PhD John D. Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Mouse University, 1234. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "2001."
                          20 "black" #false "modern" "normal" "bold" #false)))
(check-expect (publication->image "Anatomy of a mouse. ") #false)
(define (publication->image a-pub)
  (cond [(journal? a-pub) (journal-f a-pub)]
        [(conference? a-pub) (conference-f a-pub)]
        [(techreport? a-pub) (techreport-f a-pub)]
        [else #false]))
              

;; Conference -> Image
;; given an Conference, returns a formatted text image with its information
(check-expect (conference-f JOHN-FULL-CONF)
              (beside
               (text/font "\"Anatomy of a mouse\". John Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Animal Anatomy, London, UK. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "Jul, 2003." 20 "black" #false "modern" "normal" "bold" #false)))
(check-expect (conference-f JOHN-OFFICIAL-CONF)
              (beside
               (text/font "\"Anatomy of a mouse\". PhD John D. Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Animal Anatomy, London, UK. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "Jul, 2003."
                          20 "black" #false "modern" "normal" "bold" #false)))
(define (conference-f a-conference)
  (beside
   (text/font (string-append "\""
                             (conference-title a-conference)
                             "\". "
                             (if (full? (conference-author a-conference))
                                 (string-append (full-first (conference-author a-conference))
                                                 " "
                                                 (full-last (conference-author a-conference))
                                                 ". ")
                                 (string-append (official-title (conference-author a-conference))
                                                 " "
                                                (official-first (conference-author a-conference))
                                                " "
                                                (official-middle (conference-author a-conference))
                                                " "
                                                (official-last (conference-author a-conference))
                                                ". ")))
              20 "black" #false "modern" "normal" "normal" #false)
   (text/font (string-append (conference-cname a-conference)
                             ", "
                             (conference-location a-conference)
                             ". ")
              20 "black" #false "modern" "italic" "normal" #false)
   (text/font (string-append (symbol->string (conference-month a-conference))
                             ", "
                             (number->string (conference-year a-conference))
                             ".")
              20 "black" #false "modern" "normal" "bold" #false)))

;; Journal -> Image
;; given a Journal, returns a formatted text image with its information
(check-expect (journal-f JOHN-FULL-JOURNAL)
              (beside
               (text/font "\"Anatomy of a mouse\". John Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Mouse Journal, 23. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "Feb, 2002."
                          20 "black" #false "modern" "normal" "bold" #false)))
(check-expect (journal-f JOHN-OFFICIAL-JOURNAL)
              (beside
              (text/font "\"Anatomy of a mouse\". PhD John D. Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Mouse Journal, 23. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "Feb, 2002."
                          20 "black" #false "modern" "normal" "bold" #false)))
(define (journal-f a-journal)
  (beside
   (text/font (string-append "\""
                             (journal-title a-journal)
                             "\". "
                             (if (full? (journal-author a-journal))
                                 (string-append (full-first (journal-author a-journal))
                                                 " "
                                                 (full-last (journal-author a-journal))
                                                 ". ")
                                 (string-append (official-title (journal-author a-journal))
                                                 " "
                                                (official-first (journal-author a-journal))
                                                " "
                                                (official-middle (journal-author a-journal))
                                                " "
                                                (official-last (journal-author a-journal))
                                                ". ")))
              20 "black" #false "modern" "normal" "normal" #false)
   (text/font (string-append (journal-jname a-journal)
                             ", "
                             (number->string (journal-issue a-journal))
                             ". ")
              20 "black" #false "modern" "italic" "normal" #false)
   (text/font (string-append (symbol->string (journal-month a-journal))
                             ", "
                             (number->string (journal-year a-journal))
                             ".")
              20 "black" #false "modern" "normal" "bold" #false)))

;; TechnicalReport -> Image
;; given a TechnicalReport, returns a formatted text image with its information
(check-expect (techreport-f JOHN-FULL-TR)
              (beside
               (text/font "\"Anatomy of a mouse\". John Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Mouse University, 1234. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "2001."
                          20 "black" #false "modern" "normal" "bold" #false)))
(check-expect (techreport-f JOHN-OFFICIAL-TR)
              (beside
              (text/font "\"Anatomy of a mouse\". PhD John D. Doe. "
                          20 "black" #false "modern" "normal" "normal" #false)
               (text/font "Mouse University, 1234. "
                          20 "black" #false "modern" "italic" "normal" #false)
               (text/font "2001."
                          20 "black" #false "modern" "normal" "bold" #false)))
(define (techreport-f a-techreport)
  (beside
   (text/font (string-append "\""
                             (techreport-title a-techreport)
                             "\". "
                             (if (full? (techreport-author a-techreport))
                                 (string-append (full-first (techreport-author a-techreport))
                                                 " "
                                                 (full-last (techreport-author a-techreport))
                                                 ". ")
                                 (string-append (official-title (techreport-author a-techreport))
                                                 " "
                                                (official-first (techreport-author a-techreport))
                                                " "
                                                (official-middle (techreport-author a-techreport))
                                                " "
                                                (official-last (techreport-author a-techreport))
                                                ". ")))
              20 "black" #false "modern" "normal" "normal" #false)
   (text/font (string-append (techreport-institution a-techreport)
                             ", "
                             (number->string (techreport-tr-id a-techreport))
                             ". ")
              20 "black" #false "modern" "italic" "normal" #false)
   (text/font (string-append (number->string (techreport-year a-techreport))
                             ".")
              20 "black" #false "modern" "normal" "bold" #false)))



   
  
