(defpackage #:open-orders.random
  (:use #:cl))
(in-package #:open-orders.random)

(defparameter *first-names*
  '("James" "Michael" "John" "Robert" "David" "William"
    "Richard" "Joseph" "Thomas" "Christopher" "Charles" "Daniel"
    "Matthew" "Anthony" "Mark" "Steven" "Andrew" "Donald"
    "Joshua" "Paul" "Kevin" "Kenneth" "Brian" "Timothy"
    "Ronald" "Jason" "George" "Edward" "Jeffrey" "Jacob"
    "Ryan" "Nicholas" "Gary" "Eric" "Jonathan" "Stephen"
    "Larry" "Justin" "Benjamin" "Scott" "Brandon" "Samuel"
    "Alexander" "Gregory" "Patrick" "Jack" "Frank" "Raymond"
    "Dennis" "Aaron" "Tyler" "Jerry" "Jose" "Nathan"
    "Adam" "Henry" "Zachary" "Douglas" "Peter" "Noah"
    "Ethan" "Kyle" "Christian" "Jeremy" "Austin" "Keith"
    "Sean" "Terry" "Roger" "Dylan" "Walter" "Gerald"
    "Jordan" "Gabriel" "Carl" "Bryan" "Jesse" "Logan"
    "Lawrence" "Elijah" "Arthur" "Bruce" "Harold" "Billy"
    "Liam" "Alan" "Juan" "Joe" "Mason" "Lucas"
    "Randy" "Willie" "Wayne" "Vincent" "Caleb" "Albert"
    "Luke" "Isaac" "Bradley" "Cameron" "Mary" "Patricia"
    "Jennifer" "Linda" "Elizabeth" "Barbara" "Susan" "Jessica"
    "Karen" "Lisa" "Nancy" "Sandra" "Ashley" "Emily"
    "Kimberly" "Michelle" "Donna" "Margaret" "Carol" "Betty"
    "Amanda" "Melissa" "Deborah" "Stephanie" "Rebecca" "Sharon"
    "Cynthia" "Laura" "Amy" "Kathleen" "Angela" "Emma"
    "Shirley" "Dorothy" "Brenda" "Nicole" "Pamela" "Samantha"
    "Anna" "Olivia" "Katherine" "Christine" "Debra" "Rachel"
    "Maria" "Carolyn" "Janet" "Heather" "Diane" "Julie"
    "Catherine" "Victoria" "Joyce" "Lauren" "Kelly" "Christina"
    "Helen" "Joan" "Hannah" "Judith" "Andrea" "Evelyn"
    "Megan" "Cheryl" "Ruth" "Virginia" "Sophia" "Madison"
    "Jacqueline" "Abigail" "Isabella" "Teresa" "Sara" "Charlotte"
    "Janice" "Kathryn" "Martha" "Gloria" "Ann" "Judy"
    "Amber" "Danielle" "Denise" "Grace" "Julia" "Natalie"
    "Diana" "Marilyn" "Brittany" "Beverly" "Alice" "Theresa"
    "Jean" "Kayla" "Alexis" "Frances" "Ava" "Tiffany"
    "Lori" "Sarah"))


(defparameter *last-names*
  '("Smith" "Johnson" "Williams" "Brown" "Jones" "Garcia" "Miller"
    "Rodriguez" "Martinez" "Hernandez" "Gonzales" "Wilson" "Anderson"
    "Thomas" "Taylor" "Moore" "Jackson" "Martin" "Lee" "Perez" "Thompson"
    "White" "Harris" "Sanchez" "Clark" "Ramirez" "Lewis" "Robinson"
    "Walker" "Young" "Allen" "King" "Wright" "Scott" "Torres"
    "Nguyen" "Hill" "Flores" "Green" "Adams" "Nelson" "Baker"
    "Hall" "Rivera" "Campbell" "Mitchell" "Carter" "Roberts" "Gomez"
    "Phillips" "Evans" "Turner" "Diaz" "Parker" "Cruz" "Edwards"
    "Collins" "Reyes" "Stewart" "Morris" "Morales" "Murphy" "Cook"
    "Rogers" "Gutierrez" "Ortiz" "Morgan" "Cooper" "Peterson" "Bailey"
    "Reed" "Kelly" "Howard" "Ramos" "Kim" "Cox" "Ward"
    "Richardson" "Watson" "Brooks" "Chavez" "Wood" "James" "Bennet"
    "Gray" "Mendoza" "Ruiz" "Hughes" "Price" "Alvarez" "Castillo"
    "Sanders" "Patel" "Myers" "Long" "Ross" "Foster" "Jimenez"))

(defparameter *last-name-prefixes*
  '("Von " "Van " "Del " "O'"))

(defparameter *trees*
  '("Ash" "Oak" "Post Oak" "Live Oak" "Cedar"
    "Juniper" "Elm" "Cedar Elm" "Birch" "Apple"
    "Orange" "Lemon" "Walnut" "Pecan"))

(defparameter *flowers*
  '("Rose" "Daisy" "Violet" "Dandelion" "Briar"
    "Sunflower" "Petunia" "Tulip" "Bluebonnet"
    "Forget Me Not" "Sunwheel" "Indian Paintbrush"))

(defparameter *rocks*
  '("Granite" "Diorite" "Slate" "Mica" "Limestone" "Basalt"))

(defparameter *presidents* 
  '("Bush" "Clinton" "Reagan" "Carter" "Ford"
    "Nixon" "Johnson" "Fitzgerald" "Eisenhower" "Truman"
    "Roosevelt" "Hoover" "Coolidge" "Harding" "Wilson"
    "Taft" "Roosevelt" "McKinley" "Cleveland" "Harrison"
    "Cleveland" "Arthur" "Garfield" "Hayes" "Grant"
    "Johnson" "Lincoln" "Buchanan" "Pierce" "Fillmore"
    "Taylor" "Polk" "Tyler" "Harrison" "Van"
    "Jackson" "Adams" "Monroe" "Madison" "Jefferson"
    "Washington"))

(defparameter *street-suffixes*
  '("Street" "Avenue" "Parkway" "Circle" "Way" "Loop"
    "Highway"))

(defparameter *single-letter-street-name-chance* 1/15)
(defparameter *numerical-street-name-chance* 1/15)

(defun upcase)

(defun random-capital-letter ()
  (make-string
   1 :initial-element
   (code-char (+ (char-code #\A)
                 (random (- (char-code #\Z) (char-code #\A)))))))

(defun street ()
  (concatenate
   'string
   (cond ((rarely *single-letter-street-name-chance*)
          (format nil "~a." (random-capital-letter)))
         ((rarely *numerical-street-name-chance*)
          (format nil "~:R" (random 100)))
         (t (random-value (concatenate 'list
                                       *trees* *rocks*
                                       *flowers* *presidents*))))
   " "
   (random-value *street-suffixes*))
  )

(defparameter *vowels* '(#\a #\e #\i #\o #\u))
(defun vowelp (letter) (member letter *vowels*))
(defparameter *consonants*
  (loop :for code :from (char-code #\a) :to (char-code #\z)
        :for char = (code-char code) 
        :unless (vowelp char)
          :collect char))
(defun consonantp (letter)
  (member letter *consonants*))

(defparameter *last-name-prefix-chance* 1/15)
(defparameter *first/last-swap-chance* 1/10)
(defparameter *change-letter-chance* 1/12)

(defun random-index (list)
  (random (length list)))

(defun random-value (list)
  (nth (random-index list) list))

(defun rarely (chance)
  "chance should be a fraction like 1/10, or 1/100"
  (assert (>= chance 0/1))
  (assert (<= chance 1/1))
  (= 1 (random (/ 1 chance))))

(defun random-consonant ()
  (random-value *consonants*))

(defun random-vowel ()p
  (random-value *vowels*))

(defun change-random-letter (string)
  "changes a random letter to a random consonant or vowel"
  (let* ((copy (copy-seq string))
         (i (random-index string))
         (ch (aref string i))
         (new-letter (if (vowelp ch)
                         (random-vowel)
                         (random-consonant))))
    (setf (aref copy i)
          (if (upper-case-p ch)
              (char-upcase new-letter)
              new-letter))
    copy))

(defun rarely-change-letter (string)
  (if (rarely *change-letter-chance*)
      (change-random-letter string)
      string))

(defun first-name ()
  (rarely-change-letter
   (random-value
    (if (rarely *first/last-swap-chance*)
        *last-names*
        *first-names*))))

(defun last-name ()
  (rarely-change-letter
   (random-value
    (if (rarely *first/last-swap-chance*)
        *first-names*
        *last-names*))))

(defun last-name-prefix ()
  (if (rarely *last-name-prefix-chance*)
      (random-value *last-name-prefixes*)
      ""))

(defun full-name ()
  (concatenate 'string (first-name) " " (last-name-prefix) (last-name)))
