(define-module (day-7 full-device)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define test-input
  '("$ cd /"
    "$ ls"
    "dir a"
    "14848514 b.txt"
    "8504156 c.dat"
    "dir d"
    "$ cd a"
    "$ ls"
    "dir e"
    "29116 f"
    "2557 g"
    "62596 h.lst"
    "$ cd e"
    "$ ls"
    "584 i"
    "$ cd .."
    "$ cd .."
    "$ cd d"
    "$ ls"
    "4060174 j"
    "8033020 d.log"
    "5626152 d.ext"
    "7214296 k"
    ""))

(define (tests)
  (in-test-suite ("Day 7: No Space Left On Device")
    (test-equal (star-1 test-input) 95437)))



(define (tokenize-line line)
  (string-split line char-set:whitespace))

(define (process-ls lines entries)
  (match (tokenize-line (car lines))
    ((or ("") ("$" _ ...))
     (vector lines entries))
    (((? string->number size) name)
     (process-ls (cdr lines)
                 (cons (list name (string->number size))
                       entries)))
    (("dir" name)
     (process-ls (cdr lines)
                 (cons (list name 'dir)
                       entries)))))

(define (unformed-directory-in? entries name)
  (equal? (assoc-ref entries name) '(dir)))

(define (read-sized-dir name lines)
  (cond ((null? lines) (vector '() '()))
        ((not (equal? (tokenize-line (car lines))
                      '("$" "ls")))
         (error "Malformed log: ~a is not an \"ls\"" (car lines)))
        (else (match-let rec ((#(lines entries) (process-ls (cdr lines) '())))
                (match (tokenize-line (car lines))
                  ((or () (""))
                   (vector lines entries))
                  (("$" "cd" "..")
                   (vector (cdr lines) entries))
                  (("$" "cd" (? (curry unformed-directory-in? entries) dir))
                   (match-let ((#(lines_ dir-entries) (read-sized-dir dir (cdr lines))))
                     (rec (vector lines_ (assoc-set entries dir dir-entries))))))))))

(define (recover-sized-file-tree lines)
  (match (tokenize-line (car lines))
    (("$" "cd" dir)
     (match-let ((#((or () ("")) entries) (read-sized-dir dir (cdr lines))))
       (cons dir entries)))))

(define (file-tree-size file-tree)
  (let rec ((node file-tree))
    (match node
      ((name (? number? size))
       size)
      ((name . entries)
       (apply + (map rec entries))))))

(define (filter-file-tree pred file-tree)
  "Returns a list of all nodes where (PRED NODE) is true"
  (reverse
   (let rec ((node file-tree)
             (acc '()))
     (match node
       ((name (? number?))
        (if (pred node)
            (cons (list name) acc)
            acc))
       ((name . entries)
        (append (map (curry cons name) (fold rec '() entries))
                (if (pred node) (list (list name)) '())
                acc))))))

(define (file-tree-ref file-tree path)
  (let rec ((node file-tree)
            (path path))
    (match (list node path)
      (((name . _) (name))
       node)
      (((dir . entries)
        (dir . (and (next . _) rest)))
       (and (assoc-ref entries next)
            (rec (assoc next entries equal?)
                 rest))))))

(define (star-1 lines)
  (define small-directory?
    (match-lambda
      ((name (? number?))
       #f)
      ((and node (name . entries))
       (< (file-tree-size node) 100000))))
  (let ((tree (recover-sized-file-tree lines)))
    (->> tree
         (filter-file-tree small-directory?)
         (map (compose file-tree-size
                       (curry file-tree-ref tree)))
         (sum))))
