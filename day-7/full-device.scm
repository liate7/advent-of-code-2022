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



