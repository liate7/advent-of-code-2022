(define-module (advent-of-code-2022 day-2 rock-paper-scissors)
  #:use-module (advent-of-code-2022 utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;;; Appreciative of your help yesterday, one Elf gives you an encrypted strategy guide (your puzzle
;;; input) that they say will be sure to help you win. "The first column is what your opponent is
;;; going to play: A for Rock, B for Paper, and C for Scissors. The second column--" Suddenly, the
;;; Elf is called away to help with someone's tent.

;;; The second column, you reason, must be what you should play in response: X for Rock, Y for
;;; Paper, and Z for Scissors. Winning every time would be suspicious, so the responses must have
;;; been carefully chosen.

;;; The winner of the whole tournament is the player with the highest score. Your total score is
;;; the sum of your scores for each round. The score for a single round is the score for the shape
;;; you selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of
;;; the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

;;; Since you can't be sure if the Elf is trying to help you or trick you, you should calculate the
;;; score you would get if you were to follow the strategy guide.

;;; For example, suppose you were given the following strategy guide:

;;; A Y
;;; B X
;;; C Z

;;; This strategy guide predicts and recommends the following:

;;;     In the first round, your opponent will choose Rock (A), and you should choose Paper
;;;     (Y). This ends in a win for you with a score of 8 (2 because you chose Paper + 6 because
;;;     you won).  In the second round, your opponent will choose Paper (B), and you should choose
;;;     Rock (X). This ends in a loss for you with a score of 1 (1 + 0).  The third round is a draw
;;;     with both players choosing Scissors, giving you a score of 3 + 3 = 6.

;;; In this example, if you were to follow the strategy guide, you would get a total score of 15 (8
;;; + 1 + 6).

;;; What would your total score be if everything goes exactly according to your strategy guide?

(define (decode-other str)
  (match str
    ("A" 'rock)
    ("B" 'paper)
    ("C" 'scissors)))

(define (decode-line line)
  (define (decode-self str)
    (match str
      ("X" 'rock)
      ("Y" 'paper)
      ("Z" 'scissors)))
  (match (string-split line #\space)
    ((other self)
     (list (decode-other other)
           (decode-self self)))
    (("") #f)))

(define (line-score line)
  (define (throw-score line)
    (match line
      ((_other 'rock)     1)
      ((_other 'paper)    2)
      ((_other 'scissors) 3)))
  (define (outcome-score line)
    (match line
      ((x x) 3) ; draw
      ((or '(rock paper) ; win
           '(paper scissors)
           '(scissors rock))
       6)
      ((or '(paper rock) ; loss
           '(scissors paper)
           '(rock scissors))
       0)))
  (if line
      (+ (throw-score line)
         (outcome-score line))
      0))

(define (guide-score lines)
  (sum (map (compose line-score decode-line)
            lines)))

;;; The Elf finishes helping with the tent and sneaks back over to you. "Anyway, the second
;;; column says how the round needs to end: X means you need to lose, Y means you need to end the
;;; round in a draw, and Z means you need to win. Good luck!"

;;; Following the Elf's instructions for the second column, what would your total score be if
;;; everything goes exactly according to your strategy guide?

(define wins-against 
  '((paper . rock)
    (rock . scissors)
    (scissors . paper)))
(define loses-against
  (map (lambda (assoc)
         (xcons (car assoc) (cdr assoc)))
       wins-against))

(define (decode-line-v2 line)
  (define (decode-self self)
    (match self 
      ("Y" 'draw)
      ("X" 'lose)
      ("Z" 'win)))
  (match (string-split line #\space)
    (("") #f)
    ((other self)
     (list (decode-other other)
           (decode-self self)))))

(define (get-throw-from-line line)
  (match line
    ((throw 'win)  (assoc-ref loses-against throw))
    ((throw 'draw) throw)
    ((throw 'lose) (assoc-ref wins-against throw))))

(define (line-score-v2 line)
  (define (outcome-score line)
    (match line
      ((other 'win)  6)
      ((other 'draw) 3)
      ((other 'lose) 0)))
  (define (throw-score line)
    (match (get-throw-from-line line)
      ('rock     1)
      ('paper    2)
      ('scissors 3)))
  (if line
      (+ (throw-score line) (outcome-score line))
      0))

(define (guide-score-v2 lines)
  (sum (map (compose line-score-v2 decode-line-v2)
            lines)))
