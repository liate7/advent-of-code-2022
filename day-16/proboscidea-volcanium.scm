(define-module (day-16 proboscidea-volcanium)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define test-input
  '("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
    "Valve HH has flow rate=22; tunnel leads to valve GG"
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
    "Valve JJ has flow rate=21; tunnel leads to valve II"
    ""))

(define (tests)
  (in-test-suite ("Day 16: Proboscidea Volcanium")
    (test-equal (star-1 test-input) 1651)))



(define (star-1 lines)
  (error "Not implemented yet"))
