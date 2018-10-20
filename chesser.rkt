#lang racket

(require racket/draw)

(define cell-size 30)

(define w-king "\u2654")
(define w-queen "\u2655")
(define w-rook "\u2656")
(define w-bishop "\u2657")
(define w-knight "\u2658")
(define w-pawn "\u2659")

(define b-king "\u265A")
(define b-queen "\u265B")
(define b-rook "\u265C")
(define b-bishop "\u265D")
(define b-knight "\u265E")
(define b-pawn "\u265F")

(define configs
  '())

(define (init-config)
  (vector (vector b-rook b-knight b-bishop
                  b-king b-queen b-bishop
                  b-knight b-rook)
          (vector b-pawn b-pawn b-pawn b-pawn
                  b-pawn b-pawn b-pawn b-pawn)
          (vector empty empty empty empty
                  empty empty empty empty)
          (vector empty empty empty empty
                  empty empty empty empty)
          (vector empty empty empty empty
                  empty empty empty empty)
          (vector empty empty empty empty
                  empty empty empty empty)
          (vector w-pawn w-pawn w-pawn w-pawn
                  w-pawn w-pawn w-pawn w-pawn)
          (vector w-rook w-knight w-bishop
                  w-king w-queen w-bishop
                  w-knight w-rook)))

(define (draw-board config)
  (let* ([size (* 8 cell-size)]
         [target (make-bitmap (+ 20 size) (+ 20 size))]
         [dc (new bitmap-dc% [bitmap target])])
    (send dc set-brush "brown" 'solid)
    (send dc draw-rectangle 0 0 (+ 20 size) (+ 20 size))
    (let ([height 10]
          [bwr 1])
      (for ([row-vector config])
        (let ([width 10]
              [bwc bwr])
          (for ([col row-vector])
            (if (< bwc 0)
                (send dc set-brush "white" 'solid)
                (send dc set-brush "gray" 'solid))
            (send dc draw-rectangle
                  width height
                  cell-size cell-size)
            (send dc draw-text (if (empty? col) " " col)
                  (+ width (/ cell-size 4))
                  (+ height (/ cell-size 4)))
            (set! width (+ width cell-size))
            (set! bwc (* -1 bwc))))
        (set! height (+ height cell-size))
        (set! bwr (* -1 bwr))))
    target))
