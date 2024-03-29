;; Example problem definition for the Shrdlite domain
(define (problem ShrdliteProblem)
  (:domain ShrdLitePosition)
  (:objects a b c d col1 col2)
  (:init
    (block a)
    (block b)
    (block c)
    (block d)
    (column col1 0)
    (column col2 1)
    (ontop a col1)
    (ontop b a)
    (ontop c col2)
    (ontop d c)
    (rightof c a)
    (rightof d a)
    (rightof c b)
    (rightof d b)
    (leftof a c)
    (leftof b c)
    (leftof a d)
    (leftof b d)
  )
  (:goal
    (ingrabber c)
  )
)