;; This is a rudimentary domain that bases facts upon row and column indices
;; It is not complete as it lacks:
;;  - predicates for leftof and rightof, which are needed to express the goals

(define (domain ShrdLite)
  (:requirements :strips,:adl)
  (:predicates
                (row ?index)
                (column ?index)
                (block ?block)
                (at ?block ?row ?column)
                (emptyPosition ?row ?column)
                (grabber ?g)
                (ingrabber ?grabber ?block)
                (emptyGrabber ?grabber)
  )
  (:action pick
                :parameters (?block ?row ?column ?grabber)
                :precondition
                  (and
                    (block ?block)
                    (grabber ?grabber)
                    (at ?block ?row ?column)
                    (emptyGrabber ?grabber)
                  )
                :effect
                  (and
                    (not (at ?block ?row ?column ))
                    (emptyPosition ?row ?column)
                    (ingrabber ?block)
                    (not (emptyGrabber ?grabber))
                  )
  )
  (:action drop
                :parameters (?block ?row ?column ?grabber)
                :precondition
                  (and
                    (ingrabber ?grabber ?block)
                    (not (exists(?otherBlock) (and (at(?otherBlock ?row ?column)))))
                  )
                :effect
                  (and 
                    (not (ingrabber(?grabber ?block)))
                    (at ?block ?row ?column)
                  )
  )
)