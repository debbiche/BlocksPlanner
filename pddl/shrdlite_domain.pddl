(define (domain ShrdLite)
  (:requirements :strips, :adl)
  (:predicates
                (column ?column ?columnIndex)
                (oncolumn ?column ?block)
                (leftof ?block1 ?block2)
                (rightof ?block1 ?block2)
                (block ?block)
                (pyramid ?block)
                (ball ?block)
                (small ?block)
                (medium ?block)
                (large ?block)
                (ontop ?block1 ?thing) ;; Expresses that block1 is on top of thing
                (ingrabber ?block)
  )
  (:action pick
                :parameters (?column)
                :precondition
                  (and 
                    (column(?column))
                    (exists (?block) 
                      (and
                        (oncolumn ?column ?block)
                        (forall (?otherBlock) (not (ontop ?otherBlock ?block)))
                        (not (ingrabber ?block))
                      )
                    )
                  )
                :effect
                  (when
                    ;; condition
                    (exists (?block)
                      (and
                        (oncolumn ?column ?block)
                        (forall (?otherBlock) (not (ontop ?otherBlock ?block)))
                        (not (ingrabber ?block))
                      ))
                    ;; effect
                    (and
                      (ingrabber ?block)
                      (not (oncolumn ?column ?block))
                      (foreach (?otherBlock)
                        (and
                          (when (and (ontop ?block ?otherBlock))
                            (and
                              (not (ontop ?block ?otherBlock))
                            )
                          )
                          (when (and (leftof ?otherBlock ?block))
                            (and
                              (not (leftof ?otherBlock ?block))
                            )
                          )
                          (when (and (rightof ?otherBlock ?block))
                            (and
                              (not (rightof ?otherBlock ?block))
                            )
                          )
                        )
                      )
                    )
                  )
  )
  (:action drop
                :parameters (?columnIndex)
                :precondition
                  (and
                    (exists (?block) (ingrabber ?block))
                    (exists (?column) 
                      (and
                        ;; There should be a column at the selected idnex
                        (column(?column ?columnIndex))
                        ;; There should not be a block that is a pyramid or ball and that is topmost 
                        ;; of that column
                        (not
                          (exists (?otherBlock)
                            (and
                              (oncolumn ?column ?otherBlock)
                              (not (exists (?thirdBlock)(ontop ?thirdBlock ?otherBlock)))
                              (or
                                (pyramid ?otherBlock)
                                (ball ?otherBlock)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                :effect
                  (when (exists (?block) (ingrabber ?block))
                    (when (exists (?column) (column ?column ?columnIndex))
                      (and 
                        (oncolumn ?column ?block)
                        (not (ingrabber ?block))
                        ;; Add ontop for the highest block in the selected column
                        (when (exists
                                (?otherBlock)
                                (and
                                  (oncolumn(?column ?block))
                                  (not
                                    (exists(?thirdBlock)(and(ontop(?thirdblock ?block))))
                                  )
                                )
                              )
                              (and(ontop(?block ?otherBlock)))
                        )
                        ;; Add leftof for blocks in the neighbouring column
                        (when 
                          (exists (?otherColumn) (and (column ?otherColumn) (leftof (?otherColumn ?column)))
                            (forall (?otherBlock)
                              (and
                                (leftof ?otherBlock ?block)
                              )
                            )
                          )
                        )
                        ;; Add rightof for blocks in the neighbouring column
                        (when 
                          (exists (?otherColumn) (and (column ?otherColumn) (rightof (?otherColumn ?column)))
                            (forall (?otherBlock)
                              (when (and(oncolumn ?otherColumn ?otherBlock))
                                (and
                                  (rightof ?otherBlock ?block)
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
  )
)