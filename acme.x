
           (and (= (length args1) (length args2)) ; cond 1
                ; don't match objects and propositions:
                (type-compatible args1 args2 (get *the-person* 'all-propositions)))
