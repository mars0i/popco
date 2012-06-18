; Experiment concerning analog structure order

(mapcar #'clear-plists (get 'presidents 'members))
(clear-person-nets 'presidents)

; The persons in the 1st pair should differ from those in the 2nd pair,
; on the hypothesis that order of analog structures matters.
; THIS TURNED OUT TO BE FALSE.

; I thought that the 1st two persons should be largely identical; 'target, 'source are abitrary:
(make-person 'iraq-targ-1st 'presidents
             '()
             '((make-struc 'target 'problem
                           '(start
                              ((president-of (Saddam Iraq) PSI)
                               (invade (Iraq Kuwait) IIK))))
               (make-struc 'source 'problem
                           '(start
                              ((fuhrer-of (Adolph Germany) FHG)
                               (occupy (Germany Austria) OGA))))))

(make-person 'ww2-targ-2nd 'presidents
             '()
             '((make-struc 'source 'problem
                           '(start
                              ((president-of (Saddam Iraq) PSI)
                               (invade (Iraq Kuwait) IIK))))
               (make-struc 'target 'problem
                           '(start
                              ((fuhrer-of (Adolph Germany) FHG)
                               (occupy (Germany Austria) OGA))))))

; I thought that these two persons should be also largely identical:
(make-person 'ww2-targ-1st 'presidents
             '()
             '((make-struc 'target 'problem
                           '(start
                              ((fuhrer-of (Adolph Germany) FHG)
                               (occupy (Germany Austria) OGA))))
               (make-struc 'source 'problem
                           '(start
                              ((president-of (Saddam Iraq) PSI)
                               (invade (Iraq Kuwait) IIK))))))

(make-person 'iraq-targ-2nd 'presidents
             '()
             '((make-struc 'source 'problem
                           '(start
                              ((fuhrer-of (Adolph Germany) FHG)
                               (occupy (Germany Austria) OGA))))
               (make-struc 'target 'problem
                           '(start
                              ((president-of (Saddam Iraq) PSI)
                               (invade (Iraq Kuwait) IIK))))))

; Another experiment.  Calling pplist on this person shows that it's deformed.
; So the names 'target and 'source are essential. (as of early sept 2011)
; This appears to come from my function update-net in popco.lisp.
(make-person 'iraq-zowee 'presidents
             '()
             '((make-struc 'zowee 'problem
                           '(start
                              ((president-of (Saddam Iraq) PSI)
                               (invade (Iraq Kuwait) IIK))))
               (make-struc 'blahblah 'problem
                           '(start
                              ((fuhrer-of (Adolph Germany) FHG)
                               (occupy (Germany Austria) OGA))))))

(create-nets 'presidents)

; try this:

(setf   *the-person* 'iraq-targ-1st   itf (mapcar #'personal-to-generic-sym (get 'iraq-targ-1st 'all-units)))
(setf   *the-person* 'iraq-targ-2nd  its (mapcar #'personal-to-generic-sym (get 'iraq-targ-2nd 'all-units)))
(setf   *the-person* 'ww2-targ-1st    wtf (mapcar #'personal-to-generic-sym (get 'ww2-targ-1st 'all-units)))
(setf   *the-person* 'ww2-targ-2nd   wts (mapcar #'personal-to-generic-sym (get 'ww2-targ-2nd 'all-units)))

; now note that (equal itf its) and (equal wtf wts)
; but not (equal itf wtf) or (equal its wts)
; the difference is in which name appears before/after the equal sign in the map node names

; further investigation shows that iraq-targ-1st and iraq-targ-2nd are the same apart from the use
; of the person-names in symbols and a few differences of order.  I assume the same is true for ww2-targ-*.
