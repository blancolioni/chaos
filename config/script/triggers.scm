;(lith-define add-trigger (macro (index name params fn) (list 'chaos-add-trigger index name params fn)))
(chaos-add-trigger 1 'Acquired '(chaos-flag this acquired text-1))
(chaos-add-trigger 2 'AttackedBy '(chaos-script-flag this object 'attacked-by))
(chaos-add-trigger #x0D 'Exists '(chaos-object-exists object))
(chaos-add-trigger #x0F 'Global '(eq? (chaos-get-property text-1 text-2) integer-1))
(chaos-add-trigger #x1C 'See '(chaos-can-see this object))
(chaos-add-trigger #x20 'HitBy '(chaos-script-flag this object 'hit-by))
(chaos-add-trigger #x22 'TimerExpired '(chaos-timer-expired this integer-1))
(chaos-add-trigger #x23 'True #t)
(chaos-add-trigger #x30 'False #f)
(chaos-add-trigger #x36 'OnCreation '(not (chaos-flag this 'script-executed)))
(chaos-add-trigger #x39 'NumTimesTalkedTo '(= (chaos-get-property this 'num-times-talked-to) integer-1))
(chaos-add-trigger #x39 'NumTimesTalkedToGT '(> (chaos-get-property this 'num-times-talked-to) integer-1))
(chaos-add-trigger #x3B 'NumTimesTalkedToLT '(< (chaos-get-property this 'num-times-talked-to) integer-1))
(chaos-add-trigger #x3C 'Reaction '(= (chaos-get-reaction this object) integer-1))
(chaos-add-trigger #x3D 'ReactionGT '(> (chaos-get-reaction this object) integer-1))
(chaos-add-trigger #x3E 'ReactionLT '(< (chaos-get-reaction this object) integer-1))
(chaos-add-trigger #x47 'RandomNum '(= (random 1 integer-1) integer-2))
(chaos-add-trigger #x48 'RandomNumGT '(> (random 1 integer-1) integer-2))
(chaos-add-trigger #x49 'RandomNumLT '(< (random 1 integer-1) integer-2))
(chaos-add-trigger #x51 'Dead '(chaos-flag text-1 'dead))
(chaos-add-trigger #x61 'HasItem '(chaos-has-item object text-1))
(chaos-add-trigger #xf00 'NumberOfTimesTalkedTo '(= (chaos-get-property this 'num-times-talked-to) integer-1))
