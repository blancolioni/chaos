(chaos-add-action 0 'no-action #f)
(chaos-add-action 3 'Attack '(chaos-attack this object-1))
(chaos-add-action 7 'create-creature '(chaos-create-actor this text-1 x y integer-1))
(chaos-add-action 15 'GiveItem '(chaos-give-item this object-1 object-2))
(chaos-add-action 30 'SetGlobal '(chaos-set-property text-2 text-1 integer-1))
(chaos-add-action 31 'Spell '(chaos-cast-spell this object-1 integer-1))
(chaos-add-action 36 'continue #f)
(chaos-add-action 61 'start-timer '(chaos-set-timer this integer-1 integer-2))
(chaos-add-action 115 'set-global-timer '(chaos-set-timer text-1 integer-1))
(chaos-add-action 116 'TakePartyItem '(chaos-take-party-item this text-1))
(chaos-add-action 151 'display-string '(ui-display-string integer-1))
(chaos-add-action 161 'increment-chapter '(chaos-set-property global 'chapter (+ 1 (chaos-get-property global 'chapter))))
(chaos-add-action 164 'AddExperienceParty '(chaos-party-experience integer-1))
(chaos-add-action 177 'trigger-activation '(chaos-set-trigger this object-1 integer-1))
