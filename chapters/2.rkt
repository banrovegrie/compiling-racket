;; Here, we shall discuss the following passes:
;;      - uniquify (rename)
;;      - remove complex operations (use let bindings)
;;      - explicate control ()
;;
;; Explicate control results in a new language called C0.

;; Grammar for remove complex operations:
;;      atm := vat | int
;;      exp := atm | (read) | (-atm ) | (+ atm atm)
;;              | (let ([var exp]) exp)
;;      R1 := exp

;; 
