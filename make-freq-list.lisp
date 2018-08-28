;;;  GENERATE THE FREQUENCY LIST FROM THE MESSAGE

;;;========================
(defun freqlist (message)
  (fl message nil))

;;;=========================
(defun fl (message pflist)
  (cond ((endp message) pflist)
	(t (fl (rest message)(update (first message) pflist)))))

;;;========================
(defun update (word fl)
  (cond ((endp fl) (list(list(list word) 1)))
	((fequal word (first(first fl))) (cons (incpair(first fl))(rest fl)))
	( t (cons (first fl)(update word (rest fl))))))

;;;=========================
(defun fequal (word pair) (equal word (first pair)))

;;;=========================
(defun incpair (pair) (list (first pair)(1+ (second pair))))





