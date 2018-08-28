(defun decode (binary huffman-tree)
  (labels ((symbol-decoder (bits current-branch)
	   (if (not(null bits))
	     (if (or (= (first bits) 0) (= (first bits) 1))
       	       (if (leaf-p (next-decoding-branch (first bits) current-branch))
	       	   (cons (first(first(first (next-decoding-branch (first bits) current-branch)))) 
			 (symbol-decoder (rest bits) huffman-tree)) 
		   (symbol-decoder (rest bits) (next-decoding-branch (first bits) current-branch)))
	       (error "ONLY BINARY NUMBER ARE ALLOWED FOR DECODING!!!")))))
    (symbol-decoder binary huffman-tree)))

;;;==========================================================
(defun decody (binary huffman-tree)
  (labels ((symbol-decoder (bits current-branch)
	     (if (not (null bits))
		 (cond ((or (= (first bits) 0) (= (first bits) 1))
			(cond ((leaf-p (next-decoding-branch (first bits) current-branch))
			       (cons (first(first(first (next-decoding-branch (first bits) current-branch))))
				     (symbol-decoder (rest bits) huffman-tree)))
			      (t (symbol-decoder (rest bits) (next-decoding-branch (first bits) current-branch)))))
		       (t (error "ONLY BINARY NUMBER ARE ALLOWED FOR DECODING!!!"))))))
    (symbol-decoder binary huffman-tree)))
