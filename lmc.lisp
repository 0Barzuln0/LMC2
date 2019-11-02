

;;; VALIDAZIONI SULLO STATE

;;; Funzioni accessorie a is-valid-state/1

(defun is-valid-mem-value (n) 
  (and (numberp n)
       (<= 0 n 999)))

;;; Un indice valido di cella di memoria e un valore valido per il pc
(defun is-valid-cell-value (n) 
  (and (numberp n)
       (<= 0 n 99)))

(defun list-has-valid-values (lista) 
  (if (null lista) 
      T
    (and (is-valid-mem-value (first lista))
	 (list-has-valid-values (rest lista)))))

(defun is-acc-valid (acc)
  (is-valid-mem-value acc))

(defun is-pc-valid (pc)
  (is-valid-cell-value pc))

(defun is-mem-valid (mem)
  (and (listp mem)
       (eq (list-length mem) 100)
       (list-has-valid-values mem)))

(defun is-in-valid (in) 
  (and (listp in)
       (list-has-valid-values in)))
  
(defun is-flag-valid (flag) 
  (or (equal flag 'flag)
      (equal flag 'noflag)))

(defun is-state-name-valid (state-name) 
  (or (equal state-name 'state) 
      (equal state-name 'halted-state)))

;;; is-valid-state/1
;;; Usata in one-instruction per assicurarsi che lo stato passato come 
;;; argomento della funzione sia valido
(defun is-valid-state (state)
  (and (listp state)
       (is-state-name-valid (nth 0 state)) 
       (is-acc-valid (nth 2 state))
       (is-pc-valid (nth 4 state))
       (and (listp (nth 6 state))
	    (eq (list-length (nth 6 state)) 100))
       (listp (nth 8 state))
       (listp (nth 10 state))
       (is-flag-valid (nth 12 state))))


;;; ISTRUZIONI LMC

;;; get-keyvalue/2
;;; Ricava il valore della proprietà chiave indicata nello stato passato
(defun get-keyvalue (state key)
  (getf (rest state) key))

;;; get-instr/1
;;; Estrae il numero dell'istruzione, contenuto nella cella di memoria 
;;; indicata nel program counter
(defun get-instr (state) 
  (nth (get-keyvalue state :pc) 
       (get-keyvalue state :mem)))

;;; lmc-add/1
;;; Esegue la somma tra il contenuto della cella di memoria xx con il valore 
;;; contenuto nell accumulatore
(defun lmc-add (state) 
  (+ (nth (mod (get-instr state) 100) 
	  (get-keyvalue state :mem))
     (get-keyvalue state :acc)))

;;; lmc-sub/1
;;; Esegue la sottrazione tra il valore contenuto nell accumulatore e 
;;; il contenuto della cella di memoria xx 
(defun lmc-sub (state) 
  (- (get-keyvalue state :acc) 
     (nth (mod (get-instr state) 100) 
	  (get-keyvalue state :mem))))

;;; one-instruction/1
;;; Esecuzione di una singola istruzione
;;; Restituisce il nuovo stato derivato dall'esecuzione dell'istruzione
;;; Fallisce se era già stato eseguita un'istruzione di halt,
;;; se l'istruzione è di input ma la lista di input è vuota, oppure se
;;; l'istruzione da eseguire non è valida
(defun one-instruction (state) 
  (if (and (is-valid-state state)
	   (equal (nth 0 state) 'state))
      (let* ((opcode (floor (get-instr state) 100))
	     (cellmem (mod (get-instr state) 100))
	     (value (nth cellmem (get-keyvalue state :mem))))
	(cond 
	 ;; HALT
	 ((eq opcode 0) 
	  (list 'halted-state 
		:acc (get-keyvalue state :acc)
		:pc (get-keyvalue state :pc)
		:mem (get-keyvalue state :mem)
		:in (get-keyvalue state :in)
		:out (get-keyvalue state :out)
		:flag (get-keyvalue state :flag)
		))
	 ;; ADD
	 ((eq opcode 1)
	  (if (is-valid-mem-value value)
	      (list 'state 
		    :acc (mod (lmc-add state) 1000)
		    :pc (mod (+ 1 (get-keyvalue state :pc)) 100)
		    :mem (get-keyvalue state :mem)
		    :in (get-keyvalue state :in)
		    :out (get-keyvalue state :out)
		    :flag (if (>= (lmc-add state) 1000)
			      'flag
			    'noflag)
		    )
	    NIL))
	 ;; SUB
	 ((eq opcode 2)
	  (if (is-valid-mem-value value)
	      (list 'state 
		    :acc (mod (lmc-sub state) 1000)
		    :pc (mod (+ 1 (get-keyvalue state :pc)) 100)
		    :mem (get-keyvalue state :mem)
		    :in (get-keyvalue state :in)
		    :out (get-keyvalue state :out)
		    :flag (if (< (lmc-sub state) 0)
			      'flag
			    'noflag)
		    )
	    NIL))
	 ;; STORE
	 ((eq opcode 3)
	  (setf (nth cellmem (get-keyvalue state :mem))
		(get-keyvalue state :acc))
	  (list 'state 
		:acc (get-keyvalue state :acc)
		:pc (mod (+ 1 (get-keyvalue state :pc)) 100)
		:mem (get-keyvalue state :mem)
		:in (get-keyvalue state :in)
		:out (get-keyvalue state :out)
		:flag (get-keyvalue state :flag)
		))
	 ;; LOAD
	 ((eq opcode 5) 
	  (if (is-valid-mem-value value)
	      (list 'state 
		    :acc (nth (mod (get-instr state) 100) 
			      (get-keyvalue state :mem))
		    :pc (mod (+ 1 (get-keyvalue state :pc)) 100)
		    :mem (get-keyvalue state :mem)
		    :in (get-keyvalue state :in)
		    :out (get-keyvalue state :out)
		    :flag (get-keyvalue state :flag)
		    )
	    NIL))
	 ;; BRANCH
	 ((eq opcode 6) 
	  (list 'state 
		:acc (get-keyvalue state :acc)
		:pc (mod (get-instr state) 100)
		:mem (get-keyvalue state :mem)
		:in (get-keyvalue state :in)
		:out (get-keyvalue state :out)
		:flag (get-keyvalue state :flag)
		))
	 ;; BRANCH IF ZERO
	 ((eq opcode 7) 
	  (list 'state 
		:acc (get-keyvalue state :acc)
		:pc (if (and (eq (get-keyvalue state :acc) 0)
			     (eq (get-keyvalue state :flag) 'noflag))
			(mod (get-instr state) 100)
		      (mod (+ 1 (get-keyvalue state :pc)) 100))
		:mem (get-keyvalue state :mem)
		:in (get-keyvalue state :in)
		:out (get-keyvalue state :out)
		:flag (get-keyvalue state :flag)
		))
	 ;; BRANCH IF POSITIVE
	 ((eq opcode 8) 
	  (list 'state 
		:acc (get-keyvalue state :acc)
		:pc (if (eq (get-keyvalue state :flag) 'noflag)
			(mod (get-instr state) 100)
		      (mod (+ 1 (get-keyvalue state :pc)) 100)) 
		:mem (get-keyvalue state :mem)
		:in (get-keyvalue state :in)
		:out (get-keyvalue state :out)
		:flag (get-keyvalue state :flag)
		))
	 ;; INPUT
	 ((and (eq (get-instr state) 901) 
	       (not (null (get-keyvalue state :in)))
	       (is-valid-mem-value (first (get-keyvalue state :in))))
	  (list 'state 
		:acc (first (get-keyvalue state :in))
		:pc (mod (+ 1 (get-keyvalue state :pc)) 100)
		:mem (get-keyvalue state :mem)
		:in (rest (get-keyvalue state :in))
		:out (get-keyvalue state :out)
		:flag (get-keyvalue state :flag)
		))
	 ;; OUTPUT
	 ((eq (get-instr state) 902) 
	  (list 'state 
		:acc (get-keyvalue state :acc)
		:pc (mod (+ 1 (get-keyvalue state :pc)) 100)
		:mem (get-keyvalue state :mem)
		:in (get-keyvalue state :in)
		:out (append (get-keyvalue state :out) 
			     (list (get-keyvalue state :acc)))
		:flag (get-keyvalue state :flag)
		))))
    NIL))

;;; execution-loop/1
;;; Esecuzione delle istruzioni in memoria.
;;; Produce il contenuto della coda di output dopo l’arresto del LMC.
;;; Fallisce se l’esecuzione termina senza eseguire una istruzione di halt.
(defun execution-loop (state) 
  (if (equal (nth 0 state) 'state)
      (execution-loop (one-instruction state))
    (get-keyvalue state :out)))

	
;;; PARSING ISTRUZIONI ASSEMBLY	

;;; Decodifica codice assembly
;;; Divisione degli elementi in istruzioni, commenti, valori, etichette

;;; is-instruction/1
;;; Se l'istruzione è valida, ne ritorna l'opcode
(defun is-instruction (word) 
  (if (stringp word)
      (let ((upword (string-upcase word)))
	(cond ((equal upword "ADD") 100)
	      ((equal upword "SUB") 200)
	      ((equal upword "STA") 300)
	      ((equal upword "LDA") 500)
	      ((equal upword "BRA") 600)
	      ((equal upword "BRZ") 700)
	      ((equal upword "BRP") 800)
	      ((equal upword "INP") 901)
	      ((equal upword "OUT") 902)
	      ((equal upword "HLT") 000)
	      ((equal upword "DAT") 000)))
    NIL))

(defun is-comment (word)
  (and (stringp word) 
       (> (length word) 1) 
       (equal (char word 0) #\/) 
       (equal (char word 1) #\/)))

;;; is-value/1
;;; Se il valore è valido, lo restituisce convertendolo da stringa a intero
(defun is-value (word)
  (if (and (stringp word)
	   (> (length word) 0)
	   (check-if-numbers (coerce word 'list))
	   (is-valid-mem-value (parse-integer word)))
      (parse-integer word) 
    NIL))

;;; check-if-numbers/1
;;; Funzione accessoria di is-value
;;; Controlla se tutti i caratteri di una stringa sono numeri
(defun check-if-numbers (lista-caratteri) 
  (cond
   ((null lista-caratteri)
    T)
   ;; se il carattere controllato è un valore non numerico fallisce
   ((null (digit-char-p (first lista-caratteri)))
    NIL) 
   ;; altrimenti continua a controllare
   (T
    (check-if-numbers (rest lista-caratteri)))))

(defun is-label (word) 
  (if (and (null (is-comment word))
	   (null (is-instruction word))
	   (null (is-value word)) 
	   (stringp word)
	   (> (length word) 0)
	   (alpha-char-p (char word 0)))
      T 
    NIL))


;;; RICONOSCIMENTO ISTRUZIONI ASSEMBLY VALIDE	

;;; decode-words/2
;;; Funzione accessoria di decodifica per read-words/2
;;; Risconosce le righe di codice assembly valide, una volta eliminati 
;;; i commenti. Se le righe non sono nelle forme specificata, fallisce.
(defun decode-words (words labels) 
  (cond
   ;; ISTRUZIONE CELLMEM (non DAT) O LABEL ISTRUZIONE CELLMEM (non DAT)
   ((and (<= 100
	     (is-instruction (first words))
	     899)
	 (numberp (is-value (second words)))				
	 (null (third words))) 						 	
    (if (is-valid-cell-value (is-value (second words)))
	(+ (is-instruction (first words))
	   (is-value (second words)))
      NIL))
   ;; ISTRUZIONE (DAT) CELLMEM - o LABEL ISTRUZIONE (DAT) CELLMEM
   ((and (is-value (second words))
	 (null (third words)))
    (+ (is-instruction (first words))
       (is-value (second words))))
   ;; ISTRUZIONE (INP/OUT/HLT/DAT) o LABEL ISTRUZIONE (INP/OUT/HLT/DAT)
   ((null (second words))
    (is-instruction (first words)))
   ;; ISTRUZIONE LABEL o LABEL ISTRUZIONE LABEL
   ((and (<= 100
	     (is-instruction (first words))
	     899)
	 (is-label (second words))
	 (null (third words))
	 (member (string-upcase (second words))
		 labels :test 'equal))
    ;; somma l'opcode dell'istruzione con il valore associato alla label
    (+ (nth (1+ (position (string-upcase (second words))
			  labels :test 'equal))
	    labels)
       (is-instruction (first words))))))

;;; read-words/2
;;; Riconosce le righe di codice assembly valide. 
;;; Il primo controllo è effettuato sulla prima parola della riga assembly,
;;; che può essere solo una instruction oppure una label.
;;; In caso contrario fallisce.
;;; E' chiamata ricorsivamente da read-lines/2
(defun read-words (words labels)
  (cond
   ;; La prima parola è una istruzione.
   ((numberp (is-instruction (first words))) 
    (decode-words words labels))
   ;; La prima parola è una label. Il controllo viene quindi effettuato
   ;; sulle parole successive alla prima.
   ((and (is-label (first words))
	 (numberp (is-instruction (second words))))
    (decode-words (rest words) labels))))


;;; CARICAMENTO IN MEMORIA DELLE ISTRUZIONI DI UN FILE

;;; remove-comment/1
;;; Riconoscimento e rimozione commenti dalle istruzioni assembly.
(defun remove-comment (line)
  (if (or (null (first line))
	  (is-comment (first line)))
      NIL
    (cons (first line)
	  (remove-comment (rest line)))))

;;; split-line/1
;;; Divide le righe assembly in singole parole.
;;; Funzione accessoria per save-labels e read-lines.
(defun split-line (line)
  (split-sequence '(#\Space #\Tab)
		  line
		  :coalesce-separators t))

;;; save-labels/2
;;; Salvataggio delle etichette dichiarate nel file assembly e associazione
;;; con il valore 
(defun save-labels (lines num-instr)
  (if (< num-instr 101)
      (let ((clean-line (remove-comment (split-line (first lines)))))
	(cond
	 ((null lines)
	  NIL)
	 ;; RIGA VUOTA O COMMENTO
	 ((or (null (first clean-line))
	      (equal (first clean-line)
		     ""))
	  (save-labels (rest lines) num-instr))
	 ;; NO DICHIARAZIONE LABEL
	 ((not (is-label (first clean-line)))
	  (save-labels (rest lines) (+ 1 num-instr)))
	 ;; DICHIARAZIONE LABEL
	 (T 
	  (let ((etichetta (string-upcase (first clean-line)))
		(etichette (save-labels (rest lines) (+ 1 num-instr))))
	    (if (not (member etichetta etichette :test 'equal))
		(append (list etichetta
			      num-instr)
			etichette)
	      (cons 'error
		    nil))))))
    (cons 'error
	  nil)))

;;; read-lines/2
;;; Rimuove ricorsivamente commenti dal codice assembly riga per riga
;;; e ignora nella decodifica con decode-lines/1 righe vuote.
(defun read-lines (lines labels)
  (let* ((clean-line (remove-comment (split-line (first lines))))
	 (instr (read-words clean-line labels)))
    (cond
     ((null lines) NIL)
     ;; RIGA VUOTA O COMMENTO
     ((or (null clean-line)
	  (equal (first clean-line)
		 ""))
      (read-lines (rest lines) labels))
     ;; ISTRUZIONE
     ((not (null instr))
      (cons instr
	    (read-lines (rest lines) labels)))
     (T
      (cons 'error
		nil)))))

;;; decode-lines/1
;;; Funzione accessoria per read-lines/2
(defun decode-lines (lines)
  (let* ((etichette (save-labels lines 0))
	 (instrs (read-lines lines (save-labels lines 0))))
    (if (or (equal (car (last etichette)) 'error)
	    (equal (car (last instrs)) 'error))
	'error
      instrs)))

;;; fill-memory/1
;;; Se le istruzioni da caricare nella memoria sono in numero minore a 100,
;;; finisce di riempire la memoria con istruzioni di halt.
(defun fill-memory (instrs)
  (if (< (list-length instrs) 100)
      (append instrs
	      (make-list (- 100
			    (list-length instrs))
			 :initial-element 0))
    instrs))

;;; read-file/1	
;;; Ottiene dalla lettura del file una lista di stringhe che rappresentano 
;;; le righe di codice assembly.
(defun read-file (file-name)
  (with-open-file (in file-name
		      :direction :input
		      :if-does-not-exist :error)
		  (read-lines-from-stream in)))

;;; read-lines-from-stream/1
;;; Funzione accessoria per read-file/1.
(defun read-lines-from-stream (input-stream)
  (let ((line (read-line input-stream
			 NIL
			 'eof
			 NIL)))
    (unless (eq line 'eof)
      (cons line
	    (read-lines-from-stream input-stream)))))

;;; lmc-load/1
;;; Leggere un file che contiene un codice assembly e che produce il 
;;; contenuto iniziale della memoria del sistema
(defun lmc-load (filename)
  (let ((instrs (decode-lines (read-file filename))))
    (if (equal instrs 'error)
		NIL
      (fill-memory instrs))))

;;; lmc-run/2
;;; Legge un file che contiene codice assembly, lo carica con lmc-load e
;;; inizializza la coda di input al valore fornito.
;;; Produce un output che è il risultato dell’invocazione di execution-loop.
(defun lmc-run (filename in)
  (if (is-in-valid in)
      (execution-loop (list 'state
			    :acc 0
			    :pc 0
			    :mem (lmc-load filename)
			    :in in
			    :out '()
			    :flag 'noflag))
    NIL))
