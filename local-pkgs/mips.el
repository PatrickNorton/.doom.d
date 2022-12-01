;;; local-pkgs/mips.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eieio)
(require 'thingatpt)

(defconst mips-word-size 32)
(defconst mips-word-bytes 4)

(defun mips-wrap-unsigned (val)
  (logand val #xffffffff))

(defun mips-wrap-signed (val)
  (let ((val (mips-wrap-unsigned val)))
    (if (zerop (logand val #x80000000))
        val
      (- val #x100000000))))

(defun mips-trap-signed (val)
  (if (or (>= val #x80000000) (< val (- #x80000000)))
      (signal 'mips-overflow (list val))
    val))

(define-error 'mips-overflow "Signed overflow")

(define-error 'mips-parse-error "Invalid MIPS syntax")

(define-error 'mips-invalid-addr "Address out of range")

(define-error 'mips-unaligned-addr "Unaligned address")

(defun mips-parse-error (&rest msgs)
  (signal 'mips-parse-error (if msgs (list (apply #'format msgs)) nil)))

(defun mips-op (env opcode args)
  (funcall (assoc opcode mips-instruction-table) env args))

(defun mips-todo (&rest args)
  (error "Still not done: %s" (apply #'format args)))

(defun mips-assert-aligned (addr alignment)
  (unless (zerop (mod addr alignment))
    (signal 'mips-unaligned-addr (list addr alignment))))

(defun mips-init-registers ()
  (let ((regs (make-vector 32 0)))
    (setf (aref regs mips-sp) mips-stack-high)
    (setf (aref regs mips-ra) (mips-wrap-unsigned -1))
    regs))

(defclass mips-env ()
  ((code :initarg :code
         :type array)
   (registers :initarg :registers
              :type array)
   (stack :initarg :stack
          :initform nil
          :type list)
   (stack-start :initarg :stack-start
                :type number)
   (heap :initarg :heap)
   (ip :initarg :ip
       :type (number -1 *))
   (high :initform 0
         :type (number 0 #xffffffff))
   (low :initform 0
        :type (number 0 #xffffffff))
   (buffer :initarg :buffer
           :type buffer)
   (io-buf :initarg :io-buf
           :type buffer)
   (labels :initarg :labels
           :type list)
   (breakpoints :type list
                :initform nil)
   (static-data :initarg :static-data
                :type array)))

(defun mips-env-new (code start buffer labels)
  (mips-env :code code
            :registers (mips-init-registers)
            :stack-start mips-stack-high
            :heap (make-hash-table)
            :ip start
            :buffer buffer
            :io-buf (generate-new-buffer "mips-io")
            :labels labels))

(cl-defmethod mips-env-cleanup ((obj mips-env))
  (kill-buffer (slot-value obj 'io-buf)))

(cl-defmethod get-reg ((obj mips-env) register &optional signed)
  (let ((reg-val (aref (slot-value obj 'registers) register)))
    (if (and signed (>= reg-val #x80000000))
        (- reg-val #x100000000)
      reg-val)))

(cl-defmethod set-reg ((obj mips-env) register value)
  (cl-assert (not (zerop register)))
  (aset (slot-value obj 'registers) register (mips-wrap-unsigned value)))

(cl-defmethod reset-mips-state ((obj mips-env))
  (setf (slot-value obj 'registers) (mips-init-registers))
  (setf (slot-value obj 'stack) nil)
  (setf (slot-value obj 'stack-start) mips-stack-high)
  (setf (slot-value obj 'heap) (make-hash-table))
  (setf (slot-value obj 'ip) (1+ (cdr (assoc "main" (slot-value obj 'labels)))))
  (setf (slot-value obj 'high) 0)
  (setf (slot-value obj 'low) 0)
  (with-current-buffer (slot-value obj 'io-buf)
    (erase-buffer)))

(gv-define-setter get-reg (value obj register &optional _signed)
  `(set-reg ,obj ,register ,value))

(defun zero-byte (val)
  (cond
   ((zerop (logand #xff000000 val)) 0)
   ((zerop (logand #x00ff0000 val)) 1)
   ((zerop (logand #x0000ff00 val)) 2)
   ((zerop (logand #x000000ff val)) 3)
   (t nil)))

(defun trunc-zero (val null-term)
  (if val
      (pcase (if null-term (1+ val) val)
        (0 "")
        (1 (string (ash (logand #xff000000) 24)))
        (2 (string
            (ash (logand #xff000000) 24)
            (ash (logand #x00ff0000) 16)))
        (3 (string
            (ash (logand #xff000000) 24)
            (ash (logand #x00ff0000) 16)
            (ash (logand #x0000ff00) 8)))
        (4 (string
            (ash (logand #xff000000) 24)
            (ash (logand #x00ff0000) 16)
            (ash (logand #x0000ff00) 8)
            (logand #x000000ff)))
        (5 (string
            (ash (logand #xff000000) 24)
            (ash (logand #x00ff0000) 16)
            (ash (logand #x0000ff00) 8)
            (logand #x000000ff)
            ?\x00)))
    (string
     (ash (logand #xff000000) 24)
     (ash (logand #x00ff0000) 16)
     (ash (logand #x0000ff00) 8)
     (logand #x000000ff))))

(cl-defmethod load-string ((obj mips-env) addr &optional null-term)
  (if (mips-stackp addr)
      (let ((stack (slot-value obj 'stack))
            (stack-index (/ (- addr stack-start) mips-word-bytes)))
        (let ((str (cl-loop for f in (nthcdr stack-index stack)
                            for zloc = (zero-byte f)
                            concat (trunc-zero f null-term)
                            until zloc)))
          str))))

(cl-defmethod load-word ((obj mips-env) addr)
  (mips-assert-aligned addr mips-word-bytes)
  (cond
   ((mips-stackp addr)
    (let ((stack (slot-value obj 'stack))
          (stack-index (/ (- addr (slot-value obj 'stack-start))
                          mips-word-bytes)))
      (or (nth stack-index stack)
          (signal 'mips-invalid-addr addr))))
   ((mips-static-data-p addr)
    (let ((data (slot-value obj 'static-data))
          (data-index (- addr mips-stack-high)))
      (condition-case nil
          (aref data data-index)
        (args-out-of-range (signal 'mips-invalid-addr (list addr))))))
   (t (mips-todo))))

(cl-defmethod store-word ((obj mips-env) addr value)
  (mips-assert-aligned addr mips-word-bytes)
  (cond
   ((mips-stackp addr)
    (let ((stack (slot-value obj 'stack))
          (stack-index (/ (- addr (slot-value obj 'stack-start))
                          mips-word-bytes)))
      (if (>= stack-index 0)
          (setf (nth stack-index stack) (mips-wrap-unsigned value))
        (dotimes (_ (abs stack-index))
          (push 0 (slot-value obj 'stack))
          (cl-decf (slot-value obj 'stack-start) mips-word-bytes))
        (setf (nth 0 (slot-value obj 'stack)) (mips-wrap-unsigned value)))))
   ((mips-static-data-p addr)
    (signal 'mips-invalid-addr (list addr "read-only")))
   (t (mips-todo "Not in stack: %08x" addr))))

(cl-defmethod add-breakpoint ((obj mips-env) line)
  (push line (slot-value obj 'breakpoints)))

(cl-defmethod current-command ((obj mips-env))
  (condition-case nil
      (aref (slot-value obj 'code) (slot-value obj 'ip))
    (args-out-of-range (mips--throw-terminate))))

(defun mips-stackp (addr)
  (and (>= addr mips-stack-low)
       (< addr mips-stack-high))) ;; TODO: Actually make heap addresses viable

(defun mips-static-data-p (addr)
  (>= addr mips-stack-high))

(defconst mips-stack-high #x02000000)
(defconst mips-stack-low #x01000000)

(defconst mips-instruction-table
  '(("special" . mips-todo)
    ("bcond" . mips-todo)
    ("j" . mips-j)
    ("jal" . mips-jal)
    ("beq" . mips-beq)
    ("bne" . mips-bne)
    ("blez" . mips-blez)
    ("bgtz" . mips-bgtz)
    ("addi" . mips-addi)
    ("addiu" . mips-addiu)
    ("slti" . mips-slti)
    ("sltiu" . mips-sltiu)
    ("andi" . mips-andi)
    ("ori" . mips-ori)
    ("xori" . mips-xori)
    ("lui" . mips-lui)
    ("cop0" . mips-todo)
    ("cop1" . mips-todo)
    ("cop2" . mips-todo)
    ("cop3" . mips-todo)
    ("lb" . mips-lb)
    ("lh" . mips-lh)
    ("lwl" . mips-todo)
    ("lw" . mips-lw)
    ("lbu" . mips-lbu)
    ("lhu" . mips-lhu)
    ("lwr" . mips-todo)
    ("sb" . mips-todo)
    ("sh" . mips-todo)
    ("swl" . mips-todo)
    ("sw" . mips-sw)
    ("swr" . mips-todo)
    ("lwc0" . mips-todo)
    ("lwc1" . mips-todo)
    ("lwc2" . mips-todo)
    ("lwc3" . mips-todo)
    ("swc0" . mips-todo)
    ("swc1" . mips-todo)
    ("swc2" . mips-todo)
    ("swc3" . mips-todo)

    ("sll" . mips-sll)
    ("srl" . mips-srl)
    ("sra" . mips-sra)
    ("sllv" . mips-todo)
    ("srlv" . mips-todo)
    ("srav" . mips-todo)
    ("jr" . mips-jr)
    ("jalr" . mips-jalr)
    ("syscall" . mips-syscall)
    ("break" . mips-break)
    ("mfhi" . mips-mfhi)
    ("mflo" . mips-mflo)
    ("mtlo" . mips-mtlo)
    ("mult" . mips-mult)
    ("multu" . mips-multu)
    ("div" . mips-div)
    ("divu" . mips-divu)
    ("add" . mips-add)
    ("addu" . mips-addu)
    ("sub" . mips-sub)
    ("subu" . mips-subu)
    ("and" . mips-and)
    ("or" . mips-or)
    ("xor" . mips-xor)
    ("nor" . mips-nor)
    ("slt" . mips-slt)
    ("sltu" . mips-sltu)

    ("abs" . mips-abs)
    ("bgt" . mips-bgt)
    ("bge" . mips-bge)
    ("blt" . mips-blt)
    ("ble" . mips-ble)
    ("li" . mips-li)
    ("la" . mips-la)
    ("move" . mips-move)
    ("sge" . mips-sge)
    ("sgt" . mips-sgt)))

(defun mips-j (env args)
  (setf (slot-value env 'ip) (nth 0 args)))

(defun mips-jal (env args)
  (set-reg env mips-ra (slot-value env 'ip))
  (setf (slot-value env 'ip) (nth 0 args)))

(defmacro mips-branch-fn (name test)
  `(defun ,name (env args)
     (when (,test (get-reg env (nth 0 args))
                  (get-reg env (nth 1 args)))
       (setf (slot-value env 'ip)
             ;; FIXME? Should this be a relative or absolute jump?
             (nth 2 args)))))

(mips-branch-fn mips-beq =)
(mips-branch-fn mips-bne /=)
(mips-branch-fn mips-bgt >)
(mips-branch-fn mips-bge >=)
(mips-branch-fn mips-blt <)
(mips-branch-fn mips-ble <=)

(defun mips-blez (env args)
  (when (<= (get-reg env (nth 0 args) t) 0)
    (setf (slot-value env 'ip) (nth 1 args))))

(defun mips-bgtz (env args)
  (when (> (get-reg env (nth 0 args) t) 0)
    (setf (slot-value env 'ip) (nth 1 args))))

(defun mips-dup-first-arg (args)
  (if (= (length args) 2)
      (list (nth 0 args) (nth 0 args) (nth 1 args))
    args))

(defmacro mips-immu-fn (name fn)
  `(defun ,name (env args)
     (let ((args (mips-dup-first-arg args)))
       (set-reg env (nth 0 args)
                (,fn (get-reg env (nth 1 args))
                     (nth 2 args))))))

(defun mips-addi (env args)
  (let ((args (mips-dup-first-arg args)))
    (set-reg env (nth 0 args)
             (mips-trap-signed (+ (get-reg env (nth 1 args) t)
                                  (nth 2 args))))))

(mips-immu-fn mips-addiu +)

(defun mips-slti* (env args signedp)
  (if (< (get-reg env (nth 1 args) signedp)
         (nth 2 args))
      (set-reg env (nth 0 args) 1)
    (set-reg env (nth 0 args) 0)))

(defun mips-slti (env args)
  (mips-slti* env args t))

(defun mips-sltiu (env args)
  (mips-slti* env args nil))

(mips-immu-fn mips-andi logand)
(mips-immu-fn mips-ori logior)
(mips-immu-fn mips-xori logxor)

(defun mips-lui (env args)
  (set-reg env (nth 0 args)
           (ash (nth 1 args) 16)))

(defun mips-lb* (env args signedp)
  (set-reg env (nth 0 args)
           (let* ((addr (+ (nth 1 args) (get-reg env (nth 2 args))))
                  (addr-rem (mod addr mips-word-bytes))
                  (rounded-addr (- addr addr-rem))
                  (word (load-word env rounded-addr))
                  (by (logand word (ash #xff addr-rem))))
             ;; Sign-extension
             (if (or (not signedp) (zerop (logand by #x80)))
                 by
               (logior by #xffffff00)))))

(defun mips-lh* (env args signedp)
  (set-reg env (nth 0 args)
           (let* ((addr (+ (nth 1 args) (get-reg env (nth 2 args))))
                  (word (load-word env addr))
                  (val (logand word #xffff)))
             ;; Sign-extension
             (if (or (not signedp) (zerop (logand val #x8000)))
                 val
               (logior val #xffff0000)))))

(defun mips-lw (env args)
  (set-reg env (nth 0 args)
           (load-word env (+ (nth 1 args) (get-reg env (nth 2 args))))))

(defun mips-lb (env args)
  (mips-lb* env args t))

(defun mips-lbu (env args)
  (mips-lb* env args nil))

(defun mips-lh (env args)
  (mips-lh* env args t))

(defun mips-lhu (env args)
  (mips-lh* env args nil))

(defun mips-sw (env args)
  (store-word env (+ (nth 1 args) (get-reg env (nth 2 args)))
              (get-reg env (nth 0 args))))

(defun mips-sll (env args)
  (set-reg env (nth 0 args)
           (ash (get-reg env (nth 1 args)) (nth 2 args))))

(defun mips-srl (env args)
  (set-reg env (nth 0 args)
           (ash (get-reg env (nth 1 args)) (- (nth 2 args)))))

(defun mips-sra (env args)
  (set-reg env (nth 0 args)
           (ash (get-reg env (nth 1 args) t) (- (nth 2 args)))))

(defun mips-jr (env args)
  (setf (slot-value env 'ip) (get-reg env (nth 0 args))))

(defun mips-jalr (env args)
  (set-reg env mips-ra (1+ (slot-value env 'ip)))
  (setf (slot-value env 'ip) (get-reg env (nth 0 args))))

(defun mips-break (_env _args)
  (mips--throw-terminate))

(defun mips-mfhi (env args)
  (set-reg env (nth 0 args) (slot-value env 'high)))

(defun mips-mflo (env args)
  (set-reg env (nth 0 args) (slot-value env 'low)))

(defun mips-mtlo (env args)
  (setf (slot-value env 'low) (get-reg env (nth 0 args))))

(defun mips-mult* (env args signedp)
  (let ((prod (* (get-reg env (nth 0 args) signedp)
                 (get-reg env (nth 1 args) signedp))))
    (setf (slot-value env 'low) (logand prod #xffffffff))
    (setf (slot-value env 'high)
          (ash (logand prod #xffffffff00000000) mips-word-size))))

(defun mips-mult (env args)
  (mips-mult* env args t))

(defun mips-multu (env args)
  (mips-mult* env args nil))

(defun mips-div (env args)
  (pcase-let* ((dividend (get-reg env (nth 0 args) t))
               (divisor (get-reg env (nth 1 args) t))
               (`(,quot ,rem) (cl-truncate dividend divisor)))
    (setf (slot-value env 'low) (mips-wrap-unsigned quot))
    (setf (slot-value env 'high) (mips-wrap-unsigned rem))))

(defun mips-divu (env args)
  (pcase-let* ((dividend (get-reg env (nth 0 args)))
               (divisor (get-reg env (nth 1 args)))
               (`(,quot ,rem) (cl-floor dividend divisor)))
    (setf (slot-value env 'low) (mips-wrap-unsigned quot))
    (setf (slot-value env 'high) (mips-wrap-unsigned rem))))

(defmacro mips-unsigned (name op)
  `(defun ,name (env args)
     (set-reg env (nth 0 args)
              (,op (get-reg env (nth 1 args))
                   (get-reg env (nth 2 args))))))

(defun mips-add (env args)
  (set-reg env (nth 0 args)
           (mips-trap-signed
            (+ (get-reg env (nth 1 args) t)
               (get-reg env (nth 2 args) t)))))

(mips-unsigned mips-addu +)

(defun mips-sub (env args)
  (set-reg env (nth 0 args)
           (mips-trap-signed
            (- (get-reg env (nth 1 args) t)
               (get-reg env (nth 2 args) t)))))

(mips-unsigned mips-subu +)

(mips-unsigned mips-and logand)
(mips-unsigned mips-or logior)
(mips-unsigned mips-xor logxor)

(defun lognor (a b)
  (lognot (logior a b)))

(mips-unsigned mips-nor lognor)

(defun mips-scmp (env args cmp signedp)
  (set-reg env (nth 0 args)
           (if (funcall cmp
                        (get-reg env (nth 1 args) signedp)
                        (get-reg env (nth 2 args) signedp))
               1
             0)))

(defun mips-slt (env args)
  (mips-scmp env args #'< t))

(defun mips-sltu (env args)
  (mips-scmp env args #'< nil))

(defun mips-abs (env args)
  (set-reg env (nth 0 args)
           (abs (get-reg env (nth 1 args) t))))

(defun mips-li (env args)
  (set-reg env (nth 0 args) (nth 1 args)))

(defun mips-la (env args)
  (set-reg env (nth 0 args) (nth 1 args)))

(defun mips-move (env args)
  (set-reg env (nth 0 args) (get-reg env (nth 1 args))))

(defun mips-sge (env args)
  (mips-scmp env args #'>= t))

(defun mips-sgt (env args)
  (mips-scmp env args #'> t))

(defun mips-syscall (env args)
  (cl-assert (null args))
  (pcase (get-reg env mips-v0)
    (1 (mips-print-int env))
    (2 (mips-todo)) ;; print float
    (3 (mips-todo)) ;; print double
    (4 (mips-print-str env))
    (5 (mips-input-int env))
    (6 (mips-todo)) ;; input float
    (7 (mips-todo)) ;; input double
    (8 (mips-input-str env))
    (9 (mips-todo)) ;; sbrk
    (10 (mips--throw-terminate))
    (11 (mips-print-char env))
    (12 (mips-input-char env))
    (_ (mips-todo))))

(defun mips-print-int (env)
  (with-current-buffer (slot-value env 'io-buf)
    (insert (number-to-string (get-reg env mips-a0)))))

(defun mips-input-int (env)
  (with-current-buffer (slot-value env 'buffer)
    (mips-write-output env)
    (let ((input (read-number "Input int: ")))
      (set-reg env mips-v0 input))))

(defun mips-print-str (env)
  (with-current-buffer (slot-value env 'io-buf)
    (insert (load-string env (get-reg env mips-a0)))))

(defun mips-input-str (env)
  (with-current-buffer (slot-value env 'buffer)
    (mips-write-output env)
    (let ((input (read-string "Input string: ")))
      (set-reg env mips-v0 input))))

(defun mips-print-char (env)
  (with-current-buffer (slot-value env 'io-buf)
    (insert (logand #xff (get-reg env mips-a0)))))

(defun mips-input-char (env)
  (with-current-buffer (slot-value env 'buffer)
    (mips-write-output env)
    (let ((input (read-char "Input char: ")))
      (set-reg env mips-v0 input))))

(defun mips-register-from-name (name)
  (pcase name
    ((or "$0" "$zero") 0)
    ("$at" 1)
    ("$v0" 2)
    ("$v1" 3)
    ("$a0" 4)
    ("$a1" 5)
    ("$a2" 6)
    ("$a3" 7)
    ("$t0" 8)
    ("$t1" 9)
    ("$t2" 10)
    ("$t3" 11)
    ("$t4" 12)
    ("$t5" 13)
    ("$t6" 14)
    ("$t7" 15)
    ("$s0" 16)
    ("$s1" 17)
    ("$s2" 18)
    ("$s3" 19)
    ("$s4" 20)
    ("$s5" 21)
    ("$s6" 22)
    ("$s7" 23)
    ("$t8" 24)
    ("$t9" 25)
    ("$k0" 26)
    ("$k1" 27)
    ("$gp" 28)
    ("$sp" 29)
    ("$fp" 30)
    ("$ra" 31)
    ;; Matches $r0..$r31
    ((rx "$r" (let reg (* digit)))
     (if (<= 0 (string-to-number reg) 31)
         (string-to-number reg)
       (mips-parse-error "Unknown register %s" name)))
    (x (mips-parse-error "Unknown register %s" x))))

(defconst mips-a0 (mips-register-from-name "$a0"))
(defconst mips-v0 (mips-register-from-name "$v0"))
(defconst mips-sp (mips-register-from-name "$sp"))
(defconst mips-fp (mips-register-from-name "$fp"))
(defconst mips-ra (mips-register-from-name "$ra"))

(defun mips-colno ()
  (- (point) (bounds-of-thing-at-point 'line)))

;; FIXME: Parse static data (.ascii, .asciz, .quad, etc.)
(defun mips-parse-buf (buffer)
  (with-current-buffer buffer
    (let ((temp-syntax-table (make-syntax-table asm-mode-syntax-table)))
      (modify-syntax-entry ?\$ "w" temp-syntax-table)
      (modify-syntax-entry ?_ "w" temp-syntax-table)
      (modify-syntax-entry ?# "<" temp-syntax-table)
      (modify-syntax-entry ?\n ">" temp-syntax-table)
      (with-syntax-table temp-syntax-table
        (goto-char (point-min))
        (let* ((labels nil)
               (instrs (cl-loop with i = 0
                                for (instr . label) = (mips-parse-line)
                                do (forward-line)
                                when label
                                ;; TODO: Use map-put! here
                                do (setf labels (map-insert labels label (1- i)))
                                when instr
                                collect instr
                                and do (cl-incf i)
                                until (eobp))))
          (list
           (apply #'vector (mapcar (lambda (instr) (mips-link-labels instr labels)) instrs))
           (1+ (cdr (assoc "main" labels)))
           labels))))))

(defconst mips-instr-format
  '(("special" . ())
    ("bcond" . ())
    ("j" . (label))
    ("jal" . (label))
    ("beq" . (register register label))
    ("bne" . (register register label))
    ("blez" . (register label))
    ("bgtz" . (register label))
    ("addi" . (register opt-reg immediate))
    ("addiu" . (register opt-reg u-imm))
    ("slti" . (register register immediate))
    ("sltiu" . (register register u-imm))
    ("andi" . (register opt-reg immediate))
    ("ori" . (register opt-reg immediate))
    ("xori" . (register opt-reg immediate))
    ("lui" . (register immediate))
    ("cop0" . ())
    ("cop1" . ())
    ("cop2" . ())
    ("cop3" . ())
    ("lb" . (register offset))
    ("lh" . (register offset))
    ("lwl" . (register offset))
    ("lw" . (register offset))
    ("lbu" . (register offset))
    ("lhu" . (register offset))
    ("lwr" . (register offset))
    ("sb" . (register offset))
    ("sh" . (register offset))
    ("swl" . (register offset))
    ("sw" . (register offset))
    ("swr" . (register offset))
    ("lwc0" . ())
    ("lwc1" . ())
    ("lwc2" . ())
    ("lwc3" . ())
    ("swc0" . ())
    ("swc1" . ())
    ("swc2" . ())
    ("swc3" . ())

    ("sll" . (register register u-imm))
    ("srl" . (register register u-imm))
    ("sra" . (register register u-imm))
    ("sllv" . (register register register))
    ("srlv" . (register register register))
    ("srav" . (register register register))
    ("jr" . (register))
    ("jalr" . (register))
    ("syscall" . ())
    ("break" . ())
    ("mfhi" . (register))
    ("mflo" . (register))
    ("mtlo" . (register))
    ("mult" . (register register))
    ("multu" . (register register))
    ("div" . (register register))
    ("divu" . (register register))
    ("add" . (register register register))
    ("addu" . (register register register))
    ("sub" . (register register register))
    ("subu" . (register register register))
    ("and" . (register register register))
    ("or" . (register register register))
    ("xor" . (register register register))
    ("nor" . (register register register))
    ("slt" . (register register register))
    ("sltu" . (register register register))

    ("abs" . (register register))
    ("bgt" . (register register label))
    ("bge" . (register register label))
    ("blt" . (register register label))
    ("ble" . (register register label))
    ("li" . (register immediate))
    ("la" . (register label))
    ("move" . (register register))
    ("sge" . (register register register))
    ("sgt" . (register register register))))

(defconst mips-label-regex (rx (group (* (or word ?.))) ?:))

(defun mips-skip-whitespace (remaining)
  (when (and remaining (eolp))
    (mips-parse-error "Unexpected end of line"))
  (when (seq-contains-p " \t" (char-after))
    (forward-whitespace 1)
    (forward-comment 1)))

(defun mips-regex-search-end ()
  (save-excursion
    (let* ((line-end (cdr (bounds-of-thing-at-point 'line)))
           (hashtag (search-forward "#" line-end t)))
      (or hashtag line-end))))

(defun mips-parse-line ()
  (save-excursion
    (back-to-indentation)
    (if (or (eolp)
            (eq ?. (char-after))
            (eq ?# (char-after)))
        (cons nil nil)
      (if (looking-at mips-label-regex)
          (cons nil (substring-no-properties (match-string 1)))
        (let* ((instruction (mips-read-word))
               (syntax (alist-get instruction mips-instr-format 'not-found nil #'string=)))
          (when (null instruction)
            (mips-parse-error "Instruction not found on line %d" (line-number-at-pos)))
          (when (eq syntax 'not-found)
            (mips-parse-error "Instruction not found: %s (line %d)"
                              instruction (line-number-at-pos)))
          (mips-skip-whitespace syntax)
          (cons
           (cons (substring-no-properties instruction)
                 (cl-loop for (reg . remaining) on syntax
                          append (mips-parse-syntax reg)
                          when (and remaining (eq ?, (char-after)))
                          do (forward-char)
                          do (mips-skip-whitespace remaining)))
           nil))))))

(defun mips-read-int ()
  (let ((start (point)))
    (when (eq ?- (char-after))
      (forward-char)
      (setf negative t))
    (while (<= ?0 (char-after) ?9)
      (forward-char))
    (if (= start (point))
        (mips-parse-error "Expected number, got %c" (char-after))
      (string-to-number (buffer-substring-no-properties start (point))))))

(defun mips-read-word ()
  (if (looking-at (rx (+ word)))
      (progn
        (goto-char (match-end 0))
        (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
    (mips-parse-error "Expected word")))

(defun mips-parse-syntax (syntax)
  (pcase syntax
    ('register (list (mips-register-from-name (mips-read-word))))
    ('immediate (list (mips-wrap-signed (mips-read-int))))
    ('u-imm (list (mips-wrap-unsigned (mips-read-int))))
    ('offset (let ((offset (mips-wrap-unsigned (mips-read-int))))
               (if (eq ?\( (char-after))
                   (forward-char)
                 (mips-parse-error "Expected open-paren"))
               (let ((register (mips-register-from-name (mips-read-word))))
                 (if (eq ?\) (char-after))
                         (forward-char)
                   (mips-parse-error "Expected close-paren"))
                 (list offset register))))
    ('label (prog1
                (list (thing-at-point 'word))
              (end-of-thing 'word)))
    ('opt-reg (if (eq ?$ (char-after))
                  (list (mips-register-from-name (mips-read-word)))
                nil))
    (x (mips-parse-error "Unknown syntax %s" x))))

(defun mips-link-labels (instr labels)
  (cons (car instr)
        (mapcar (lambda (arg)
                  (if (stringp arg)
                      (cdr (assoc arg labels))
                    arg))
                (cdr instr))))

(defun mips--fmt-instruction ()
  (pcase (if (boundp 'mips-format-type)
             mips-format-type
           nil)
    ('hex "0x%08x")
    ('upper-hex "0X%08X")
    ('decimal "%10d")
    ((pred stringp) mips-format-type)
    (_ (message "Unknown or unspecified MIPS number formatting option.")
       "0x%08x")))

(defun mips--fmt-registers (env)
  (let ((fmt (mips--fmt-instruction)))
    (apply #'format "$zero  %1$s        $s0    %17$s
$at    %2$s        $s1    %18$s
$v0    %3$s        $s2    %19$s
$v1    %4$s        $s3    %20$s
$a0    %5$s        $s4    %21$s
$a1    %6$s        $s5    %22$s
$a2    %7$s        $s6    %23$s
$a3    %8$s        $s7    %24$s
$t0    %9$s        $t8    %25$s
$t1    %10$s        $t9    %26$s
$t2    %11$s        $k0    %27$s
$t3    %12$s        $k1    %28$s
$t4    %13$s        $gp    %29$s
$t5    %14$s        $sp    %30$s
$t6    %15$s        $fp    %31$s
$t7    %16$s        $ra    %32$s

$lo    %33$s        $hi    %34$s
$ip    %35$s"
           (mapcar (lambda (val) (format fmt val))
                   (append (slot-value env 'registers)
                           (list (slot-value env 'low)
                                 (slot-value env 'high)
                                 (slot-value env 'ip)))))))

(defun mips--fmt-stack (env)
  (if (null (slot-value env 'stack))
      "Empty stack"
    (let* ((stack-start (slot-value env 'stack-start))
           (stack-extra-bytes (mod stack-start (* 4 mips-word-bytes)))
           (stack-extra (/ stack-extra-bytes mips-word-bytes))
           (initial-addr (- stack-start stack-extra-bytes))
           (stack (append (make-vector stack-extra 0)
                          (slot-value env 'stack))))
      (cl-loop for (a b c d) on stack by #'cddddr
               with stack-value = initial-addr
               concat (format "%8x:    %08x  %08x  %08x  %08x\n"
                              stack-value a b c d)
               do (cl-incf stack-value 4)))))

(defun mips--fmt-breakpoints (env)
  (let ((breaks (slot-value env 'breakpoints)))
    (if (null breaks)
        "No breakpoints set"
      (mapconcat #'number-to-string (slot-value env 'breakpoints) ", "))))

(defmacro with-writable-buf (&rest args)
  (declare (indent defun))
  `(if buffer-read-only
       (unwind-protect
           (progn
             (setf buffer-read-only nil)
             ,@args)
         (setf buffer-read-only t))
     (progn ,@args)))

(defun mips-write-output (env)
  (with-current-buffer (slot-value env 'buffer)
    (with-writable-buf
      (erase-buffer)
      (insert (format "MIPS evaluator state:

Registers:
%s

Current stack state:
%s

Breakpoints:
%s

Output:
%s"
                  (mips--fmt-registers env)
                  (mips--fmt-stack env)
                  (mips--fmt-breakpoints env)
                  (with-current-buffer (slot-value env 'io-buf)
                    (buffer-string)))))))

(defun mips-test-fmt ()
  (interactive)
  (let* ((info-buffer (generate-new-buffer "mips-test-fmt"))
         (mips-envt (mips-env-new [] 100 info-buffer nil)))
    (with-current-buffer-window info-buffer nil nil
      (mips-output-mode)
      (setq-local mips-buf-env mips-envt)
      (mips-write-output mips-envt))))

(defun mips--step (env &optional count)
  (catch 'mips-terminate
    (dotimes (_ (or count 1))
      (pcase-let ((`(,cmd . ,args) (current-command env)))
        (funcall (cdr (assoc cmd mips-instruction-table)) env args)
        (cl-incf (slot-value env 'ip)))))
  (mips-write-output env))

(defun mips--throw-terminate ()
  (throw 'mips-terminate 'mips-terminate))

(defun mips--handle-overflow (env err)
  (with-current-buffer (slot-value env 'buffer)
    (mips-write-output env)
    (with-writable-buf
      (goto-char (point-max))
      (insert (format "\nInteger overflow: value %s" err)))))

(defun mips--run (env &optional preserve-state)
  (mips-write-output env)
  (unless preserve-state
    (reset-mips-state env))
  (condition-case err
      (progn
        (catch 'mips-terminate
          (cl-loop
           for (cmd . args) = (current-command env)
           until (memq (slot-value env 'ip) (slot-value env 'breakpoints))
           do (funcall (cdr (assoc cmd mips-instruction-table)) env args)
           (cl-incf (slot-value env 'ip))
           when (>= (slot-value env 'ip) (length (slot-value env 'code)))
           do (mips--throw-terminate)))
        (mips-write-output env))
    (mips-overflow (mips--handle-overflow env err))))

(defun mips--parse-breakpoint-val (values ip)
   (cond
    ((null values) nil)
    ((string-prefix-p "+" (nth 0 values))
     (+ ip (string-to-number (substring (nth 0 values) 1))))
    ((string-prefix-p "-" (nth 1 values))
     (- ip (string-to-number (substring (nth 0 values) 1))))
    ((ignore-error 'error
       (cl-parse-integer (nth 0 values) :junk-allowed nil)))
    (t (nth 0 values))))

(defun mips--help-file (_args)
  (mips-todo))

(defun mips-continue ()
  (interactive)
  (mips--run mips-buf-env t))

(defun mips-step ()
  (interactive)
  (mips-step-n 1))

(defun mips-step-n (count)
  (interactive "nCount: ")
  (mips--step mips-buf-env count))

(defun mips-full-command (cmd)
  (interactive "sCommand: ")
  (let ((cmd (mips-parse-command cmd (slot-value mips-buf-env 'ip))))
    (funcall cmd mips-buf-env)))

(defun mips-run ()
  (interactive)
  (mips--run mips-buf-env nil))

(defun mips-next ()
  (interactive)
  (mips-next-n 1))

(defun mips-next-n (count)
  (interactive "nCount: ")
  (mips--step mips-buf-env count))

(defun mips-breakpoint (breakpoint)
  (interactive "sBreakpoint: ")
  (let ((breakpoint (mips--parse-breakpoint-val
                     (list breakpoint) (slot-value mips-buf-env 'ip))))
    (cond
     ((null breakpoint) (add-breakpoint mips-buf-env (slot-value mips-buf-env 'ip)))
     ((numberp breakpoint) (add-breakpoint mips-buf-env breakpoint))
     ((stringp breakpoint)
      (add-breakpoint mips-buf-env (cdr (assoc breakpoint (slot-value mips-buf-env 'labels)))))
     (t (mips-parse-error))))
  (mips-write-output mips-buf-env))

(defun mips-write-reg (register value)
  (interactive "sRegister: \nnValue: ")
  (set-reg mips-buf-env (mips-register-from-name register) value))

(defun mips-quit ()
  (interactive)
  (kill-buffer))

(defun mips--display-value (values)
  (pcase (nth 1 values)
    ("hex" 'hex)
    ("decimal" 'decimal)
    ((or "upper-hex" "Hex") 'upper-hex)
    ("fmt" (nth 2 values))
    (x (mips-parse-error "Invalid display command: %s" x))))

(defun mips-parse-command (text ip)
  (let ((values (split-string text :omit-nulls t)))
    (pcase (nth 0 values)
      ((or "r" "run") #'mips--run)
      ((or "n" "next") (if (= (length values) 1)
                           #'mips--step
                         (lambda (env) (mips--step env (nth 1 values)))))
      ((or "b" "break")
       (let ((breakpoint (mips--parse-breakpoint-val (cdr values) ip)))
         (cond
          ((null breakpoint) (lambda (env) (add-breakpoint env (slot-value env 'ip))))
          ((numberp breakpoint) (lambda (env) (add-breakpoint env breakpoint)))
          ((stringp breakpoint)
           (lambda (env)
             (add-breakpoint env (cdr (assoc (nth 1 args) (slot-value env 'labels))))))
          (t (mips-parse-error)))))
      ((or "c" "continue") (lambda (env) (mips--run env t)))
      ((or "s" "step") (if (= (length values) 1) ;; TODO: Differentiate this from n
                           #'mips--step
                         (lambda (env) (mips--step env (nth 1 values)))))
      ((or "p" "print") (mips-todo))
      ((or "h" "help") (mips--help-file (cdr values)))
      ((or "q" "quit") (lambda (env) (kill-buffer (slot-value env 'buffer))))
      ((or "w" "write") (lambda (env)
                          (set-reg env (mips-register-from-name (nth 1 values))
                                   (string-to-number (nth 2 values)))))
      ("ignore" (mips-todo))
      ("delete" (mips-todo))
      ("disable" (mips-todo))
      ("enable" (mips-todo))
      ("display" (lambda (_)
                   (setq mips-format-type (mips--display-value values))))
      (x (mips-parse-error "Instruction not found: %s" x)))))

(defun mips--parse-data ()
  (save-excursion
    (back-to-indentation)
    (let* ((label (if (looking-at (rx (group (* word)) ?\:))
                      (prog1
                          (match-string 1)
                        (goto-char (match-end 0)))
                    nil)))
      (mips-skip-whitespace t)
      (unless (looking-at (rx ?\. (* word)))
        (mips-parse-error))
      (goto-char (match-end 0))
      (mips-skip-whitespace nil)
      (cons label
            (pcase (match-string 0)
              (".ascii" (mips--parse-string))
              ((or ".asciz" ".string") (concat (mips--parse-string) "\0"))
              (".byte" (apply #'vector
                              (mapcar #'mips-wrap-ubyte
                                      (mips--read-int-list))))
              (".word" (seq-mapcat
                        #'mips-split-int (mips--read-int-list)
                        'vector))
              (".space" (make-vector (mips-read-int) 0))
              (".data" nil)
              (x (mips-parse-error "Unknown data type %s" x)))))))

(defun mips--parse-string ()
  (mips-forward-whitespace t)
  (buffer-substring-no-properties
   (point)
   (or (nth 3 (syntax-ppss))
       (mips-parse-error "Expected a string (line %s)" (line-number-at-pos)))))

(defun mips-wrap-ubyte (byte)
  (logand byte #xff))

(defun mips-split-int (int)
  (let ((wrapped (mips-wrap-unsigned int)))
    (list
     (ash (logand #xff000000 wrapped) 24)
     (ash (logand #x00ff0000 wrapped) 16)
     (ash (logand #x0000ff00 wrapped) 8)
     (logand #x000000ff wrapped))))

(defun mips--read-int-list ()
  (cl-loop with eol = (cdr (bounds-of-thing-at-point 'line))
           until (or (eolp) (>= (point) eol))
           collect (mips-read-int)
           while (= (char-after (point)) ?,)
           do (forward-char)
           do (mips-skip-whitespace nil)))

(defun mips--find-text-marker ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
         (rx line-start (* whitespace) (group ".text") word-end)
         :noerror t)
        (match-beginning 1)
      nil)))

(defun mips--kill-buf ()
  (when (boundp 'mips-buf-env)
    (mips-env-cleanup mips-buf-env)))

(defun mips--setup-output-vars (&optional env)
  (add-hook 'kill-buffer-hook #'mips--kill-buf :local t)
  (make-local-variable 'mips-buf-env)
  (setq-local mips-format-type 'hex)
  (when env
    (setq-local mips-buf-env env)))

(defun mips-run-file ()
  (interactive)
  (pcase-let ((`(,data ,start ,labels) (mips-parse-buf (current-buffer))))
    (let* ((info-buffer (generate-new-buffer "mips-run"))
           (mips-envt (mips-env-new data start info-buffer labels)))
      (with-current-buffer-window info-buffer nil nil
        (mips-output-mode)
        (setq-local mips-buf-env mips-envt)
        (mips-write-output mips-envt)))))

(defvar mips-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "r" #'mips-run)
    (define-key map "n" #'mips-next)
    (define-key map "N" #'mips-next-n)
    (define-key map "b" #'mips-breakpoint)
    (define-key map "c" #'mips-continue)
    (define-key map "s" #'mips-step)
    (define-key map "S" #'mips-step-n)
    (define-key map "i" #'mips-full-command)
    (define-key map "w" #'mips-write-reg)
    (define-key map "q" #'mips-quit)
    map))

(define-derived-mode mips-output-mode special-mode "MIPS output mode"
  :interactive nil
  :after-hook (mips--setup-output-vars))
