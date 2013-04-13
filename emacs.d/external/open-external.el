(defun execute-program-silent (command)
  "Execute the COMMAND asynchronously and track it so later can be listed using `execute-list'."
  (interactive (list 
		(read-shell-command "Command?")))
  (let* ((proc-obj 
	  (start-process-shell-command "execute-process" "execute-process-buffer" command))
	 )
    
    (when proc-obj
      ; Add to the alist
      (setq execute-command-alist
	    (cons (cons command proc-obj) execute-command-alist))
      )
    )
  )

(defun execute-external()
  (execute-program-silent "chromium-browser www.yahoo.com"))

