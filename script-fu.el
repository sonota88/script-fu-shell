;;;; script-fu.el

;;; .emacs

;; ;; (add-to-list 'load-path "~/foo/script-fu-shell") ; if needed
;; (require 'script-fu)
;;
;; (setq script-fu-program-name
;;       "~/foo/bar/gimp/script-fu/script-fu-shell")
;; ;; NG: "ruby ~/foo/script-fu-shell -v"
;; ;;specify script-fu-shell.bat when windows.
;;
;; ;; not necessary
;; ;; (add-to-list 'ac-modes 'script-fu-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar script-fu-program-name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun script-fu:refresh-ac-dictionary ()
    (interactive)
    ;; 辞書に gimp-procedural-db-query の結果をセット
    (setq func-names
           (read
            (with-temp-buffer
              "*gimp-db*"
              (call-process script-fu-program-name nil t nil
                            "--functions-sexp")
              (buffer-string))))
    (mapcar
     (lambda (func-name)
       (add-to-list 'ac-user-dictionary func-name))
     func-names))


(defun script-fu-other-window ()
    "Run scheme on other window"
    (interactive)
    (let ((win (selected-window)))
      (switch-to-buffer-other-window
       (get-buffer-create "*scheme*"))
      (run-scheme scheme-program-name)
      (select-window win)))


(define-derived-mode script-fu-mode scheme-mode
  "script-fu"
  "Major mode for Script-Fu."

  (setq scheme-program-name script-fu-program-name)
  
  (define-key global-map
    "\C-c\C-s" 'script-fu-other-window)

  (if (featurep 'auto-complete)
      (script-fu:refresh-ac-dictionary))
  
  (defadvice run-scheme
    (after my-run-scheme activate)
    (set-process-coding-system
     (get-process "scheme") 'utf-8-unix 'utf-8-unix))
  (ad-activate 'run-scheme)

  (run-hooks 'script-fu-mode-hook))

(provide 'script-fu)
