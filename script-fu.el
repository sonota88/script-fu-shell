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
(defvar script-fu:functions-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun script-fu:eldoc-signature (name)
  (with-temp-buffer
    "*gimp-db*"
    (call-process script-fu-program-name nil t nil
                  "--function-args" name)
    (goto-char (point-max))
    (backward-delete-char 1)
    (goto-char (point-min))
    (replace-string "\n" " ")
    (buffer-string)))

(defun script-fu:get-current-symbol-info ()
  (interactive)
  (let* ((name (format "%s" (sexp-at-point)))
         (signature
          (if (member name script-fu:functions-list)
              (script-fu:eldoc-signature name)
            nil)))
    (when signature
      (format "%s: (%s)"
              name
              signature))))


(defun script-fu:refresh-ac-dictionary ()
    (interactive)
    (auto-complete-mode t)
    ;; 辞書に gimp-procedural-db-query の結果をセット
    (setq func-names
           (read
            (with-temp-buffer
              "*gimp-db*"
              (call-process script-fu-program-name nil t nil
                            "--functions-sexp")
              (buffer-string))))
    (setq script-fu:functions-list func-names)

    (mapcar
     (lambda (func-name)
       (add-to-list 'ac-user-dictionary func-name))
     func-names))

(defun script-fu:regist-builtin-functions ()
  (dolist (func-name
           '("unbreakupstr"
             "string-append"))
    (add-to-list 'ac-user-dictionary func-name)))


(defun script-fu-other-window ()
    "Run scheme on other window"
    (interactive)
    (let ((win (selected-window)))
      (switch-to-buffer-other-window
       (get-buffer-create "*scheme*"))
      (run-scheme scheme-program-name)
      (select-window win)))


(define-derived-mode script-fu-mode scheme-mode
  "Script-Fu"
  "Major mode for Script-Fu."

  (setq scheme-program-name script-fu-program-name)

  (define-key script-fu-mode-map
    (kbd "C-c C-s") 'script-fu-other-window)

  (when (featurep 'auto-complete)
    (script-fu:refresh-ac-dictionary)
    (script-fu:regist-builtin-functions))
  
  (when t
    (make-local-variable 'eldoc-documentation-function)
    (setq eldoc-documentation-function 'script-fu:get-current-symbol-info)
    (turn-on-eldoc-mode))
  
  (defadvice run-scheme
    (after my-run-scheme activate)
    (set-process-coding-system
     (get-process "scheme") 'utf-8-unix 'utf-8-unix))
  (ad-activate 'run-scheme)

  (run-hooks 'script-fu-mode-hook))


(provide 'script-fu)
