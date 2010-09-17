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
(defvar script-fu:help-buffer-name "*Script-Fu Help*")


(defun script-fu:help-highlight (buffer)
  (with-current-buffer buffer

    (save-excursion
      (goto-char (point-min))
      (while (search-forward "" nil t)
        (replace-match "" nil nil)))
    
    (let ((end))
      (font-lock-mode t)
      (goto-char (point-min))
      (while (search-forward-regexp "^----.+----$" nil t)
        (setq end (point))
        (beginning-of-line)
        (add-text-properties
         (point) end '(font-lock-face font-lock-builtin-face))
        (forward-line))

      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\(-------- \\(Arguments\\|Return values\\) --------\\|--\\)$" nil t)
        (forward-line) (end-of-line)
        (setq end (point))
        (beginning-of-line)
        (add-text-properties
         (point) end '(font-lock-face font-lock-variable-name-face))
        (forward-line)))))


(defun script-fu:show-help-filter (proc str)
  (set-process-filter proc nil)

  (let ((help-buf (get-buffer-create "*Script-Fu Help*")))
    (with-current-buffer help-buf
      (erase-buffer)
      (insert str)

      ;; format output-msg
      (progn
        ;; (delete-backward-char 4)
        (insert "\n")

        (goto-char (point-min))
        (if (search-forward "=> \"" nil t)
            (replace-match "")))

      (goto-char (point-min))
      (script-fu:help-highlight help-buf)
      (goto-char (point-min)))))


(defun script-fu:help-other-window (name)
  (let ((orig-buf (current-buffer))
        (orig-win (selected-window))
        (help-buf (get-buffer-create script-fu:help-buffer-name))
        (str-to-send (concat
                      "(procedure-info \""
                      name
                      "\")\n"))
        (process-name "scheme"))

    (with-temp-buffer ;; inhibit switching buffer
      (run-scheme script-fu-program-name))

    (with-current-buffer help-buf
      (erase-buffer)
      (insert "please wait..."))

    (set-process-filter (get-process process-name)
                        'script-fu:show-help-filter)
    (process-send-string process-name str-to-send)

    (if (equal help-buf (current-buffer))
        (goto-char (point-min))
      (pop-to-buffer help-buf))

    (select-window orig-win)
    (switch-to-buffer orig-buf)))


(setq anything-c-source-script-fu
      '((name . "Script-Fu functions")
        (candidates . script-fu:functions-list)
        (action . (("Show help" . script-fu:help-other-window)
                   ("Insert" . insert)))))

(defun anything-script-fu-functions ()
  (interactive)
  (anything 'anything-c-source-script-fu))


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
  
  (let ((process-result)
        func-names)
    (catch 'get-function-list
      (setq func-names
            (read
             (with-temp-buffer
               "*gimp-db*"
               (setq process-result
                     (call-process script-fu-program-name nil t nil
                                   "--functions-sexp"))
               (unless (= 0 process-result)
                 (throw 'get-function-list process-result))
               (buffer-string))))
      (setq script-fu:functions-list func-names)
      (setq ac-user-dictionary (append func-names ac-user-dictionary)))

    (unless (= 0 process-result)
      (warn "Script-Fu mode: Fail in getting function list. Maybe server is not running."))))


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
