; -*- lexical-binding: t -*-
;; ==== ==== ==== ==== GENRAL SETTINGS ==== ==== ==== ====
;; ---- ---- garbage collection ---- ----
(setq gc-cons-threshold (* 20 gc-cons-threshold))

;; ---- ---- coding system ---- ----
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;(setq ns-pop-up-frames nil)
(setq-default indent-tabs-mode nil)
(setq load-prefer-newer t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ==== ==== ==== ==== FILETYPE ASSOCIATION ==== ==== ==== ====
;; ---- ---- LaTeX ---- ----
(add-to-list 'auto-mode-alist '("\\.tex$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.ltx$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.cls$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.sty$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.clo$" . gfn-latex-mode))
(add-to-list 'auto-mode-alist '("\\.bbl$" . gfn-latex-mode))

;; ---- ---- OCaml ---- ----
(add-to-list 'auto-mode-alist '("\\.ml$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mli$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mll$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mly$" . tuareg-mode))

;; ---- ---- Coq ---- ----
(add-to-list 'auto-mode-alist '("\\.v$" . coq-mode))

;; ---- ---- Markdown ---- ----
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; ---- ---- Egison ---- ----
(add-to-list 'auto-mode-alist '("\\.egi$" . egison-mode))

;; ---- ---- Macrodown ---- ----
(add-to-list 'auto-mode-alist '("\\.mcrd$" . mcrd-mode))
(add-to-list 'auto-mode-alist '("\\.mcrdh$" . mcrd-mode))
(add-to-list 'auto-mode-alist '("\\.mcrds$" . mcrd-mode))

;; ---- ---- SATySFi ---- ----
(add-to-list 'auto-mode-alist '("\\.saty$" . satysfi-mode))
(add-to-list 'auto-mode-alist '("\\.satyh$" . satysfi-mode))
(add-to-list 'auto-mode-alist '("\\.satyg$" . satysfi-mode))

;; ==== ==== ==== ==== USER INTERFACE ==== ==== ==== ====
;; ---- ---- hide tool bar and menu bar ---- ----
(tool-bar-mode -1)
(menu-bar-mode -1)

;; ---- ---- font settings ---- ----
(set-face-attribute 'default nil :family "Monaco" :height 130)
;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo UI"))
;(setq face-font-rescale-alist '(("Meiryo UI" . 1.08)))

;;色
;;(set-face-background 'hl-line "darkolivegreen")

;; ---- ---- highlighting current line ---- ----
(global-hl-line-mode 1)

;; ---- ---- show balanced paren ---- ----
(show-paren-mode 1)

;;eldocを使う
;;(add-hook emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; ---- ---- display line and column number ---- ----
(line-number-mode 1)
(column-number-mode 1)

;; ==== ==== general functions ==== ====

(defun gfn-insert-paren-pair-scheme (opn cls)
  (interactive)
  (cond ((use-region-p)
         (let ((rb (region-beginning)))
           (let ((re (region-end)))
             (progn
               (goto-char rb)
               (insert opn)
               (goto-char (1+ re))
               (insert cls)
               (forward-char -1)))))
        (t
         (progn
           (insert opn)
           (insert cls)
           (forward-char -1)))))

(defun gfn-insert-paren-pair ()
  (interactive)
  (gfn-insert-paren-pair-scheme "(" ")"))

(defun gfn-insert-square-bracket-pair ()
  (interactive)
  (gfn-insert-paren-pair-scheme "[" "]"))

(defun gfn-insert-brace-pair ()
  (interactive)
  (gfn-insert-paren-pair-scheme "{" "}"))

(defun gfn-enable-transparency ()
  (interactive)
  (cond
   (window-system
    (progn
      (set-background-color "Black")
      (set-foreground-color "LightGray")
      (set-cursor-color "Gray")
      (set-frame-parameter nil 'alpha 70)
      ))
   (t
    (message "cannot enable transparency; not a window system"))))

(defun gfn-set-alpha (alpha-num)
  (interactive "nalpha: ")
  (cond
   (window-system
    (set-frame-parameter nil 'alpha alpha-num))
   (t
    (message "cannot set transparency; not a window system"))))

(defun gfn-insert-datetime-gen (form)
  (insert (format-time-string form)))

(defun gfn-insert-datetime ()
  (interactive)
  (gfn-insert-datetime-gen "%Y-%m-%d %H:%M:%S"))

(defun gfn-insert-date ()
  (interactive)
  (gfn-insert-datetime-gen "%Y-%m-%d"))

(defun gfn-insert-coding-utf-8 ()
  (interactive)
  (insert "-*- coding: utf-8 -*-"))

(defun gfn-find-title-in-head (head-content)
  (pcase head-content
    (`((title ,_ ,s) . ,_) `(right ,s))
    (`(,_ . ,tail) (gfn-find-title-in-head tail))
    (`nil `(left "cannot detect title"))))

(defun gfn-foldl (f i xs)
  (pcase xs
    (`nil i)
    (`(,x . ,tail) (gfn-foldl f (apply f (list i x)) tail))))

(defun gfn-extract-title-from-dom (dom)
  (pcase dom
    (`(title ,_ ,s)
     `(right ,s))
    (`(,_ ,_ . ,children)
     (gfn-foldl
      (lambda (res child)
        (pcase res
          (`(right ,_) res)
          (`(left ,_) (gfn-extract-title-from-dom child))))
      `(left "seed")
      children))))


(defun gfn-insert-md-link-main (url title)
  (insert (format "* 参考： [%s](%s)" title url)))

(defun gfn-request-get-html (url cb)
  (request
   url
   :parser 'buffer-string
   :error
   (cl-function
    (lambda (&key error-thrown &allow-other-keys&rest _)
      (apply cb (list `(left ,(format "%S" error-thrown))))))
   :success
   (cl-function
    (lambda (&key data &allow-other-keys)
      (if data
          (let ((dom (with-current-buffer (get-buffer-create "*response body*")
                       (erase-buffer)
                       (insert data)
                       (libxml-parse-html-region (point-min) (point-max)))))
            (apply cb (list `(right ,dom))))
        (apply cb (list `(left ,"nil-data"))))))))

(defun gfn-insert-md-link (url)
  (interactive "sURL: ")
  (gfn-request-get-html
   url
   (lambda (res)
     (pcase res
       (`(right ,dom)
        (pcase (gfn-extract-title-from-dom dom)
          (`(right ,title)
           (progn
             (gfn-insert-md-link-main url title)
             (message "finished")))
          (`(left ,errmsg)
           (message "got error (2): %s" errmsg))
          (other
           (message "bug (2): %s" other))))
       (`(left ,errmsg)
        (message "got error (1): %s" errmsg))
       (other
        (message "bug (1): %s" other))))))

;; ==== ==== ==== ==== DISTRIBUTED PACKAGES ==== ==== ==== ====
;; ---- ---- package ---- ----
(require 'package)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade"    . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org"          . "http://orgmode.org/elpa/") t)

(package-initialize)

;; ---- ---- exec-path-from-shell ---- ----
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
;; ---- ---- f ---- ----
;(require 'f)

;; ---- ---- magit ---- ----
;(require 'magit)

;; ---- ---- unicode-escape ---- ----
;(require 'unicode-escape)

;; ---- ---- save-place ---- ----
;(setq-default save-place t)
;(require 'save-place)

;; ---- ---- helm ---- ----
(require 'helm-config)
(helm-mode 1)

;; ---- ---- tabbar ---- ----
(require 'tabbar)
(tabbar-mode 1)

;; ---- ---- open-junk-file ---- ----
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S.")

;; ---- ---- tuareg ---- ----
(require 'tuareg)
(setq tuareg-use-smie nil)

;; ---- ---- Custom --- ---
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-selected-packages
   (quote
    (request company-go go-mode ensime scala-mode rustic flycheck haskell-mode elm-mode sml-mode flymake-cursor point-undo htmlize markdown-mode exec-path-from-shell undo-tree tuareg tabbar restart-emacs recentf-ext paredit open-junk-file helm auto-complete auto-async-byte-compile)))
 '(tuareg-match-clause-indent 2))

;; ---- ---- Golang ---- ----
(add-to-list 'exec-path (expand-file-name "/usr/local/go/bin/"))
(add-to-list 'exec-path (expand-file-name "~/go/bin/"))

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook
  (lambda ()
    (add-hook 'before-save-hook 'gofmt-before-save)
    (local-set-key (kbd "M-.") 'godef-jump)
    (set (make-local-variable 'company-backends) '(company-go))
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 4)
    (setq tab-width 4)))

(require 'company-go)
(add-hook 'go-mode-hook
  (lambda ()
    (company-mode)
    (setq company-transformers '(company-sort-by-backend-importance))
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 3)
    (setq company-selection-wrap-around t)
    (setq completion-ignore-case t)
    (setq company-dabbrev-downcase nil)
    (global-set-key (kbd "C-M-i") 'company-complete)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)))

;; ---- ---- scala ---- ----
;(use-package ensime
;  :ensure t
;  :pin melpa-stable)

;; ---- ---- merlin ---- ----
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)))
(setq merlin-ac-setup t)

;; ---- ---- coq ---- ----
(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "gallina" "Major mode for editing Coq vernacular." t)
(autoload 'run-coq "inferior-coq" "Run an inferior Coq process." t)
(autoload 'run-coq-other-window "inferior-coq" "Run an inferior Coq process in a new window." t)
(autoload 'run-coq-other-frame "inferior-coq" "Run an inferior Coq process in a new frame." t)
(load-file "/usr/local/share/emacs/site-lisp/proof-general/generic/proof-site.el")

;; ---- ---- install-elisp ---- ----
;(require 'install-elisp)

;; ---- ---- restart-emacs ---- ----
(require 'restart-emacs)

;; ---- ---- auto-async-byte-compile ---- ----
(require 'auto-async-byte-compile)
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; ---- ---- undo-tree ---- ----
(require 'undo-tree)
(global-undo-tree-mode t)

;; ---- ---- paredit ---- ----
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

;; ---- ---- auto-complete ---- ----
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; ---- ---- recentf ---- ----
(setq recentf-max-saved-items 2000)
(require 'recentf-ext)

;; ---- ---- autoinsert ---- ----
(auto-insert-mode)
(setq-default auto-insert-directory "~/.emacs.d/auto-insert/")
(define-auto-insert "\\.tex$" "latex-template.tex")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ---- ---- point-undo ---- ----
(require 'point-undo)

;; ---- ---- flymake / flymake-cursor ---- ----
(require 'flymake)
(require 'flymake-cursor)
(add-hook 'c-mode-common-hook
          (lambda ()
            (flymake-mode t)
            (flymake-cursor-mode t)
            (setq flymake-check-was-interrupted t)
            (local-set-key (kbd "C-c C-v") 'flymake-start-syntax-check)
            (local-set-key (kbd "C-c C-p") 'flymake-goto-prev-error)
            (local-set-key (kbd "C-c C-n") 'flymake-goto-next-error)
            (local-set-key (kbd "(") 'gfn-insert-paren-pair)
            (local-set-key (kbd "[") 'gfn-insert-square-bracket-pair)
            (local-set-key (kbd "{") 'gfn-insert-brace-pair)))

;; ---- ---- js-mode ---- ----
(setq js-indent-level 2)

;; ---- ---- css-mode ---- ----
(setq css-indent-offset 2)

;; ---- ---- egison-mode ---- ----
(require 'egison-mode)

;; ---- ---- haskell-mode ---- ----
(require 'hs-lint)
(defun my-haskell-mode-hook ()
    (local-set-key (kbd "C-c l") 'hs-lint))
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

;; ---- ---- flycheck ---- ----
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)

;; ---- ---- request --- ----
(require 'request)

;; ==== ==== ==== ==== KEY BIND ==== ==== ==== ====
;(global-set-key [M-kanji] 'ignore)
;(global-set-key [kanji] 'ignore)
;; ---- ---- common ---- ----
(define-key global-map (kbd "C-c s") 'query-replace)
(define-key global-map (kbd "C-c a") 'beginning-of-buffer)
(define-key global-map (kbd "C-c e") 'end-of-buffer)
(define-key global-map (kbd "C-c t") 'shell)
(define-key global-map (kbd "C-h") 'delete-backward-char)
;(define-key global-map (kbd "M-n") 'scroll-up-command)
;(define-key global-map (kbd "M-p") 'scroll-down-command)
(define-key global-map (kbd "(") 'gfn-insert-paren-pair)
(define-key global-map (kbd "[") 'gfn-insert-square-bracket-pair)
(define-key global-map (kbd "{") 'gfn-insert-brace-pair)
(define-key global-map (kbd "<f5> t") 'gfn-insert-datetime)
(define-key global-map (kbd "<f5> d") 'gfn-insert-date)
(define-key global-map (kbd "<f5> u") 'gfn-insert-coding-utf-8)
(define-key global-map (kbd "<f5> l") 'gfn-insert-md-link)
;; ---- ---- original ---- ----
                                        ;(define-key global-map (kbd "C-c m") 'gfn-macro)
                                        ;(define-key global-map (kbd "C-c l") 'gfn-insert-line)
;; ---- ---- helm ---- ----
(define-key global-map (kbd "C-x C-b") 'helm-buffers-list)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
;; ---- ---- recentf ---- ----
(define-key global-map (kbd "C-c f") 'recentf-open-files)
;; ---- ---- tabbar ---- ----
(define-key global-map (kbd "M-<right>") 'tabbar-forward-tab)
(define-key global-map (kbd "M-<left>") 'tabbar-backward-tab)
(define-key global-map (kbd "C->") 'tabbar-forward-tab)
(define-key global-map (kbd "C-<") 'tabbar-backward-tab)
;; ---- ---- open-junk-file ---- ----
(define-key global-map (kbd "C-x j") 'open-junk-file)
;; ---- ---- undo-tree ---- ----
(define-key global-map (kbd "M-/") 'undo-tree-redo)
(define-key global-map (kbd "C-z") 'undo-tree-undo)
;; ---- ---- point-undo ---- ----
(define-key global-map (kbd "<f7>") 'point-undo)
(define-key global-map (kbd "S-<f7>") 'point-redo)

;; ==== ==== ==== ==== MY PACKAGES ==== ==== ==== ====
(require 'gfn-latex)
(require 'open-group)
(require 'mcrd)
(require 'satysfi)

;; ==== ==== ==== ==== FOOTER ==== ==== ==== ====
(provide 'init)
;;; init.el ends here
