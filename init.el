;TODO 'C-S-h', org-mode, and 'M-!' don't work in tty, make word objects work the same way they do in vim? Ivy completion doen't work in terminal.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

; ----------------------------------------------------------------------------- new stuff
(defun launch-separate-emacs-in-terminal () ; https://emacs.stackexchange.com/questions/5428/restart-emacs-from-within-emacs
  (suspend-emacs "fg ; emacs -nw"))
(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))
(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'launch-separate-emacs-under-x
							   #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

; install the missing packages
;(package-refresh-contents)
;(when (require 'use-package nil 'noerror)
;  (package-install 'use-package))
;(package-install 'use-package)
(setq package-list '(evil evil-collection evil-numbers evil-leader evil-commentary telephone-line ivy eyebrowse geiser use-package gruvbox-theme counsel lua-mode))
; constant-theme 
(let ((restart nil))
  (progn
    (dolist (package package-list)
      (if (not (package-installed-p package)) ; It works, I did it!
	  (progn (package-refresh-contents) ; https://emacs.stackexchange.com/questions/39250/error-package-use-package-is-unavailable
		 (package-install package)
		 (setq restart t))))
    (if restart (restart-emacs)))) ; WARNING: this avoids problems with bytecompile warnings, and evil initialization order but also stops me from seeing warnings and such

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/")
  (require 'use-package))

; https://www.reddit.com/r/emacs/comments/4fqu0a/automatically_install_packages_on_startup/
; list the packages you want
;(setq package-list '(evil evil-collection evil-numbers evil-leader evil-commentary telephone-line ivy constant-theme eyebrowse))

; ----------------------------------------------------------------------------- new stuff

(setq make-backup-files nil) ; stop creating backup~ files, link: http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html
(setq auto-save-default nil) ; stop creating #autosave# files

(setq auto-save-default nil) ;; sanity
(savehist-mode 0)  ;; sanity

(setq inhibit-splash-screen 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq evil-want-keybinding nil)   ;Warning (evil-collection): Make sure to set `evil-want-keybinding' to nil before loading evil or evil-collection.
(setq evil-want-C-i-jump nil)     ;make tab work with evil and org mode in terminal. Taken from; https://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil) ; make tab work with evil and org mode in terminal. Taken from; https://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working
(use-package evil)			  ;See https://github.com/emacs-evil/evil-collection/issues/60 for more details.
(use-package evil-collection)
(evil-collection-init)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up) ;;sanity
(define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward) ;;why was this not bound by default?
(use-package evil-commentary)
(evil-commentary-mode 1)
(use-package evil-leader)
(global-evil-leader-mode)
(evil-leader/set-key
  "n" 'notes-menu
  "x b" 'ivy-switch-buffer
  "r" 'eval-region)
(use-package evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-visual-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-S-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-S-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-S-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-S-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "<f9>") 'compile)
(define-key evil-normal-state-map (kbd "<f10>") '(lambda () (interactive) (progn (save-buffer) (compile "make"))))
(setq sentence-end-double-space nil) ; from: https://emacs.stackexchange.com/questions/14358/how-do-i-jump-to-the-next-sentence-in-evil

(setq scroll-margin 7)
(setq scroll-conservatively 1)

(use-package telephone-line)
(telephone-line-mode)

(use-package ivy)
(define-key ivy-switch-buffer-map (kbd "C-k") nil)                         ;; unbind ivy-switch-buffer-kill
(define-key ivy-switch-buffer-map (kbd "C-S-k") 'ivy-switch-buffer-kill)   ;; rebind ivy-switch-buffer-kill
(define-key ivy-minibuffer-map (kbd "C-j") 'next-line)                     ;; this works for some reason
(define-key ivy-minibuffer-map (kbd "C-k") 'previous-line)                 ;; see line 301, 302 of ivy.el for reasoning
(define-key ivy-switch-buffer-map (kbd "C-k") 'previous-line)              ;; for some reason this has to be bound in both keymaps after rebinding ivy-switch-buffer-kill
(ivy-mode 1)

(use-package counsel)
(counsel-mode 1)

(defun notes-menu ()
  "This function is meant to replace the myriad functions I
previously had for accessing my notes and config files"
  (interactive)
  (find-file (ivy-read "bookmark files: " notes-list)))


(use-package gruvbox-theme)
(load-theme 'gruvbox-dark-hard t)
;; (use-package constant-theme)
;; (load-theme 'constant t)
;; (use-package snazzy-theme)
;; (load-theme 'snazzy t)
;(use-package soothe-theme)
;(load-theme 'soothe t)


(global-display-line-numbers-mode)

(setq notes-list '("~/Documents/notes/bookmarks.org"
                   "~/Documents/notes/qutebrowser.org"
                   "~/Documents/notes/awesome.org"
                   "~/Documents/notes/openSCAD.org"
                   "~/Documents/notes/CommonLisp.org"
                   "~/Documents/notes/Python.org"
                   "~/Documents/notes/MATELLog.org"
                   "~/Documents/notes/EmacsLog.org"
                   "~/.emacs.d/mylisp/defaults.el"
                   "~/Documents/notes/ClusterLog.org"
                   "~/Documents/notes/BashLog.org"
                   "~/Documents/notes/Todo.org"
                   "~/Documents/notes/Log.org"
                   "~/.emacs.d/notes-menu.org"
                   "~/.emacs.d/init.el"
                   "~/Documents/lisp/clojure/test.clj"
                   "~/Documents/notes/c-notes.org"
		   "~/Documents/notes/javascript.org"))

(use-package geiser)

;;---------- sarcasm -----------------------------------------
(progn
  (defun sarcasify-line ()
    (interactive)
    (let ((line-size
           (- (line-end-position) (line-beginning-position))) ; size of line
          (count 1)
          (start-position (point))) ; 0 index it?
      (beginning-of-line)
      (forward-char)
      (while (< count line-size)
        (progn (evil-invert-case (point) (1+ (point)))
               (forward-char 2)
               (setq count (+ count 2))))
      (goto-char start-position)))
  (evil-leader/set-key "x s" 'sarcasify-line))

;;------- some eyebrowse bindings -------------------------------------
(use-package eyebrowse)  ;; <- I don't need this?
(define-key evil-normal-state-map (kbd "g 0") 'eyebrowse-switch-to-window-config-0) ;; maybe also do this for visual state?
(define-key evil-normal-state-map (kbd "g 1") 'eyebrowse-switch-to-window-config-1)
(define-key evil-normal-state-map (kbd "g 2") 'eyebrowse-switch-to-window-config-2)
(define-key evil-normal-state-map (kbd "g 3") 'eyebrowse-switch-to-window-config-3)
(define-key evil-normal-state-map (kbd "g 4") 'eyebrowse-switch-to-window-config-4)
(define-key evil-normal-state-map (kbd "g 5") 'eyebrowse-switch-to-window-config-5)
(define-key evil-normal-state-map (kbd "g 6") 'eyebrowse-switch-to-window-config-6)
(define-key evil-normal-state-map (kbd "g 7") 'eyebrowse-switch-to-window-config-7)
(define-key evil-normal-state-map (kbd "g 8") 'eyebrowse-switch-to-window-config-8)
(define-key evil-normal-state-map (kbd "g 9") 'eyebrowse-switch-to-window-config-9)
(setq eyebrowse-new-workspace t)
(eyebrowse-mode)

;; (define-key evil-normal-state-map (kbd "g 0") '(lambda () (interactive) (progn (eyebrowse-switch-to-window-config-0) (delete-other-windows) (switch-to-buffer "scritch"))))
;; (define-key evil-normal-state-map (kbd "g 1") '(lambda () (interactive) (progn (eyebrowse-switch-to-window-config-1) (delete-other-windows) (switch-to-buffer "scritch"))))
;; (define-key evil-normal-state-map (kbd "g 2") '(lambda () (interactive) (progn (eyebrowse-switch-to-window-config-2) (delete-other-windows) (switch-to-buffer "scritch"))))
;; (define-key evil-normal-state-map (kbd "g 3") '(lambda () (interactive) (progn (eyebrowse-switch-to-window-config-3) (delete-other-windows) (switch-to-buffer "scritch"))))
;; (define-key evil-normal-state-map (kbd "g 4") '(lambda () (interactive) (progn (eyebrowse-switch-to-window-config-4) (delete-other-windows) (switch-to-buffer "scritch"))))
;; (define-key evil-normal-state-map (kbd "g 5") '(lambda () (interactive) (progn (eyebrowse-switch-to-window-config-5) (delete-other-windows) (switch-to-buffer "scritch"))))
;; (define-key evil-normal-state-map (kbd "g 6") '(lambda () (interactive) (progn (eyebrowse-switch-to-window-config-6) (delete-other-windows) (switch-to-buffer "scritch"))))
;; (define-key evil-normal-state-map (kbd "g 7") '(lambda () (interactive) (progn (eyebrowse-switch-to-window-config-7) (delete-other-windows) (switch-to-buffer "scritch"))))
;; (define-key evil-normal-state-map (kbd "g 8") '(lambda () (interactive) (progn (eyebrowse-switch-to-window-config-8) (delete-other-windows) (switch-to-buffer "scritch"))))
;; (define-key evil-normal-state-map (kbd "g 9") '(lambda () (interactive) (progn (eyebrowse-switch-to-window-config-9) (delete-other-windows) (switch-to-buffer "scritch"))))

;; here is essh stuff
;; (use-package essh)
(load-file "~/.emacs.d/essh.el")
(defun essh-sh-hook ()
  (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)
  (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step)
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)
  (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory))
(add-hook 'sh-mode-hook 'essh-sh-hook)

;; this replaces eshell's weird native tab completion with ivy's tab completion
(add-hook 'eshell-mode-hook  ; https://emacs.stackexchange.com/questions/27849/how-can-i-setup-eshell-to-use-ivy-for-tab-completion
  (lambda () 
    (define-key eshell-mode-map (kbd "<tab>")
      (lambda () (interactive) (pcomplete-std-complete)))))

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
    (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))
;; Bind it to a key.
;; (global-set-key [(super shift return)] 'toggle-maximize-buffer) 
(evil-leader/set-key "m" 'toggle-maximize-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "ba913d12adb68e9dadf1f43e6afa8e46c4822bb96a289d5bf1204344064f041e" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" default)))
 '(package-selected-packages
   (quote
    (ivy-explorer doom-themes counsel sublimity rainbow-blocks rainbow-delimiters gruvbox-theme lua-mode go-mode minimal-theme constant-theme dracula-theme geiser evil-leader evil-numbers evil-commentary ivy telephone-line soothe-theme snazzy-theme helm eyebrowse evil-collection))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-unset-key (kbd "C-x C-c")) ;; try to find what was lost
