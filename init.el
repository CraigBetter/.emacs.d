;TODO 'C-S-h', org-mode, and 'M-!' don't work in tty
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(setq make-backup-files nil) ; stop creating backup~ files, link: http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html
(setq auto-save-default nil) ; stop creating #autosave# files

(setq auto-save-default nil) ;; sanity
(savehist-mode 0)  ;; sanity

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq evil-want-keybinding nil)   ;Warning (evil-collection): Make sure to set `evil-want-keybinding' to nil before loading evil or evil-collection.
(setq evil-want-C-i-jump nil)     ;make tab work with evil and org mode in terminal. Taken from; https://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil) ; make tab work with evil and org mode in terminal. Taken from; https://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working
(require 'evil)			  ;See https://github.com/emacs-evil/evil-collection/issues/60 for more details.
(require 'evil-collection)
(evil-collection-init)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up) ;;sanity
(define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward) ;;why was this not bound by default?
(require 'evil-commentary)
(evil-commentary-mode 1)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-key
  "n" 'notes-menu
  "x b" 'ivy-switch-buffer
  "r" 'eval-region)
(require 'evil-numbers)
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

(require 'telephone-line)
(telephone-line-mode)

(require 'ivy)
(define-key ivy-switch-buffer-map (kbd "C-k") nil)                         ;; unbind ivy-switch-buffer-kill
(define-key ivy-switch-buffer-map (kbd "C-S-k") 'ivy-switch-buffer-kill)   ;; rebind ivy-switch-buffer-kill
(define-key ivy-minibuffer-map (kbd "C-j") 'next-line)                     ;; this works for some reason
(define-key ivy-minibuffer-map (kbd "C-k") 'previous-line)                 ;; see line 301, 302 of ivy.el for reasoning
(define-key ivy-switch-buffer-map (kbd "C-k") 'previous-line)              ;; for some reason this has to be bound in both keymaps after rebinding ivy-switch-buffer-kill
(ivy-mode 1)

(defun notes-menu ()
  "This function is meant to replace the myriad functions I
previously had for accessing my notes and config files"
  (interactive)
  (find-file (ivy-read "bookmark files: " notes-list)))


(require 'constant-theme)
(load-theme 'constant t)
;; (require 'snazzy-theme)
;; (load-theme 'snazzy t)
;(require 'soothe-theme)
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
                   "~/Documents/notes/c-notes.org"))

(require 'geiser)

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
(require 'eyebrowse)  ;; <- I don't need this?
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
;; (require 'essh)
(load-file "~/.emacs.d/essh.el")
(defun essh-sh-hook ()
  (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)
  (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step)
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)
  (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory))
(add-hook 'sh-mode-hook 'essh-sh-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "ba913d12adb68e9dadf1f43e6afa8e46c4822bb96a289d5bf1204344064f041e" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" default)))
 '(package-selected-packages
   (quote
    (minimal-theme constant-theme dracula-theme geiser evil-leader evil-numbers evil-commentary ivy telephone-line soothe-theme snazzy-theme helm eyebrowse evil-collection))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
