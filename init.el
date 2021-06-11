;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgMode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; for string utils
(use-package s)

;; Keep things clean
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Reload buffer when file changes on disk
(global-auto-revert-mode 1)

;; Windmove for moving windows with Shift + arrow keys, rather than C-x o
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disable the toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing space

(menu-bar-mode -1) ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode)


;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Replace yes/no prompts with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Font
(set-face-attribute 'default nil :font "Fira Mono" :height 140)

;; Fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Mono" :height 140)

;; Variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 200 :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; general package to define keys more precisely
(use-package general
  :config
  (general-create-definer md/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (md/leader-keys
    "r" '(:ignore t :which-key "refile")
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(general-define-key
 "C-M-j" 'counsel-switch-buffer)

;; Put listed modes into evil emacs mode (normal emacs mode)
(defun md/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; Setup evil mode!
(use-package evil
  :init
  (setq evil-want-integration t) 
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t) ;; replaces universal argument key binding
  (setq evil-want-C-i-jump nil)
  :config
  (add-hook 'evil-mode-hook 'md/evil-hook)
  (evil-mode)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; remove to allow help menu in insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


;; evil mode configurations for different emacs modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package doom-themes)

(load-theme 'doom-one t)

;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

(use-package diminish)

;; which-key (helper) 
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; completion frameworks - helm, ivy -- a bit different
;; let's go with ivy!
;; we'll get auto-complete on M-x, C-x f (find file) and others.
;; TODO: investigate :diminish, to remove minor-mode for ivy from the modeline!
(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config
  (ivy-mode 1))

;; counsel - install and set bindings for details on key bindings
;; also required for ivy-rich below
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; ivy-rich - extend ivy
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; helpful - more details in better formatted help screen
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Hydra for transient key bindings
(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(md/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(defun md/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

;; Improve the bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-hide-emphasis-markers t)

(defun md/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . md/org-mode-visual-fill))

(defun md/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org ;; TODO - check
  :hook (org-mode . md/org-mode-setup)
  :config

  ;; My org file locations
  (setq md--org-projects-dir (expand-file-name "projects" org-directory))
  (setq md--org-templates (expand-file-name "templates" org-directory))
  (setq md--org-project-template (expand-file-name "project.org" md--org-templates))
  (setq md--org-tasks (expand-file-name "tasks.org" org-directory))
  (setq md--org-incubate (expand-file-name "incubate.org" org-directory))

  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files `(,md--org-tasks ,md--org-projects-dir))
  (md/org-font-setup))

(defun md/get-project-name ()
  (setq md--org-capture-project (read-string "Project name:"))
  (expand-file-name
   (format "%s.org" (s-snake-case md--org-capture-project)) md--org-projects-dir))

(setq org-capture-templates
      `(("p" "Projects")
        ("pp" "Project" entry (file md/get-project-name)
         (file ,md--org-project-template))
        ("t" "Task" entry (file+headline md--org-tasks "Tasks")
         "* TODO %?\n %U\n %a\n %i" :empty-lines 1)))

;; Save org buffers after refiling
(advice-add 'org-refile :after
            (lambda (&rest _)
              (org-save-all-org-buffers)))

;; From https://mollermara.com/blog/Fast-refiling-in-org-mode-with-hydras/
(defun md/org-refile (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))

;; Refiling operations for processing the inbox
(defhydra hydra-org-refiler ()
  ("<up>" org-previous-visible-heading "prev")
  ("<down>" org-next-visible-heading "next")
  ("k" org-previous-visible-heading "prev")
  ("j" org-next-visible-heading "next")
  ("t" (md/org-refile md--org-tasks "Tasks") "Tasks")
  ("i" (md/org-refile md--org-incubate "Incubate") "Incubate"))

(md/leader-keys
  "r" '(hydra-org-refiler/body :which-key "refile"))

;; Rainbow delimiters!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
