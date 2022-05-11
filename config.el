;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.

(setq user-full-name "klchen0112"
    user-mail-address "klchen0112@gmail.com")

;; Simple Settings

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

(display-time-mode 1)                             ; Enable time in the mode-line


(global-subword-mode 1)                           ; Iterate through CamelCase words

;; Framing Size
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 80))

;; Windows

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))


;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
(setq doom-font (font-spec :family "Fira Code" :size 16)
      doom-unicode-font (font-spec :family "PingFang SC"))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
;; DON'T use (`font-family-list'), it's unreliable on Linux
;; org mode table
;;(add-hook 'org-mode-hook #'valign-mode)


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.org" ; let's put files here
      org-use-property-inheritance t              ; it's convenient to have properties inherited
      org-log-done 'time                          ; having the time a item is done sounds convenient
      org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
      org-export-in-background t                  ; run export processes in external emacs process
      org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
      org-export-with-sub-superscripts '{})       ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
(use-package org-roam
  :after org
  :init
  :commands (org-roam-buffer
             org-roam-setup
             org-roam-capture
             org-roam-node-find)
  :custom
  (org-roam-directory "~/.org/pages")
  (org-roam-dailies-directory "~/.org/journals")
  (org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-file-extensions '("org"))
  (setq org-id-link-to-org-use-id t)
  (setq org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%I:%M %p>: %?"
       :if-new (file+head "%<%Y_%m_%d>.org" "%<%Y/%m/%d>\n* tags\n"))))
  (org-roam-capture-templates '(;; ... other templates ;; 设置 capture 模板
                ("d" "default" plain "%?"
                 :target (file+head "${slug}.org"
                                    "${title}\n#+public: true\n* tags\n")
                 :unnarrowed t)
                ))
  :bind (("C-c n a" . org-id-get-create)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n r" . org-roam-ref-find)
         ("C-c n R" . org-roam-ref-add)
         ("C-c n s" . org-roam-db-sync)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  ;; Ensure the keymap is available
  (require 'org-roam-dailies)
  )
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
    :hook (after-init . org-roam-ui-mode)
    :commands org-roam-ui-open
    :config
    (require 'org-roam) ; in case autoloaded
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)
    )

;;自动创建笔记的创建时间和修改时间
(use-package! org-roam-timestamps
  :after org-roam
  :config
  (org-roam-timestamps-mode))

;;跨文件的引用，能够实现笔记的一处修改，处处修改。
(use-package! org-transclusion
  :after org-roam
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))
;; org latex
(add-hook 'org-mode-hook 'org-fragtog-mode)
;; org roam bibtex
(use-package org-roam-bibtex
  :after org-roam
  :init
  (org-roam-bibtex-mode 1)
  :custom
  (orb-note-actions-interface 'default)
)
;; org-roam-company
(use-package company-org-roam
  :after org-roam
  :config
  (push 'company-org-roam company-backends))
;; Company Mode
;; Plain Text Company
(setq company-idle-delay 0.2)
(setq company-show-quick-access t)
(setq company-elisp-detect-function-context nil)
(setq company-minimum-prefix-length 2)


(defun ora-company-number ()
  "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (or (cl-find-if (lambda (s) (string-match re s))
                        company-candidates)
            (> (string-to-number k)
               (length company-candidates))
            (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
        (self-insert-command 1)
      (company-complete-number
       (if (equal k "0")
           10
         (string-to-number k))))))

(defun ora--company-good-prefix-p (orig-fn prefix)
  (unless (and (stringp prefix) (string-match-p "\\`[0-9]+\\'" prefix))
    (funcall orig-fn prefix)))

(let ((map company-active-map))
  (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
        (number-sequence 0 9))
  (define-key map " " (lambda ()
                        (interactive)
                        (company-abort)
                        (self-insert-command 1)))
  (define-key map (kbd "<return>") nil))
;; LSP MODE
(use-package lsp-grammarly
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp))))  ; or lsp-deferred

;; Which-key
;; DOOM EMACS key help
(setq which-key-idle-delay 0.5) ;; I need the help, I really do

;; Input Method
(if IS-MAC (use-package! rime
    :custom
    (rime-librime-root "~/.emacs.d/librime/dist")
    (rime-show-candidate 'posframe)
    (rime-show-preedit 'inline)
    (rime-user-data-dir "~/.emacs.d/.local/etc/rime/")
    (rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@28/28.1/include")
))
(setq default-input-method "rime")

;;
;; Some functions copied from `pyim', thanks for tumashu@github.com .
;;
(defun +rime--char-before-to-string (num)
  "得到光标前第 `num' 个字符，并将其转换为字符串。"
  (let* ((point (point))
         (point-before (- point num)))
    (when (and (> point-before 0)
               (char-before point-before))
      (char-to-string (char-before point-before)))))

(defun +rime--string-match-p (regexp string &optional start)
  "与 `string-match-p' 类似，如果 REGEXP 和 STRING 是非字符串时，
不会报错。"
  (and (stringp regexp)
       (stringp string)
       (string-match-p regexp string start)))

(defun +rime--probe-auto-english ()
  "激活这个探针函数后，使用下面的规则自动切换中英文输入：

1. 当前字符为英文字符（不包括空格）时，输入下一个字符为英文字符
2. 当前字符为中文字符或输入字符为行首字符时，输入的字符为中文字符
3. 以单个空格为界，自动切换中文和英文字符
   即，形如 `我使用 emacs 编辑此函数' 的句子全程自动切换中英输入法
"
  (let ((str-before-1 (+rime--char-before-to-string 0))
        (str-before-2 (+rime--char-before-to-string 1)))
    (unless (string= (buffer-name) " *temp*")
      (if (> (point) (save-excursion (back-to-indentation)
                                     (point)))
          (or (if (+rime--string-match-p " " str-before-1)
                  (+rime--string-match-p "\\cc" str-before-2)
                (not (+rime--string-match-p "\\cc" str-before-1))))))))

(defun +rime--beancount-p ()
  "当前为`beancount-mode'，且光标在注释或字符串当中。"
  (when (derived-mode-p 'beancount-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun +rime--evil-mode-p ()
  "检测当前是否在 `evil' 模式下。"
  (or (evil-normal-state-p)
      (evil-visual-state-p)
      (evil-motion-state-p)
      (evil-operator-state-p)))

(defun +rime-english-prober()
  "自定义英文输入探针函数，用于在不同mode下使用不同的探针列表"
  (let ((use-en (or (button-at (point))
                    (+rime--evil-mode-p))))
    (if (derived-mode-p 'telega-chat-mode)
        (setq use-en (or use-en
                         (+rime--probe-auto-english)))
      (when (derived-mode-p 'text-mode)
        (setq use-en (or use-en
                         (+rime--probe-auto-english))))
      (when (derived-mode-p 'prog-mode 'conf-mode)
        (setq use-en (or use-en
                         (rime--after-alphabet-char-p))))
      (setq use-en (or use-en
                       (rime--prog-in-code-p)
                       (+rime--beancount-p))))
    use-en))

(setq rime-disable-predicates '(+rime-english-prober))
;; VISUALIZE

;; UI EMOJI


;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
