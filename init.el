;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

(setq make-backup-files nil)

(when (eq system-type 'darwin)
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;; require the test module hello-world
;(require 'hello)

;;; 快速复制光标所在的单词, 复制一整行
(require 'copy)
(global-set-key (kbd "C-c c w") 'copy-cursor-word)
(global-set-key (kbd "C-c c b") 'backward-copy-word)
(global-set-key (kbd "C-c c l") 'copy-one-line)

;;; 删除/剪切一整行
(require 'remove)
(global-set-key (kbd "C-c r c") ' kill-clipboard-one-line)
(global-set-key (kbd "C-c r k") 'kill-one-line)


;;; 配置一些功能
(setq confirm-kill-emacs #'yes-or-no-p)      ; 在关闭 Emacs 前询问是否确认关闭，防止误触
(electric-pair-mode t)                       ; 自动补全括号
(add-hook 'prog-mode-hook #'show-paren-mode) ; 编程模式下，光标在括号上时高亮另一个括号
;(column-number-mode t)                       ; 在 Mode line 上显示列号
(global-auto-revert-mode t)                  ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(delete-selection-mode t)                    ; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
(setq inhibit-startup-message t)             ; 关闭启动 Emacs 时的欢迎界面
(setq make-backup-files nil)                 ; 关闭文件自动备份
(add-hook 'prog-mode-hook #'hs-minor-mode)   ; 编程模式下，可以折叠代码块
(global-display-line-numbers-mode 1)         ; 在 Window 显示行号
(tool-bar-mode -1)                           ; （熟练后可选）关闭 Tool bar
(when (display-graphic-p) (toggle-scroll-bar -1)) ; 图形界面时关闭滚动条

(savehist-mode 1)                            ; （可选）打开 Buffer 历史记录保存
;(setq display-line-numbers-type 'relative)   ; （可选）显示相对行号
(add-to-list 'default-frame-alist '(width . 90))  ; （可选）设定启动图形界面时的初始 Frame 宽度（字符数）
(add-to-list 'default-frame-alist '(height . 55)) ; （可选）设定启动图形界面时的初始 Frame 高度（字符数）
(if (eq system-type 'darwin) ; 设置utf8
  (setq buffer-file-coding-system 'utf-8-nfd-mac)
  (setq buffer-file-coding-system 'utf-8))
(setq split-height-threshold nil)  ; 包括下面这行，设置默认横向分割window
(setq split-width-threshold 0)

;;; 设置快捷键
(global-set-key (kbd "RET") 'newline-and-indent) ; 按回车键新起一行并做缩进
(global-set-key (kbd "M-w") 'kill-region)              ; 交换 M-w 和 C-w，M-w 为剪切
(global-set-key (kbd "C-w") 'kill-ring-save)           ; 交换 M-w 和 C-w，C-w 为复制
(global-set-key (kbd "C-a") 'back-to-indentation)      ; 交换 C-a 和 M-m，C-a 为到缩进后的行首
(global-set-key (kbd "M-m") 'move-beginning-of-line)   ; 交换 C-a 和 M-m，M-m 为到真正的行首
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region) ; 为选中的代码加注释/去注释
(global-set-key (kbd "M-z") nil) ; 不需要使用zap to char
(global-set-key (kbd "C-g") 'keyboard-quit)

;; 自定义两个函数
;; Faster move cursor
(defun next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 10))

(defun previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 10))
;; 绑定新的键到快捷键
(global-set-key (kbd "M-n") 'next-ten-lines)            ; 光标向下移动 10 行
(global-set-key (kbd "M-p") 'previous-ten-lines)        ; 光标向上移动 10 行
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "C-j C-k") 'kill-whole-line)   ; 删去光标所在行（在图形界面时可以用 "C-S-<DEL>"，终端常会拦截这个按法)

;;; try to fix https://github.com/justbur/emacs-which-key/issues/130#ref-commit-eb4a6f6
(setq inhibit-compacting-font-caches nil)

;;; 设置插件的镜像, 添加插件库 melpa
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ;("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                         ;("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
			 ("melpa" . "https://melpa.org/packages/")
                         ;("elpa" . "http://mirrors.cloud.tencent.com/elpa/elpa/")))
			 ("org" . "http://orgmode.org/elpa/")
			 ("tromey" . "http://tromey.com/elpa/")))
(package-initialize)
; package-list-packages命令用来在M-X中输入package-list-packages来展示仓库下的包	; package-install <packagename> 安装仓库

;;; 输出init.el的内容
(require 'cat-init)

;;; 安装包管理器use-package
(eval-when-compile
  (require 'use-package))

;;; flycheck: 语法检查工具
(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行  
  :hook                        ; 为模式设置 hook
  (prog-mode . flycheck-mode))

(use-package hydra
  :ensure t)

(use-package use-package-hydra
  :ensure t
  :after hydra) 

;;; ivy 安装
(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers nil)
  ;; enable this if you want `swiper' to use it
  (setq search-default-mode #'char-fold-to-regexp)
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring); 在某些终端上 C-x C-SPC 会被映射为 C-x C-@，比如在 macOS 上，所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package amx
  :ensure t
  :init (amx-mode))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; 存在bug
;; (use-package undo-tree
;;   :ensure t
;;   :init (global-undo-tree-mode)
;;   :after hydra
;;   :bind ("C-x C-h u" . hydra-undo-tree/body)
;;   :hydra (hydra-undo-tree (:hint nil)            ; 使用hydra为undo tree设置快捷键提示
;;   "
;;   _p_: undo  _n_: redo _s_: save _l_: load   "
;;   ("p"   undo-tree-undo)
;;   ("n"   undo-tree-redo)
;;   ("s"   undo-tree-save-history)
;;   ("l"   undo-tree-load-history)
;;   ("u"   undo-tree-visualize "visualize" :color blue)
;;   ("q"   nil "quit" :color blue))
;;   :custom
;;   (undo-tree-auto-save-history nil))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package good-scroll
  :ensure t
  :if window-system          ; 在图形化界面时才使用这个插件
  :init (good-scroll-mode))

;;; 展示能够使用的热键
(use-package which-key
  :config
  (setq which-key-allow-imprecise-window-fit t)
  (setq which-key-idle-delay 0.1)
  :init (which-key-mode))

;;; Emacs minibuffer 中的选项添加注解的插件
(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
  			  ("M-A" . marginalia-cycle)))

;;; 在新窗口打开buffer
(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))) ;; alternative for `describe-bindings'

;;; 通过搜索功能在不移动光标的情况下进行快速处理
(use-package avy
  :ensure t
  :config
  (defun avy-action-embark (pt)
	(unwind-protect
		(save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
	t)
  (setf (alist-get ?e avy-dispatch-alist) 'avy-action-embark)
  :bind
  (("C-j C-SPC" . avy-goto-char-timer)))

;;; 多个光标
(use-package multiple-cursors
  :ensure t
  :after hydra
  :bind
  (("C-x C-h m" . hydra-multiple-cursors/body)
   ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))
  :hydra (hydra-multiple-cursors
		  (:hint nil)
		  "
Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
		  ("l" mc/edit-lines :exit t)
		  ("a" mc/mark-all-like-this :exit t)
		  ("n" mc/mark-next-like-this)
		  ("N" mc/skip-to-next-like-this)
		  ("M-n" mc/unmark-next-like-this)
		  ("p" mc/mark-previous-like-this)
		  ("P" mc/skip-to-previous-like-this)
		  ("M-p" mc/unmark-previous-like-this)
		  ("|" mc/vertical-align)
		  ("s" mc/mark-all-in-region-regexp :exit t)
		  ("0" mc/insert-numbers :exit t)
		  ("A" mc/insert-letters :exit t)
		  ("<mouse-1>" mc/add-cursor-on-click)
		  ;; Help with click recognition in this hydra
		  ("<down-mouse-1>" ignore)
		  ("<drag-mouse-1>" ignore)
		  ("q" nil)))

 (use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!") ;; 个性签名，随读者喜好设置
  ;; (setq dashboard-projects-backend 'projectile) ;; 读者可以暂时注释掉这一行，等安装了 projectile 后再使用
  (setq dashboard-startup-banner 'official) ;; 也可以自定义图片
  (setq dashboard-items '((recents  . 5)   ;; 显示多少个最近文件
			  (bookmarks . 5)  ;; 显示多少个最近书签
			  (projects . 10))) ;; 显示多少个最近项目
  (dashboard-setup-startup-hook))

;;; 重复构造语句
(use-package tiny
  :ensure t
  :bind
  ("C-;" . tiny-expand))

;;; 高亮相同符号
(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("C-#" . highlight-symbol)) 

;;; 括号高亮
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; 获取环境变量, 开启对性能有一定影响
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure f
  :init
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;;; 自动补全
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序，如果不喜欢可以去掉

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))

;;; 基于tabnine ai进行代码补全，对性能有影响
;(use-package company-tabnine
;  :ensure t
;  :init (add-to-list 'company-backends #'company-tabnine))

(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  ;; add company-yasnippet to company-backends
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  :bind
  (:map yas-minor-mode-map ("S-<tab>" . yas-expand)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(global-set-key (kbd "M-/") 'hippie-expand)

;;; python支持
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  ;; for debug
  (require 'dap-python))
(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" (expand-file-name "/usr/local/Caskroom/miniconda/base/envs"))
  (setq python-shell-interpreter "python3") ; 更改解释器名字
  (pyvenv-mode t))
; python lsp
(use-package lsp-pyright
  :after lsp
  :hook
  (python-mode . (lambda ()
		  (require 'lsp-pyright)
		  )))

;;; racket支持
(use-package racket-mode
  :ensure t
  :hook (racket-mode . racket-xp-mode))


;;; java支持
(setq lsp-java-maven-download-sources t)

(use-package lsp-java
  :after lsp
  :hook
  (java-mode . (lambda () (require 'lsp-java))))

(use-package java
  :ensure nil
  :after lsp-java
  :bind (:map java-mode-map ("C-c i" . lsp-java-add-import)))

(use-package c++-mode
  :after lsp
  :functions 			; suppress warnings
  c-toggle-hungry-state
  :hook
  (c++-mode . c-toggle-hungry-state))


;;; set up ruby mode, not for macos
(defun ruby-mode-variables () nil)
(when (not *is-a-mac*) 
  (use-package inf-ruby)
  (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
  ;;; ruby mode 
  (use-package ruby-mode
    :ensure nil
    :hook
    (ruby-mode . inf-ruby-minor-mode)
    (compilation-filter . inf-ruby-auto-enter)
    :ensure-system-package (solargraph . "gem install --user-install solargraph"))

  (use-package ruby-test-mode
    :after ruby-mode
    :diminish ruby-test-mode
    :config
    (defun amk-ruby-test-pretty-error-diffs (old-func &rest args)
      "Make error diffs prettier."
      (let ((exit-status (cadr args)))
        (apply old-func args)
        (when (> exit-status 0)
          (diff-mode)
          ;; Remove self
          (advice-remove #'compilation-handle-exit #'amk-ruby-test-pretty-error-diffs))))
    (defun amk-ruby-test-pretty-error-diffs-setup (old-func &rest args)
      "Set up advice to enable pretty diffs when tests fail."
      (advice-add #'compilation-handle-exit :around #'amk-ruby-test-pretty-error-diffs)
      (apply old-func args))
    (advice-add #'ruby-test-run-command :around #'amk-ruby-test-pretty-error-diffs-setup))
    (add-hook 'ruby-mode-hook #'lsp))
    

;;; lsp: vscode 语言后端服务器，用来进行程序语言处理
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
	lsp-file-watch-threshold 50)
  :defer t
  :hook 
  (python-mode . lsp)
  (java-mode . lsp)
  (c-mode . lsp)
  (c++-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration) ; which-key integration
  :commands lsp
  :config
  (setq lsp-enable-links nil)
  (setq lsp-completion-provider :none) ;; 阻止 lsp 重新设置 company-backend 而覆盖我们 yasnippet 的设置
  (setq lsp-headerline-breadcrumb-enable t)
  :bind
  ("C-c l s" . lsp-ivy-workspace-symbol) ;; 可快速搜索工作区内的符号（类名、函数名、变量名等）
  ("C-c l r" . lsp-find-references)
  ("C-c l d" . lsp-find-definition)
  ("C-c l e" . lsp-find-declaration)
  ("C-c l i" . lsp-find-implementation)
  ("C-c l t" . lsp-find-type-definition))

(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-position 'top))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

;;; 代码调试
(use-package dap-mode
  :ensure t
  :after hydra lsp-mode
  :commands dap-debug
  :custom
  (dap-auto-configure-mode t)
  :config
  (dap-ui-mode 1)
  :hydra
  (hydra-dap-mode
   (:color pink :hint nil :foreign-keys run)
   "
^Stepping^          ^Switch^                 ^Breakpoints^         ^Debug^                     ^Eval
^^^^^^^^----------------------------------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bb_: Toggle          _dd_: Debug                 _ee_: Eval
_i_: Step in        _st_: Thread             _bd_: Delete          _dr_: Debug recent          _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add             _dl_: Debug last            _es_: Eval thing at point
_c_: Continue       _su_: Up stack frame     _bc_: Set condition   _de_: Edit debug template   _ea_: Add expression.
_r_: Restart frame  _sd_: Down stack frame   _bh_: Set hit count   _ds_: Debug restart
_Q_: Disconnect     _sl_: List locals        _bl_: Set log message
                  _sb_: List breakpoints
                  _sS_: List sessions
"
   ("n" dap-next)
   ("i" dap-step-in)
   ("o" dap-step-out)
   ("c" dap-continue)
   ("r" dap-restart-frame)
   ("ss" dap-switch-session)
   ("st" dap-switch-thread)
   ("sf" dap-switch-stack-frame)
   ("su" dap-up-stack-frame)
   ("sd" dap-down-stack-frame)
   ("sl" dap-ui-locals)
   ("sb" dap-ui-breakpoints)
   ("sS" dap-ui-sessions)
   ("bb" dap-breakpoint-toggle)
   ("ba" dap-breakpoint-add)
   ("bd" dap-breakpoint-delete)
   ("bc" dap-breakpoint-condition)
   ("bh" dap-breakpoint-hit-condition)
   ("bl" dap-breakpoint-log-message)
   ("dd" dap-debug)
   ("dr" dap-debug-recent)
   ("ds" dap-debug-restart)
   ("dl" dap-debug-last)
   ("de" dap-debug-edit-template)
   ("ee" dap-eval)
   ("ea" dap-ui-expressions-add)
   ("er" dap-eval-region)
   ("es" dap-eval-thing-at-point)
   ("q" nil "quit" :color blue)
   ("Q" dap-disconnect :color red)))

(use-package ag
  :ensure t
  :after (counsel-projectile))

;;; 项目管理
(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically t))

(use-package counsel-projectile
 :ensure t
 :after (projectile)
 :init (counsel-projectile-mode))

;;; git 插件
(use-package magit
  :ensure t)

;;; 工作区插件
(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))

;;; google this
(use-package google-this
  :ensure t
  :init
  (google-this-mode)) 

;;; 图标配置
(use-package all-the-icons
  :if (display-graphic-p))

;;; Doom emacs主题
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; set window by number
(use-package winum
  :ensure t
  :init
  (winum-mode))

;;; set up org-mode
(use-package org
  :ensure t
  :config
  (setq org-adapt-indentation 'headline-data)
  :init
  (org-mode))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(package-selected-packages
   '(changelog-url exec-path-from-shell zprint-format inf-ruby ruby-test-mode all-the-icons nlinum unicode-escape jade-mode auto-package-update counsel flycheck)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
