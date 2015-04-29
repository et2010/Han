;;; packages.el --- chinese Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar chinese-packages
  '(
    ace-pinyin
    cdlatex
    ;; chinese-fonts-setup
    chinese-pyim
    ;; chinese-remote-input
    ;; chinese-word-at-point               ; useful when writing emacs packages to handle Chinese character
    find-by-pinyin-dired
    pangu-spacing;; package chineses go here
    pinyin-search
    visual-fill-column
    vlf
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar chinese-excluded-packages
  ;; '(chinese-fonts-setup)
  "List of packages to exclude.")

;; For each package, define a function chinese/init-<package-chinese>
;;

(defun chinese/init-ace-pinyin ()
  "Initialize ace-pinyin"
  (use-package ace-pinyin
    :init
    (define-key evil-normal-state-map (kbd "SPC d") 'ace-pinyin-dwim)
    :config
    (ace-pinyin-global-mode 1)))

(defun chinese/init-cdlatex ()
  "Initialize cdlatex"
  (use-package cdlatex
    :init (add-hook 'org-mode-hook 'turn-on-org-cdlatex)))

;; (defun chinese/init-chinese-fonts-setup ()
;;   "Initialize chinese-fonts-setup"
;;   (use-package chinese-fonts-setup
;;     :commands
;;     (cfs-edit-profile
;;      cfs-test-fontsizes-at-point
;;      cfs-increment-fontsize-at-point
;;      cfs-decrement-fontsize-at-point)
;;     :config
;;     (setq cfs-profiles '("coding" "writing" "reading"))))

(defun chinese/init-chinese-pyim ()
  "Initialize chinese-pyim"
  (use-package chinese-pyim
    :init
    (setq default-input-method "chinese-pyim")
    (define-key evil-emacs-state-map (kbd "C-<SPC>") 'toggle-input-method)
    :config
    (setq pyim-use-tooltip t)
    (setq pyim-tooltip-width-adjustment 1.2)
    (setq pyim-dicts
          '((:name "BigDict"
                   :file "~/.emacs.d/pyim/dicts/bigdict.pyim"
                   :coding utf-8-unix)
            (:name "SogouPY"
                   :file "~/.emacs.d/pyim/dicts/sogou.pyim"
                   :coding utf-8-unix)))
    ;; switch to English input when helm buffer activate.
    (setq pyim-english-input-switch-function
          'pyim-helm-buffer-active-p)
    ;; turn off evil escape when default input method (pyim) on.
    ;; if not, the first key of escap sequence will cause a problem
    ;; involving hitting enter to fast insert char "f" after typing
    ;; on tooltip.
    (add-hook 'input-method-activate-hook 'pyim-turn-off-evil-escape t)
    ;; after input method deactivated, turn on evil escape.
    (add-hook 'input-method-deactivate-hook 'pyim-turn-on-evil-escape t)
    ))

;; (defun chinese/init-chinese-remote-input ()
;;   "Initialize chinese-remote-input"
;;   (use-package chinese-remote-input
;;     :commands
;;     (remote-input-toggle
;;      remote-input-terminal)))

(defun chinese/init-find-by-pinyin-dired ()
  "Initialize find-by-pinyin-dired"
  (use-package find-by-pinyin-dired
    :commands find-by-pinyin-dired))

(defun chinese/init-pangu-spacing ()
  "Initialize pangu-spacing"
  (use-package pangu-spacing
    :diminish pangu-spacing-mode
    :init (add-hook 'org-mode-hook 'pangu-spacing-mode t)))

(defun chinese/init-pinyin-search ()
  "Initialize pinyin-search"
  (use-package pinyin-search
    :init
    (global-unset-key (kbd "C-s"))
    (global-unset-key (kbd "C-r"))
    (define-key evil-emacs-state-map (kbd "C-s") 'isearch-forward)
    (define-key evil-emacs-state-map (kbd "C-r") 'isearch-backward)
    (define-key evil-normal-state-map (kbd "C-s") 'isearch-forward-pinyin)
    (define-key evil-normal-state-map (kbd "C-r") 'isearch-backward-pinyin)))


(defun chinese/init-visual-fill-column ()
  "Initialize visual-fill-column"
  (use-package visual-fill-column
    :init
    (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
    ;; 最好将word-wrap的值设为nil，否则中英文混排时换行都发生在英文单词结束处，非常难看。
    (add-hook 'visual-line-mode-hook (lambda () (setq word-wrap nil)))))

(defun chinese/init-vlf ()
  "Initialize vlf"
  (use-package vlf
    :init
    (progn
      (require 'vlf-setup)
      (custom-set-variables
       '(vlf-application 'dont-ask)))))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
