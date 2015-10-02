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

(setq chinese-packages
  '(
    ;; package chineses go here
    ace-pinyin
    chinese-pyim
    find-by-pinyin-dired
    pangu-spacing
    pinyin-search
    ))

(setq chinese-excluded-packages '())

;; For each package, define a function chinese/init-<package-chinese>
;;

(defun chinese/init-ace-pinyin ()
  "Initialize ace-pinyin"
  (use-package ace-pinyin
    :init
    (define-key evil-normal-state-map (kbd "SPC d") 'ace-pinyin-dwim)
    :config
    (ace-pinyin-global-mode 1)))

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
    ;; when trying to fast insert corresponding letter by hitting Enter.
    (add-hook 'input-method-activate-hook 'pyim-turn-off-evil-escape t)
    ;; after input method deactivated, turn on evil escape.
    (add-hook 'input-method-deactivate-hook 'pyim-turn-on-evil-escape t)
    ))

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

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
