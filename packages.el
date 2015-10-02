;;; packages.el --- han Layer packages File for Spacemacs
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

(setq han-packages
  '(
    ;; package hans go here
    chinese-pyim
    pinyin-search
    ))

(setq han-excluded-packages '())

;; For each package, define a function han/init-<package-han>
;;

(defun han/init-chinese-pyim ()
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
                   :file "~/.emacs.d/.cache/bigdict.pyim"
                   :coding utf-8-unix)
            (:name "SogouPY"
                   :file "~/.emacs.d/.cache/sogou.pyim"
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

(defun han/init-pinyin-search ()
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
