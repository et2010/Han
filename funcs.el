;;; funcs.el --- Han Layer functions File for Spacemacs
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

;; from

(defun pyim-use-dict:bigdict ()
  (interactive)
  (setq pyim-dicts
        '((:name "BigDict"
                 :file "~/.emacs.d/pyim/dicts/bigdict.pyim"
                 :coding utf-8-unix)))
  (pyim-restart-1 t))

(defun pyim-use-dict:sogou ()
  (interactive)
  (setq pyim-dicts
        '((:name "SogouPY"
                 :file "~/.emacs.d/pyim/dicts/sogou.pyim"
                 :coding utf-8-unix)))
  (pyim-restart-1 t))

(defun pyim-helm-buffer-active-p ()
  (string-prefix-p
   "helm"
   (buffer-name
    (window-buffer
     (active-minibuffer-window)))))

(defun pyim-turn-off-evil-escape ()
  "Turn off evil escape by remapping the key."
  (define-key evil-emacs-state-map
    (kbd "<remap> <evil-escape-emacs-state>") 'self-insert-command))

(defun pyim-turn-on-evil-escape ()
  "Turn on evil escape by reset key mapping to default."
  (define-key evil-emacs-state-map
    [remap evil-escape-emacs-state] nil))
