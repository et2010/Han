;;; funcs.el --- Chinese Layer functions File for Spacemacs
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
                 :file "/home/neo/.emacs.d/pyim/dicts/bigdict.pyim"
                 :coding utf-8-unix)))
  (pyim-restart-1 t))

(defun pyim-use-dict:sogou ()
  (interactive)
  (setq pyim-dicts
        '((:name "SogouPY"
                 :file "/home/neo/.emacs.d/pyim/dicts/sogou.pyim"
                 :coding utf-8-unix)))
  (pyim-restart-1 t))
