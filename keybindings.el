;; keyindings.el --- Han Layer key bindings File
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

(evil-leader/set-key "d" 'ace-pinyin-dwim)
(spacemacs/declare-prefix "o" "Chinese")
(evil-leader/set-key "od" 'find-by-pinyin-dired)
(evil-leader/set-key "oy" 'youdao-dictionary-search-at-point+)
