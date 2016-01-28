;;; config.el --- Han Layer Configuration File for Spacemacs
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

;; Variables

(defvar han-default-input-method 'pinyin
  "The default chiense input method. Can be `wubi` or `pinyin`.")

(defvar han-enable-youdao-dict nil
  "Enble YouDao Dict translation service.")

(defvar han-enable-avy-pinyin t
  "Enable ace-pinyin in avy-goto-char")

(defvar han-enable-fcitx nil
  "Enable fcitx to help writing Chinese in Evil mode.")

(defun han-adjoin-to-list-or-symbol (element list-or-symbol)
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    ;; (require 'cl-lib)
    (cl-adjoin element list)))

;; Set the monospaced font size when mixed Chinese and English words
(defun spacemacs//set-monospaced-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (set-face-attribute 'fixed-pitch nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (set-face-attribute 'variable-pitch nil :font
                      (format   "%s:pixelsize=%d"  "DejaVu Sans Condensed" english-size))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (format "-*-%s-*-*-*-*-%d-*-*-*-*-*-*-*" chinese chinese-size))))

;; If the Hiragino Sans GB font is not found in your system, you could call this
;; method in dotspacemacs/user-config function with a different Chinese font name.
;; If you are using mac, you could put the following code in your dotspacemacs/user-config function.
;; (when (spacemacs/system-is-mac)
;;   (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16))

;; EOF
