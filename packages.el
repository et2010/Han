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
        ;; pinyin-search
        ace-pinyin
        find-by-pinyin-dired
        pangu-spacing
        visual-fill-column
        org
        ))

;; List of packages to exclude.
(setq gtd-excluded-packages '(adaptive-wrap))

(if han-enable-youdao-dict
    (push 'youdao-dictionary han-packages))

(if (eq han-default-input-method 'wubi)
    (push 'chinese-wbim han-packages)
  (push 'chinese-pyim han-packages))

(if (and han-enable-fcitx (not (spacemacs/system-is-mswindows))) ;; disable in Windows
    (push 'fcitx han-packages))

(defun chinese/init-fcitx ()
  (use-package fcitx
    :init
    (fcitx-evil-turn-on)))

(defun han/init-chinese-wbim ()
  "Initialize chinese-wubi"
  (use-package chinese-wbim
    :if (eq 'wubi han-default-input-method)
    :init
    (progn
      (autoload 'chinese-wbim-use-package "chinese-wubi"
        "Another emacs input method")
      ;; Tooptip is not good enough, so disable it here.
      (setq chinese-wbim-use-tooltip nil)
      (register-input-method
       "chinese-wubi" "euc-cn" 'chinese-wbim-use-package
       "五笔" "汉字五笔输入法" "wb.txt")
      (require 'chinese-wbim-extra)
      (global-set-key ";" 'chinese-wbim-insert-ascii)
      (setq default-input-method 'chinese-wubi))))

(defun han/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :if han-enable-youdao-dict
    :defer
    :config
    (progn
      ;; Enable Cache
      (setq url-automatic-caching t
            ;; Set file path for saving search history
            youdao-dictionary-search-history-file
            (concat spacemacs-cache-directory ".youdao")
            ;; Enable Chinese word segmentation support
            youdao-dictionary-use-chinese-word-segmentation t))))

(defun han/init-chinese-pyim ()
  (use-package chinese-pyim
    :if (eq 'pinyin han-default-input-method)
    :init
    (progn
      (setq pyim-use-tooltip nil
            ;; pyim-enable-words-predict nil
            pyim-dicts-directory spacemacs-cache-directory
            pyim-personal-file (concat spacemacs-cache-directory
                                       "pyim-personal.txt")
            default-input-method "chinese-pyim")
      (evilified-state-evilify pyim-dicts-manager-mode pyim-dicts-manager-mode-map))))

(defun han/init-find-by-pinyin-dired ()
  (use-package find-by-pinyin-dired
    :defer t))

(defun han/init-ace-pinyin ()
  (use-package ace-pinyin
    :defer t
    :init
    (progn
      (if han-enable-avy-pinyin
          (setq ace-pinyin-use-avy t))
      (ace-pinyin-global-mode t)
      (spacemacs|hide-lighter ace-pinyin-mode))))

(defun han/init-pangu-spacing ()
  (use-package pangu-spacing
    :defer t
    :init (progn (global-pangu-spacing-mode -1)  ;; disable pangu-spacing by default
                 (spacemacs|hide-lighter pangu-spacing-mode)
                 ;; Always insert `real' space in org-mode.
                 (add-hook 'org-mode-hook
                           '(lambda ()
                              ;; use soft space instead of hard space
                              (set (make-local-variable 'pangu-spacing-real-insert-separtor) nil))))))

(defun han/post-init-org ()
  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents))))

(defun han/init-visual-fill-column ()
  "Initialize visual-fill-column"
  (use-package visual-fill-column
    :defer t
    :init
    (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
    ;; 最好将word-wrap的值设为nil，否则中英文混排时换行都发生在英文单词结束处，非常难看。
    (add-hook 'visual-line-mode-hook
              '(lambda ()
                 (progn
                   (set (make-local-variable 'word-wrap) nil)
                 (set-fill-column 90))))))

;; (defun han/init-pinyin-search ()
;;   "Initialize pinyin-search"
;;   (use-package pinyin-search
;;     :defer t
;;     :init
;;     (global-unset-key (kbd "C-s"))
;;     (global-unset-key (kbd "C-r"))
;;     (define-key evil-emacs-state-map (kbd "C-s") 'isearch-forward)
;;     (define-key evil-emacs-state-map (kbd "C-r") 'isearch-backward)
;;     (define-key evil-normal-state-map (kbd "C-s") 'isearch-forward-pinyin)
;;     (define-key evil-normal-state-map (kbd "C-r") 'isearch-backward-pinyin)))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
