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
        avy-zap
        find-by-pinyin-dired
        pangu-spacing
        visual-fill-column
        org
        evil-escape
        ;; adaptive-wrap
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

(defun han/init-fcitx ()
  (use-package fcitx
    :init
    (fcitx-evil-turn-on)))

(defun han/init-avy-zap ()
  (use-package avy-zap
    :defer t
    :init
    (setq avy-zap-dwim-prefer-avy t)
    (global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)
    (global-set-key (kbd "M-Z") 'avy-zap-up-to-char-dwim)))

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
      (setq pyim-use-tooltip 'popup
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
                 (add-hook 'org-mode-hook
                           '(lambda ()
                              ;; use soft space instead of hard space
                              (setq-local pangu-spacing-real-insert-separtor nil))))))

(defun han/init-visual-fill-column ()
  "Initialize visual-fill-column"
  (use-package visual-fill-column
    :defer t
    :init
    (setq visual-fill-column-width 90)
    (setq visual-line-mode-hook nil)
    ;; Triggering visual-fill-column-mode when visual-line-mode is first turned on
    (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
    ;; 最好将当前buffer的word-wrap设为nil，否则中英文混排时换行都发生在英文单词结束处，非常难看。
    (add-hook 'visual-line-mode-hook
              '(lambda ()
                 (when (bound-and-true-p word-wrap) (setq-local word-wrap nil))))
    :config
    ;; Replace the hook because...
    (remove-hook 'visual-line-mode-hook 'visual-fill-column-mode)
    ;; ...when visual-line-mode is turned off, visual-fill-column-mode
    ;; isn't turned off as expected. This should fix it:
    (add-hook 'visual-line-mode-hook (lambda ()
                                       (if (bound-and-true-p visual-fill-column-mode)
                                           (progn
                                             (visual-fill-column-mode--disable)
                                             (setq-local visual-fill-column-mode nil))
                                         (visual-fill-column-mode--enable)
                                         (setq-local visual-fill-column-mode t))) 'append)))

;; (defun han/post-init-adaptive-wrap ()
;;   (add-hook 'adaptive-wrap-prefix-mode-hook (lambda ()
;;                                               (when (bound-and-true-p word-wrap)
;;                                                 (setq-local word-wrap nil)))))

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

(defun han/post-init-evil-escape ()
  "Stop evil-escape from interrupting input from pyim, especially
when you need to hit Enter while in pyim to fast input English"
  (defun input-method-is-on-p ()
    (bound-and-true-p current-input-method))
  (setq evil-escape-inhibit-functions
        (append '(input-method-is-on-p) evil-escape-inhibit-functions)))

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
