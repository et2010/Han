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
      '(ace-pinyin
        (adaptive-wrap :excluded t)
        avy-zap
        (chinese-pyim :toggle (eq han-default-input-method 'pinyin))
        (chinese-wbim :toggle (eq han-default-input-method 'wubi))
        evil-escape
        find-by-pinyin-dired
        (fcitx :toggle han-enable-fcitx)
        pangu-spacing
        org
        org-cliplink
        visual-fill-column
        (youdao-dictionary :toggle han-enable-youdao-dict)))

(defun han/init-org-cliplink ()
  (use-package org-cliplink
    :defer t))

(defun han/init-fcitx ()
  (use-package fcitx
    :init
    (progn
      (setq fcitx-use-dbus t)
      (setq fcitx-active-evil-states '(insert emacs hybrid))
      (fcitx-prefix-keys-add "M-m")
      (fcitx-aggressive-setup))))

(defun han/init-avy-zap ()
  (use-package avy-zap
    :defer t
    :init
    (progn (setq avy-zap-dwim-prefer-avy t)
           (global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)
           (global-set-key (kbd "M-Z") 'avy-zap-up-to-char-dwim))))

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
      (setq default-input-method "chinese-pyim")
      (setq pyim-use-tooltip 'pos-tip)
      ;; (setq pyim-enable-words-predict nil)
      (setq pyim-dicts-directory (concat user-emacs-directory "private/han")
            pyim-personal-file (concat spacemacs-cache-directory "pyim/pyim-personal.txt")
            pyim-property-file (concat spacemacs-cache-directory "pyim/pyim-words-property.txt")
            pyim-dicts
            '((:name "sogou" :file "~/.emacs.d/private/han/pyim-sgcore.pyim" :coding utf-8-unix :dict-type pinyin-dict)))
      (setq pyim-isearch-enable-pinyin-search t
            isearch-search-fun-function 'pyim-isearch-pinyin-search-function)
      (setq-default pyim-english-input-switch-functions '(pyim-probe-isearch-mode
                                                          pyim-probe-org-speed-commands
                                                          pyim-probe-org-structure-template
                                                          ;; pyim-probe-dynamic-english
                                                          pyim-probe-program-mode))
      (setq-default pyim-punctuation-half-width-functions '(pyim-probe-punctuation-after-punctuation
                                                            pyim-probe-punctuation-line-beginning))
      (define-key evil-hybrid-state-map (kbd "M-f") 'pyim-forward-word)
      (define-key evil-hybrid-state-map (kbd "M-b") 'pyim-backward-word)
      (evilified-state-evilify pyim-dicts-manager-mode pyim-dicts-manager-mode-map))
    ))

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
      (setq ace-pinyin-simplified-chinese-only-p nil)
      (ace-pinyin-global-mode t)
      (spacemacs|hide-lighter ace-pinyin-mode))))

(defun han/init-pangu-spacing ()
  (use-package pangu-spacing
    :defer t
    :init
    (progn
      (global-pangu-spacing-mode 1)  ;; enable pangu-spacing by default
      (spacemacs|hide-lighter pangu-spacing-mode)
      (add-hook 'org-mode-hook
                '(lambda ()
                   ;; use hard space instead of soft space
                   (setq-local pangu-spacing-real-insert-separtor nil)))

      (with-eval-after-load "org-element"
        (defadvice pangu-spacing-search-and-replace (around pangu-spacing-org-link-fix activate)
          "Addvise the function not to replace the match when one
of the match group is from an org-link element"
          (if (not (eq 'org-mode (buffer-local-value 'major-mode (current-buffer))))
              ad-do-it
            (pangu-spacing-search-buffer
             regexp (point-min) (point-max)
             (when (not (member 'link
                                (save-match-data
                                  (save-excursion
                                    (let ((p1 (match-beginning 1))
                                          (p2 (match-beginning 2)))
                                      (mapcar (lambda (pt) (goto-char pt)
                                                (org-element-type
                                                 (org-element-context)))
                                              (list p1 p2)))))))
               (replace-match match nil nil)))))))))

(defun han/init-visual-fill-column ()
  "Initialize visual-fill-column"
  (use-package visual-fill-column
    :defer t
    :init
    (progn
      (setq visual-fill-column-width 90)
      (setq visual-line-mode-hook nil)
      (add-hook 'visual-line-mode-hook
                '(lambda ()
                   ;; Triggering visual-fill-column-mode when visual-line-mode is first turned on
                   (visual-fill-column-mode)
                   (spacemacs|hide-lighter visual-line-mode)))
      ;; 最好将当前buffer的word-wrap设为nil，否则中英文混排时换行都发生在英文单词结束处，非常难看。
      (add-hook 'visual-line-mode-hook
                '(lambda ()
                   (when (bound-and-true-p word-wrap) (setq-local word-wrap nil)))))
    :config
    (progn
      ;; Replace the hook because...
      (remove-hook 'visual-line-mode-hook
                   '(lambda ()
                      (visual-fill-column-mode)
                      (spacemacs|hide-lighter visual-line-mode)))
      ;; ...when visual-line-mode is turned off, visual-fill-column-mode
      ;; isn't turned off as expected. This should fix it:
      (add-hook 'visual-line-mode-hook (lambda ()
                                         (if (bound-and-true-p visual-fill-column-mode)
                                             (progn
                                               (visual-fill-column-mode--disable)
                                               (setq-local visual-fill-column-mode nil))
                                           (visual-fill-column-mode--enable)
                                           (setq-local visual-fill-column-mode t))) 'append))))

(defun han/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-init
    (progn
      (setq org-highlight-latex-and-related '(latex script entities))
      (add-hook 'org-mode-hook '(lambda ()
                                  (variable-pitch-mode)
                                  (spacemacs|hide-lighter buffer-face-mode)))
      (add-hook 'org-mode-hook '(lambda () (setq-local line-spacing han-org-line-spacing)))
      (eval-after-load "org"
        '(mapc
          (lambda (face)
            (set-face-attribute
             face nil
             :inherit
             (han-adjoin-to-list-or-symbol
              'fixed-pitch
              (face-attribute face :inherit))))
          (list 'org-checkbox
                'org-code
                'org-block
                'org-block-begin-line
                'org-block-end-line
                'org-document-info-keyword
                'org-done
                'org-formula
                'org-latex-and-related
                'org-meta-line
                ;; 'org-link
                'org-special-keyword
                'org-table
                'org-todo
                'org-verbatim
                ))))))

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
  (push (lambda () (bound-and-true-p current-input-method))
        evil-escape-inhibit-functions))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
