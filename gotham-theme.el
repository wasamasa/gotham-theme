;;; gotham-theme.el --- A very dark Emacs color theme.

;; Copyright (C) 2014 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/gotham-theme
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A port of the Gotham theme for Vim to Emacs 24 that uses the new
;; theming support.

;;; Credits:
;; Andrea Leopardi created the original Vim theme on which this port
;; is based, it can be found under
;; <https://github.com/whatyouhide/vim-gotham>

;;; Code:

(deftheme gotham "The Gotham color theme")

;; TODO 16-color support for terminals
;; TODO offer 256 color switch?

(let ((base0   "#0c1014")
      (base1   "#11151c")
      (base2   "#091f2e")
      (base3   "#0a3749")
      (base4   "#245361")
      (base5   "#599cab")
      (base6   "#99d1ce")
      (base7   "#d3ebe9")

      (red     "#c23127")
      (orange  "#d26937")
      (yellow  "#edb443")
      (magenta "#888ca6")
      (violet  "#4e5166")
      (blue    "#195466")
      (cyan    "#33859e")
      (green   "#2aa889"))

  (custom-theme-set-faces
   'gotham
   `(default ((t (:foreground ,base6 :background ,base0))))
   `(button ((t (:foreground ,base4 :box t))))
   `(shadow ((t (:foreground ,base4))))
   `(highlight ((t (:background ,base2))))
   `(link ((t (:foreground ,orange))))
   `(link-visited ((t (:foreground ,yellow))))
   `(cursor ((t (:background ,base6))))
   `(region ((t (:foreground unspecified :background ,base3))))
   `(secondary-selection ((t (:foreground unspecified :background ,base4))))
   `(hl-line ((t (:background ,base1))))
   `(linum ((t (:foreground ,base4 :background ,base1))))
   `(fringe ((t (:foreground ,base6 :background ,base1))))
   `(vertical-border ((t (:foreground ,base4))))
   `(tooltip ((t (:foreground ,base6 :background ,base0))))
   `(trailing-whitespace ((t (:background ,red))))

   ;; font-lock
   `(escape-glyph ((t (:foreground ,orange :weight bold))))
   `(font-lock-builtin-face ((t (:foreground ,orange))))
   `(font-lock-comment-face ((t (:foreground ,base4))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,base4))))
   `(font-lock-constant-face ((t (:foreground ,magenta))))
   `(font-lock-doc-face ((t (:foreground ,green))))
   `(font-lock-function-name-face ((t (:foreground ,base5))))
   `(font-lock-keyword-face ((t (:foreground ,blue))))
   `(font-lock-negation-char-face ((t (:foreground ,red))))
   `(font-lock-preprocessor-face ((t (:foreground ,red))))
   `(font-lock-regexp-grouping-construct)
   `(font-lock-regexp-grouping-backslash)
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,orange))))
   `(font-lock-variable-name-face ((t (:foreground ,base5))))
   `(font-lock-warning-face ((t (:foreground ,red))))
   `(success ((t (:foreground ,green))))
   `(warning ((t (:foreground ,red))))

   ;; search and highlighting
   `(match ((t (:background ,base5))))
   `(isearch ((t (:inverse-video t))))
   `(isearch-fail ((t (:foreground ,red))))
   `(lazy-highlight ((t (:foreground ,base2 :background ,yellow))))
   `(show-paren-match ((t (:foreground ,base1 :background ,orange))))
   `(show-paren-mismatch ((t (:foreground ,base1 :background ,red))))

   ;; mode and header lines
   `(minibuffer-prompt ((t (:foreground ,cyan))))
   `(header-line ((t (:foreground ,base5 :background ,base2))))
   `(menu ((t (:background ,base3 :foreground ,base6))))
   `(mode-line ((t (:foreground ,base5 :background ,base2 :box nil))))
   `(mode-line-inactive ((t (:foreground ,base4 :background ,base1 :box nil))))
   `(mode-line-highlight ((t (:foreground ,base6))))
   `(mode-line-buffer-id ((t (:weight bold))))

   ;; customize
   `(custom-button ((t (:foreground ,base4 :box t))))
   `(custom-button-mouse ((t (:foreground ,base5 :box t))))
   `(custom-group-tag ((t (:inherit fixed-pitch :foreground ,magenta))))

   ;; external packages

   ;; enh-ruby-mode
   `(enh-ruby-heredoc-delimiter-face ((t (:foreground ,green :weight bold))))
   `(enh-ruby-op-face ((t (:foreground ,magenta))))
   `(enh-ruby-regexp-face ((t (:foreground ,green))))
   `(enh-ruby-regexp-delimiter-face ((t (:foreground ,green :weight bold))))
   `(enh-ruby-string-delimiter-face ((t (:foreground ,green))))
   `(erm-syn-errline ((t (:foreground ,red))))
   `(erm-syn-warnline ((t (:foreground ,orange))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'gotham)

;;; gotham-theme.el ends here
