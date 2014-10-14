;;; gotham-theme.el --- A very dark Emacs color theme.

;; Copyright (C) 2014 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/gotham-theme
;; Version: 0.2

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

(defgroup gotham nil
  "A very dark Emacs theme"
  :prefix "gotham-")

(deftheme gotham "The Gotham color theme")

;; TODO test with ruby, python, tex, c, html

(defcustom gotham-tty-extended-palette-p nil
  "Use the extended 256-color palette in the terminal?
When t, assume a regular 256-color palette, otherwise assume a
customized 16-color palette."
  :type '(boolean (const :tag "16 colors" nil)
                  (const :tag "256 colors" t))
  :group 'gotham)

(setq gotham-tty-switch 'extended)
(defvar gotham-color-alist
  `((base0   "#0c1014" ,(if gotham-tty-extended-palette-p "color-232" "black"))
    (base1   "#11151c" ,(if gotham-tty-extended-palette-p "color-233" "brightblack"))
    (base2   "#091f2e" ,(if gotham-tty-extended-palette-p "color-17"  "brightgreen"))
    (base3   "#0a3749" ,(if gotham-tty-extended-palette-p "color-18"  "brightblue"))
    (base4   "#245361" ,(if gotham-tty-extended-palette-p "color-24"  "brightyellow"))
    (base5   "#599cab" ,(if gotham-tty-extended-palette-p "color-81"  "brightcyan"))
    (base6   "#99d1ce" ,(if gotham-tty-extended-palette-p "color-122" "white"))
    (base7   "#d3ebe9" ,(if gotham-tty-extended-palette-p "color-194" "brightwhite"))

    (red     "#c23127" ,(if gotham-tty-extended-palette-p "color-124" "red"))
    (orange  "#d26937" ,(if gotham-tty-extended-palette-p "color-166" "brightred"))
    (yellow  "#edb443" ,(if gotham-tty-extended-palette-p "color-214" "yellow"))
    (magenta "#888ca6" ,(if gotham-tty-extended-palette-p "color-67"  "brightmagenta"))
    (violet  "#4e5166" ,(if gotham-tty-extended-palette-p "color-60"  "magenta"))
    (blue    "#195466" ,(if gotham-tty-extended-palette-p "color-24"  "blue"))
    (cyan    "#33859e" ,(if gotham-tty-extended-palette-p "color-44"  "cyan"))
    (green   "#2aa889" ,(if gotham-tty-extended-palette-p "color-78"  "green")))
  "List of colors the theme consists of.")

(defun gotham-set-faces (faces)
  "Helper function that sets the faces of the theme to a list of FACES.
See `gotham-transform-face' for the transformation, see
`gotham-transform-spec' for the rules."
  (apply #'custom-theme-set-faces 'gotham
         (mapcar #'gotham-transform-face faces)))

(defun gotham-transform-face (face)
  "Helper function that transforms a FACE to all variants.
FACE is a list where the first element is the name of the
affected face and the remaining elements specify the face
attributes which are transformed into face attributes for both
graphical and terminal displays. See `gotham-transform-spec' for
the rules that are applied to the face attributes."
  (let* ((name (car face))
         (spec (cdr face))
         (graphic-spec (gotham-transform-spec spec 'graphic))
         (tty-spec (gotham-transform-spec spec 'tty)))
    `(,name ((((type graphic)) ,@graphic-spec)
             (((type tty)) ,@tty-spec)))))

(defun gotham-transform-spec (spec display)
  "Helper function that transforms SPEC for DISPLAY.
DISPLAY is either 'graphic or 'tty, SPEC is a property list where
the values are substituted with colors from `gotham-color-alist'
depending on DISPLAY for keys which are either :foreground or
:background.  All other key-value combinations remain unchanged."
  (let (output)
    (while spec
      (let* ((key (car spec))
             (value (cadr spec))
             (index (cond ((eq display 'graphic) 1)
                          ((eq display 'tty) 2)))
             (color (nth index (assoc value gotham-color-alist))))
        (cond
         ((or (eq key :foreground) (eq key :background))
          (setq output (append output (list key color))))
         (t (setq output (append output (list key value))))))
      (setq spec (cddr spec)))
    output))

(gotham-set-faces
 '((default :foreground base6 :background base0)
   (button :foreground base4 :box t)
   (shadow :foreground base4)
   (highlight :background base2)
   (link :foreground orange)
   (link-visited :foreground yellow)
   (cursor :background base6)
   (region :foreground unspecified :background base3)
   (secondary-selection :foreground unspecified :background base4)
   (hl-line :background base1)
   (linum :foreground base4 :background base1)
   (fringe :foreground base6 :background base1)
   (vertical-border :foreground base4)
   (tooltip :foreground base6 :background base0)
   (trailing-whitespace :background red)

   ;; font-lock
   (escape-glyph :foreground orange :weight bold)
   (font-lock-builtin-face :foreground orange)
   (font-lock-comment-face :foreground base4)
   (font-lock-comment-delimiter-face :foreground base4)
   (font-lock-constant-face :foreground cyan :weight bold)
   (font-lock-doc-face :foreground green)
   (font-lock-function-name-face :foreground base5)
   (font-lock-keyword-face :foreground blue :weight bold)
   (font-lock-negation-char-face :foreground red)
   (font-lock-preprocessor-face :foreground red)
   (font-lock-regexp-grouping-construct)
   (font-lock-regexp-grouping-backslash)
   (font-lock-string-face :foreground green)
   (font-lock-type-face :foreground orange)
   (font-lock-variable-name-face :foreground base5)
   (font-lock-warning-face :foreground red)
   (success :foreground green)
   (warning :foreground red)

   ;; search and highlighting
   (match :background base5)
   (isearch :inverse-video t)
   (isearch-fail :foreground red)
   (lazy-highlight :foreground base2 :background yellow)
   (show-paren-match :foreground base1 :background orange)
   (show-paren-mismatch :foreground base1 :background red)

   ;; mode and header lines
   (minibuffer-prompt :foreground cyan)
   (header-line :foreground base5 :background base2)
   (menu :background base3 :foreground base6)
   (mode-line :foreground base5 :background base2 :box nil)
   (mode-line-inactive :foreground base4 :background base1 :box nil)
   (mode-line-highlight :foreground base6)
   (mode-line-buffer-id :weight bold)

   ;; customize
   (custom-button :foreground base4 :box t)
   (custom-button-mouse :foreground base5 :box t)
   (custom-group-tag :inherit fixed-pitch :foreground magenta)

   ;; external packages

   ;; enh-ruby-mode
   (enh-ruby-heredoc-delimiter-face :foreground green :weight bold)
   (enh-ruby-op-face :foreground magenta)
   (enh-ruby-regexp-face :foreground green)
   (enh-ruby-regexp-delimiter-face :foreground green :weight bold)
   (enh-ruby-string-delimiter-face :foreground green)
   (erm-syn-errline :foreground red)
   (erm-syn-warnline :foreground orange)

   ;; auctex
   (font-latex-bold-face :inherit bold)
   (font-latex-doctex-documentation-face :inherit highlight)
   (font-latex-doctex-preprocessor-face :inherit highlight :foreground red)
   (font-latex-italic-face :inherit italic)
   (font-latex-math-face :foreground base5)
   (font-latex-sectioning-0-face :inherit bold :foreground yellow)
   (font-latex-sectioning-1-face :inherit bold :foreground yellow)
   (font-latex-sectioning-2-face :inherit bold :foreground yellow)
   (font-latex-sectioning-3-face :inherit bold :foreground yellow)
   (font-latex-sectioning-4-face :inherit bold :foreground yellow)
   (font-latex-sectioning-5-face :inherit bold :foreground yellow)
   (font-latex-sedate-face :foreground orange)
   (font-latex-slide-title-face :inherit bold :foreground orange)
   (font-latex-string-face :inherit font-lock-string-face)
   (font-latex-verbatim-face :inherit font-lock-string-face)
   (font-latex-warning-face :inherit warning)
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'gotham)

;;; gotham-theme.el ends here
