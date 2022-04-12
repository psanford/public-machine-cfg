(eval-when-compile
  (require 'color-theme))

(defun color-theme-nearbuy ()
  "Default colors for nearbuy"
  (interactive)
  (let ((base03  "#032029")
        (base02  "#0a2833")
        (base01  "#465162")
        (base00  "#526770")
        (base0   "#708184")
        (base1   "#818f8f")
        (base2   "#e9e3c8")
        (base3   "#fcf4d9")
        (yellow  "#a57800")
        (orange  "#bc3800")
        (red     "#d01e1d")
        (magenta "#c51c72")
        (violet  "#5758be")
        (blue    "#1e74ce")
        (cyan    "#269185")
        (green   "#738a00"))

  (color-theme-install
   `(color-theme-nearbuy
     ((foreground-color . "light grey")
      (background-color . ,base03)
      (cursor-color . "dark orchid")
      (background-mode . dark))
     (default ((t (nil))))

     (mode-line
      ((t (:foreground ,base1 :background ,base02
                       :box (:line-width 1 :color ,base1)))))
     (mode-line-buffer-id ((t (:foreground ,base1))))
     (mode-line-inactive
      ((t (:foreground ,base0  :background ,base02
                       :box (:line-width 1 :color ,base02)))))

     (vertical-border ((t (:foreground ,base0))))

     (diff-added ((nil (:foreground "dark turquoise"))))
     (diff-changed ((nil (:foreground ,yellow))))
     (diff-removed ((nil (:foreground "violet"))))

     (magit-diff-del ((nil (:foreground "violet"))))
     (magit-item-highlight ((nil (:background ,base02))))
     (magit-diff-add ((nil (:foreground "dark turquoise"))))


     (cperl-array-face ((nil (:foreground "gold"))))
     (cperl-hash-face ((nil (:foreground "firebrick1"))))

     (trailing-whitespace ((nil (:background "grey30"))))
     (region ((t (:background "blue3"))))



     ;; (italic ((t (:foreground "dark red" :italic t))))
     ;; (bold-italic ((t (:foreground "dark red" :bold t :italic t))))
     ;; (font-lock-comment-face ((t (:foreground "Firebrick"))))

       ;; diff
     ;; (diff-added ((t (:foreground "dark turquoise" :inverse-video t))))
     ;; (diff-changed ((t (:foreground "#a57800" :inverse-video t))))
     ;; (diff-removed ((t (:foreground "violet" :inverse-video t))))


     (bold ((t (:bold))))))))

(provide 'color-theme-nearbuy)
