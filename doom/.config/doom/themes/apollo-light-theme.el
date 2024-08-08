;;; themes/apollo-light-theme.el -*- lexical-binding: t; -*-

;;; themes/apollo-theme.el -*- lexical-binding: t; -*-
(require 'doom-themes)


(def-doom-theme apollo-light
  "A nice org aware light mode for emacs"
  ((fg '("#2a2929" "black" "black"))
   (fg-alt '("#655c5a" "black" "gray"))
   (bg '("#e4e4e5" "white" "white"))
   (bg-alt '("#e4e4e5" "lightgray" "gray"))

   ;; black
   (base0 "#2a2929")
   ;; red
   (base1 "#a54242")
   ;; green
   (base2 "#60b48a")
   (base3 "#de935f")
   (base4 "#5f819d")
   (base5 "#85678f")
   (base6 "#5e8d87")
   (base7 "#89908f")
   (base8 "#655c5a")

   (blue        "#81a2be")
   (red         '("#cc6666" "red" "red"))
   (green       "#60b48a")
   (yellow      "#d0b75a")
   (orange      '("#de935f"))
   (dark-blue   "#5f819d")
   (magenta     "#85678f")
   (violet      "#b294bb")
   (cyan        "#5e8d87")
   (teal        "#5e8d87")
   (dark-cyan   "#8abeb7")
   (grey        "#655c5a")


   ;; Colors required by Doom to run
   (highlight   blue)
   (selection   blue)
   (comments    fg-alt)
   (doc-comments        fg-alt)
   (type        blue)
   (strings     green)
   (error       red)
   (numbers     yellow)
   (operators   yellow)
   (methods     fg)
   (functions   orange)
   (keywords    blue)
   (variables   fg)
   (warning     orange)
   (success     green)
   (constants   blue)
   (region      grey)
   (vertical-bar magenta)
   (builtin     magenta)

   (vc-modified yellow)
   (vc-added    green)
   (vc-deleted  red))


  ;; Begin Custom Face Definitions
  ((cursor :background grey)
   (tab-line :background bg)
   ;; Org Mode faces
   (org-document-title :foreground fg :height 3.0)
   (org-level-1 :foreground fg       :height 1.8)
   (org-level-2 :foreground blue     :height 1.6)
   (org-level-3 :foreground green    :height 1.4)
   (org-level-4 :foreground violet   :height 1.2)
   (org-level-5 :foreground orange   :height 1.1)
   (org-level-6 :foreground blue     :height 1.1)
   (org-level-7 :foreground green    :height 1.0)
   (org-level-8 :foreground violet   :height 1.0)

   (org-block :foreground fg :background bg :inherit 'fixed-pitch)
   (org-block-begin-line :foreground fg :background grey :inherit 'fixed-pitch)
   (org-block-end-line :foreground fg :background grey :inherit 'fixed-pitch)

   (org-table :inherit 'fixed-pitch)
   (org-table-header :interit 'fixed-pitch)

   (org-verse :foreground grey :background nil)
   (org-quote :foreground grey :background nil)

   (org-done :foreground grey)


   ;;Evil
   (evil-ex-search :foreground fg-alt :background yellow)
   (evil-ex-lazy-highlight :foreground fg-alt :background yellow)

   ;; Ivy
   (ivy-current-match :foreground yellow :weight 'bold)
   (ivy-minibuffer-match-face-1 :foreground blue :background nil :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground blue :background nil :underline t)
   (ivy-subdir :foreground violet :weight 'bold)
   (ivy-prompt-match :foreground fg)

   ;; Company
   (company-preview-common :foreground fg :background grey)
   (company-tooltip :forground fg :background grey)
   (company-scrollbar-bg :background grey)
   (company-scrollbar-fg :background fg)

   )
)
