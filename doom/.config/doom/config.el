;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jacklyn Daumeyer"
      user-mail-address "jacklynnnnnn@proton.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Karla" :size 14)
      doom-serif-font (font-spec :family "Roboto Slab"))

  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'catppuccin)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/doc/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(map!
        :nv "k" #'evil-previous-visual-line
        :nv "j" #'evil-next-visual-line
        )

(map! :after pdf-tools
      :map pdf-view-mode-map
       "J" #'pdf-view-next-page
      "K" #'pdf-view-previous-page
      "j" #'pdf-view-next-line-or-next-page
      "k" #'pdf-view-previous-line-or-previous-page
      "SPC" #'doom/leader ; I hate having the leader stolen
)

(setq +zen-text-scale 0)

(setq projectile-project-search-path '("~/doc/projects/" "~/doc/org/")
      projectile-globally-ignored-file-suffixes '(".pdf" ".odt" ".class" ".vs" ".s" ".o"))

(setq projectile-switch-project-action #'projectile-dired)
(setq counsel-projectile-switch-project-action #'counsel-projectile-switch-project-action-dired)

(setq-default lsp-java-workspace-dir "/home/jed/doc/osu/cse2221/workspace")
(setq lsp-java-java-path "/lib/jvm/java-11-openjdk/bin/java")

(setq org-babel-default-header-args:java
       '((:cmdline . "-cp .:/home/jed/doc/osu/components.jar")
         (:imports . "components.simplewriter.SimpleWriter1L components.simplewriter.SimpleWriter components.simplereader.SimpleReader components.simplereader.SimpleReader1L")))

(setq TeX-engine 'luatex)
(with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                '("dndnotes"
                  "\\documentclass{dndnotes}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
    ;; American Psychological Association papers
    (add-to-list 'org-latex-classes
                 '("apa7"
                "\\documentclass[stu, 11pt]{apa7}
                 \\usepackage[nodoi]{apacite}"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")))
    (add-to-list 'org-latex-classes
                '("comm3165"
                  "\\documentclass{comm3165}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

)

(setq plantuml-default-exec-mode 'executable)

(after! org

(org-babel-lob-ingest "/home/jed/.doom.d/babel/notes.org")

(org-babel-lob-ingest "/home/jed/.doom.d/babel/dnd.org")

(setq org-capture-templates
  '(("i" "Inbox" entry (file "~/doc/org/inbox.org" )
     "* TODO %? \n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n"
     :kill-buffer t)
    ("n" "Notes" entry (file "~/doc/org/notes.org")
     "* %?\n#+DATE: %U")))

(setq org-publish-project-alist
          '(
            ;;
            ;; MATH3345 Fundementals of Higher Math
            ;;
            ("math3345-notes"
             :base-directory "~/doc/osu/math3345/notes/"
             :base-extension "org"
             :publishing-directory "~/doc/osu/publicnotes/math3345/"
             :index-fn "directory.org"
             :recursive t
             :publishing-function org-html-publish-to-html
             :headline-levels 5
             :auto-preamble t)
            ("math3345-static"
             :base-directory "~/doc/osu/math3345/notes/"
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
             :publishing-directory "~/doc/osu/publicnotes/math3345/"
             :recursive t
             :publishing-function org-publish-attachment)
            ("math3345" :components ("math3345-notes" "math3345-static"))

;;
;; CSE3231 Software Engineering Techniques
;;
("cse3231-notes"
 :base-directory "~/doc/osu/cse3231/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/cse3231/"
 :index-fn "index.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("cse3231-static"
 :base-directory "~/doc/osu/cse3231/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/cse3231/"
 :recursive t
 :publishing-function org-publish-attachment)
("cse3231" :components ("cse3231-notes" "cse3231-static"))

;;
;; ECE 2060 Intro to Digital Logic            ;;
;;
("ece2060-notes"
 :base-directory "~/doc/osu/ece2060/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/ece2060/"
 :index-fn "directory.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("ece2060-static"
 :base-directory "~/doc/osu/ece2060/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/ece2060/"
 :recursive t
 :publishing-function org-publish-attachment)
("ece2060" :components ("ece2060-notes" "ece2060-static"))

;;
;; CSE 2431 Systems 2
;;
("cse2431-notes"
 :base-directory "~/doc/osu/cse2431/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/cse2431/"
 :index-fn "directory.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("cse2431-static"
 :base-directory "~/doc/osu/cse2431/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/cse2431/"
 :recursive t
 :publishing-function org-publish-attachment)
("cse2431" :components ("cse2431-notes" "cse2431-static"))

;;
;; COMM 2367 Persuasive Communication
;;
("comm2367-notes"
 :base-directory "~/doc/osu/comm2367/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/comm2367/"
 :index-fn "directory.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("comm2367-static"
 :base-directory "~/doc/osu/comm2367/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/comm2367/"
 :recursive t
 :publishing-function org-publish-attachment)
("comm2367" :components ("comm2367-notes" "comm2367-static"))

;;
;; MATH 1152 Calculus II
;;
("math1152-notes"
 :base-directory "~/doc/osu/math1152/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/math1152/"
 :index-fn "directory.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("math1152-static"
 :base-directory "~/doc/osu/math1152/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/math1152/"
 :recursive t
 :publishing-function org-publish-attachment)
("math1152" :components ("math1152-notes" "math1152-static"))

;;
;; CSE 2321 Discrete Structures
;;
("cse2421-notes"
 :base-directory "~/doc/osu/cse2421/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/cse2421/"
 :index-fn "index.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("cse2421-static"
 :base-directory "~/doc/osu/cse2421/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/cse2421/"
 :recursive t
 :publishing-function org-publish-attachment)
("cse2421" :components ("cse2421-notes" "cse2421-static"))

;;
;; CSE 2321 Discrete Structures
;;
("cse2321-notes"
 :base-directory "~/doc/osu/past/cse2321/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/cse2321/"
 :index-fn "directory.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("cse2321-static"
 :base-directory "~/doc/osu/past/cse2321/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/cse2321/"
 :recursive t
 :publishing-function org-publish-attachment)
("cse2321" :components ("cse2321-notes" "cse2321-static"))

;;
;; CSE 2331 Data Structures & Algorithms
;;
("cse2331-notes"
 :base-directory "~/doc/osu/cse2331/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/cse2331/"
 :index-fn "directory.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("cse2331-static"
 :base-directory "~/doc/osu/cse2331/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/cse2331/"
 :recursive t
 :publishing-function org-publish-attachment)
("cse2331" :components ("cse2331-notes" "cse2331-static"))

;;
;; CSE 2231 Software Development and Design
;;
("cse2231-notes"
 :base-directory "~/doc/osu/cse2231/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/cse2231/"
 :index-fn "directory.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("cse2231-static"
 :base-directory "~/doc/osu/cse2231/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/cse2231/"
 :recursive t
 :publishing-function org-publish-attachment)
("cse2231" :components ("cse2231-notes" "cse2231-static"))

;;
;; CSE 2221 Software Components
;;
("cse2221-notes"
 :base-directory "~/doc/osu/past/cse2221/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/cse2221/"
 :index-fn "directory.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("cse2221-static"
 :base-directory "~/doc/osu/past/cse2221/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/cse2221/"
 :recursive t
 :publishing-function org-publish-attachment)
("cse2221" :components ("cse2221-notes" "cse2221-static"))

;;
;; Math 1151
;;
("math1151-notes"
 :base-directory "~/doc/osu/past/math1151/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/math1151/"
 :index-fn "directory.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("math1151-static"
 :base-directory "~/doc/osu/past/math1151/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/math1151/"
 :recursive t
 :publishing-function org-publish-attachment)
("math1151" :components ("math1151-notes" "math1151-static"))

;;
;; COMM3165
;;
("comm3165-notes"
 :base-directory "~/doc/osu/comm3165/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/comm3165/"
 :index-fn "directory.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("comm3165-static"
 :base-directory "~/doc/osu/comm3165/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/comm3165/"
 :recursive t
 :publishing-function org-publish-attachment)
("comm3165" :components ("comm3165-notes" "comm3165-static"))

;;
;; COMM3545
;;
("comm3545-notes"
 :base-directory "~/doc/osu/comm3545/notes/"
 :base-extension "org"
 :publishing-directory "~/doc/osu/publicnotes/comm3545/"
 :index-fn "index.org"
 :recursive t
 :publishing-function org-html-publish-to-html
 :headline-levels 5
 :auto-preamble t)
("comm3545-static"
 :base-directory "~/doc/osu/comm3545/notes/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/doc/osu/publicnotes/comm3545/"
 :recursive t
 :publishing-function org-publish-attachment)
("comm3545" :components ("comm3545-notes" "comm3545-static"))

;;
  ;; PHYSICS 1250
  ;;
  ("physics1250-notes"
   :base-directory "~/doc/osu/past/physics1250/notes/"
   :base-extension "org"
   :publishing-directory "~/doc/osu/publicnotes/physics1250/"
   :index-fn "directory.org"
   :recursive t
   :publishing-function org-html-publish-to-html
   :headline-levels 5
   :auto-preamble t)
  ("physics1250-static"
   :base-directory "~/doc/osu/past/physics1250/notes/"
   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
   :publishing-directory "~/doc/osu/publicnotes/physics1250/"
   :recursive t
   :publishing-function org-publish-attachment)
  ("physics1250" :components ("physics1250-notes" "physics1250-static"))

))

(setq org-superstar-headline-bullets-list '("✱" "◉" "●" "○"  "◈" "◇" "➢"  "▣" "□")
      org-ellipsis " ... "
      display-line-numbers-type nil) ;; folding symbol

;; Header Faces
    (set-face-attribute 'org-document-title nil  :weight 'extra-bold)
    (set-face-attribute 'org-level-1        nil  :weight 'extra-bold)
    (set-face-attribute 'org-level-2        nil  :weight 'extra-bold)
    (set-face-attribute 'org-level-3        nil  :weight 'extra-bold)
    (set-face-attribute 'org-level-4        nil  :weight 'extra-bold)
    (set-face-attribute 'org-level-5        nil  :weight 'extra-bold)
    (set-face-attribute 'org-level-6        nil  :weight 'extra-bold)
    (set-face-attribute 'org-level-7        nil  :weight 'extra-bold)
    (set-face-attribute 'org-level-8        nil  :weight 'extra-bold)
    (set-face-attribute 'org-tag            nil :family "Fantasque Sans Mono" :height 1.0)
    ;; Agenda Faces
    (set-face-attribute 'org-agenda-date                  nil :family "Fantasque Sans Mono" :height 1.6)
    (set-face-attribute 'org-agenda-date-today            nil :family "Fantasque Sans Mono" :height 1.6)
    (set-face-attribute 'org-agenda-date-weekend          nil :family "Fantasque Sans Mono" :height 1.6)
    ;;(set-face-attribute 'org-agenda-date-later            nil :family "Roboto Slab" :height 1.5)
    ;;(set-face-attribute 'org-agenda-date-earlier          nil :family "Roboto Slab" :height 1.5)
    (set-face-attribute 'org-agenda-structure             nil :family "Roboto Slab" :height 2.0)
    ;;(set-face-attribute 'org-agenda-deadline-face         nil :foreground "red")
    (setq org-todo-keywords '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "|" "DONE(d)")))
  ;; ☑□⌚e
        ;; show actually italicized text instead of /italicized text/
  ;; Make lists look a bit cooler
  ;;(font-lock-add-keywords 'org-mode
  ;;                      '(("^+\\([-*]\\) "
  ;;                        (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "➥"))))))
;; ➥ →
    )

(add-hook! org-mode :append
           #'variable-pitch-mode)

(add-hook! org-mode :append
           #'visual-fill-column-mode)

(setq org-agenda-block-separator (string-to-char " "))
(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)

(defun my-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date 1 nil))
         (day (cadr date))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month 1))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (weekyear (cond ((and (= month 1) (>= iso-week 52))
                          (1- year))
                         ((and (= month 12) (<= iso-week 1))
                          (1+ year))
                         (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " W%02d" iso-week)
                       "")))
         (format " %s %s/%2d  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
            dayname month day)))

(setq org-agenda-custom-commands
      '(("x" "My Agenda"
          ((agenda "" (;;(org-agenda-skip-scheduled-if-done f)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 7)
                      (org-agenda-include-diary t)
                      (org-agenda-overriding-header "\n✱ SCHEDULE:\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

                      ;;(org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-prefix-format "     %t ")
                       ;;(concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                      ;;(org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-time)
                      (org-agenda-current-time-string "⯇━━━━━━━━━ NOW")
                      ;;(org-agenda-scheduled-leaders '("" ""))
                      ;;(org-agenda-deadline-leaders '("" ""))
                      (org-agenda-time-grid (quote ((today require-timed remove-match) (0800 2100) "      " "━━━━━━━━━━━━━━━")))
                      ))

          (alltodo "" ;; List of all other tasks
                ((org-agenda-overriding-header "✱ TASKS:\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
                (org-agenda-remove-tags t)
                (org-agenda-skip-deadline t)
                (org-agenda-prefix-format "   %t ")
                ;;(org-agenda-todo-keyword-format "")
                (org-agenda-dim-blocked-tasks 'invisible)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))) ;; Ignore any scheduled tasks
        )))))

(setq org-icalendar-store-UID t)

(setq org-use-property-inheritance t)

;;(map! :map gdb-mi
;;      :leader
;;      :prefix ("d". "debug")
;;
;;      :desc "gdb step"  "s" #'
;;
;;                )

(setq +latex-viewers '(pdf-tools))
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;(add-to-list 'org-babel-load-languages '(quandary . t))

(use-package! astro-ts-mode
  :after treesit-auto
  :init
  (when (modulep! +lsp)
  (add-hook 'astro-ts-mode-hook #'lsp! 'append))
  :config
  (global-treesit-auto-mode)
  (let ((astro-recipe (make-treesit-auto-recipe
                       :lang 'astro
                       :ts-mode 'astro-ts-mode
                       :url "https://github.com/virchau13/tree-sitter-astro"
                       :revision "master"
                       :source-dir "src")))
    (add-to-list 'treesit-auto-recipe-list astro-recipe)))

(setq window-divider-default-bottom-width 2  ; default is 1
      window-divider-default-right-width  2)  ; default is 1

(server-start)
