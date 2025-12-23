;;; hiro-one-dark-theme.el --- Zed One Dark inspired theme built on modus-themes -*- lexical-binding:t -*-

;; Copyright (C) 2025

;; Author: Hiro
;; URL: https://github.com/fangyuan/dotfiles
;; Keywords: faces, theme

;;; Commentary:
;;
;; A dark theme based on Zed Editor's "One Dark" color palette,
;; built on top of the modus-themes infrastructure for comprehensive
;; face coverage and accessibility.

;;; Code:

(require 'modus-themes)

(defconst hiro-one-dark-palette
  (append
   '(
     ;; Basic values (from Zed One Dark)
     (bg-main          "#282c33")
     (bg-dim           "#2f343e")
     (fg-main          "#abb2bf")
     (fg-dim           "#5d636f")
     (fg-alt           "#a9afbc")
     (bg-active        "#454a56")
     (bg-inactive      "#363c46")
     (border           "#464b57")

     ;; Common accent foregrounds (from Zed One Dark syntax)
     (red             "#e06c75")
     (red-warmer      "#d07277")
     (red-cooler      "#be5046")
     (red-faint       "#ff9580")
     (red-intense     "#e06c75")
     (green           "#98c379")
     (green-warmer    "#a1c181")
     (green-cooler    "#98c379")
     (green-faint     "#88ca9f")
     (green-intense   "#98c379")
     (yellow          "#e5c07b")
     (yellow-warmer   "#dfc184")
     (yellow-cooler   "#bf956a")
     (yellow-faint    "#d2b580")
     (yellow-intense  "#e5c07b")
     (blue            "#61afef")
     (blue-warmer     "#74ade8")
     (blue-cooler     "#61afef")
     (blue-faint      "#82b0ec")
     (blue-intense    "#61afef")
     (magenta         "#c678dd")
     (magenta-warmer  "#b477cf")
     (magenta-cooler  "#c678dd")
     (magenta-faint   "#caa6df")
     (magenta-intense "#c678dd")
     (cyan            "#56b6c2")
     (cyan-warmer     "#6eb4bf")
     (cyan-cooler     "#56b6c2")
     (cyan-faint      "#9ac8e0")
     (cyan-intense    "#56b6c2")

     ;; Uncommon accent foregrounds
     (rust       "#bf956a")
     (gold       "#dfc184")
     (olive      "#a1c181")
     (slate      "#74ade8")
     (indigo     "#b477cf")
     (maroon     "#d07277")
     (pink       "#b477cf")

     ;; Common accent backgrounds
     (bg-red-intense     "#9d1f1f")
     (bg-green-intense   "#2f822f")
     (bg-yellow-intense  "#7a6100")
     (bg-blue-intense    "#1640b0")
     (bg-magenta-intense "#7030af")
     (bg-cyan-intense    "#2266ae")

     (bg-red-subtle      "#4f1119")
     (bg-green-subtle    "#00381f")
     (bg-yellow-subtle   "#363300")
     (bg-blue-subtle     "#242679")
     (bg-magenta-subtle  "#552f5f")
     (bg-cyan-subtle     "#004065")

     (bg-red-nuanced     "#3a0c14")
     (bg-green-nuanced   "#092f1f")
     (bg-yellow-nuanced  "#381d0f")
     (bg-blue-nuanced    "#12154a")
     (bg-magenta-nuanced "#2f0c3f")
     (bg-cyan-nuanced    "#042837")

     ;; Uncommon accent background and foreground pairs
     (bg-clay     "#49191a")
     (fg-clay     "#f1b090")

     (bg-ochre    "#462f20")
     (fg-ochre    "#e0d38c")

     (bg-lavender "#38325c")
     (fg-lavender "#dfc0f0")

     (bg-sage     "#143e32")
     (fg-sage     "#c3e7d4")

     ;; Graphs
     (bg-graph-red-0     "#b52c2c")
     (bg-graph-red-1     "#702020")
     (bg-graph-green-0   "#0fed00")
     (bg-graph-green-1   "#007800")
     (bg-graph-yellow-0  "#f1e00a")
     (bg-graph-yellow-1  "#b08940")
     (bg-graph-blue-0    "#2fafef")
     (bg-graph-blue-1    "#1f2f8f")
     (bg-graph-magenta-0 "#bf94fe")
     (bg-graph-magenta-1 "#5f509f")
     (bg-graph-cyan-0    "#47dfea")
     (bg-graph-cyan-1    "#00808f")

     ;; Special purpose
     (bg-completion       "#74ade83d")
     (bg-hover            "#45605e")
     (bg-hover-secondary  "#654a39")
     (bg-hl-line          "#2f3849")
     (bg-region           "#5a5a5a")
     (fg-region           "#ffffff")

     (bg-mode-line-active        "#3b414d")
     (fg-mode-line-active        "#dce0e5")
     (border-mode-line-active    "#5a5a5a")
     (bg-mode-line-inactive      "#2e343e")
     (fg-mode-line-inactive      "#878a98")
     (border-mode-line-inactive  "#363c46")

     (modeline-err     "#e06c75")
     (modeline-warning "#e5c07b")
     (modeline-info    "#61afef")

     (bg-tab-bar      "#2f343e")
     (bg-tab-current  "#282c33")
     (bg-tab-other    "#3b414d")

     ;; Diffs
     (bg-added           "#003a2f")
     (bg-added-faint     "#002910")
     (bg-added-refine    "#034f2f")
     (bg-added-fringe    "#27a657")
     (fg-added           "#a1c181")
     (fg-added-intense   "#98c379")

     (bg-changed         "#363300")
     (bg-changed-faint   "#2a1f00")
     (bg-changed-refine  "#4a4a00")
     (bg-changed-fringe  "#d3b020")
     (fg-changed         "#dec184")
     (fg-changed-intense "#e5c07b")

     (bg-removed         "#4f1119")
     (bg-removed-faint   "#380a0f")
     (bg-removed-refine  "#781a1f")
     (bg-removed-fringe  "#e06c76")
     (fg-removed         "#d07277")
     (fg-removed-intense "#e06c75")

     (bg-diff-context    "#1a1a1a")

     ;; Paren match
     (bg-paren-match        "#2f7f9f")
     (bg-paren-expression   "#453040")

     ;; General mappings (Zed semantic style)
     (cursor blue)
     (keybind blue-cooler)
     (name magenta)
     (identifier yellow-faint)

     (err red)
     (warning yellow-warmer)
     (info cyan-cooler)

     (underline-err red-intense)
     (underline-warning yellow)
     (underline-note cyan)

     (bg-prominent-err bg-red-intense)
     (fg-prominent-err fg-main)
     (bg-prominent-warning bg-yellow-intense)
     (fg-prominent-warning fg-main)
     (bg-prominent-note bg-cyan-intense)
     (fg-prominent-note fg-main)

     (bg-active-argument bg-yellow-nuanced)
     (fg-active-argument yellow-cooler)
     (bg-active-value bg-cyan-nuanced)
     (fg-active-value cyan-cooler)

     ;; Code mappings (Zed One Dark syntax style)
     ;; keyword → magenta (purple)
     ;; string → green
     ;; function → blue
     ;; type → cyan
     ;; constant → yellow/orange
     ;; comment → dim gray
     (builtin magenta-warmer)
     (comment fg-dim)
     (constant yellow)
     (docstring fg-dim)
     (fnname blue)
     (fnname-call blue)
     (keyword magenta)
     (preprocessor red-cooler)
     (property red)
     (rx-backslash magenta)
     (rx-construct green-cooler)
     (string green)
     (type cyan)
     (variable red)
     (variable-use slate)

     ;; Accent mappings
     (accent-0 blue)
     (accent-1 magenta)
     (accent-2 cyan)
     (accent-3 yellow)

     ;; Completion mappings
     (fg-completion-match-0 blue)
     (fg-completion-match-1 magenta)
     (fg-completion-match-2 cyan)
     (fg-completion-match-3 yellow)

     ;; Date mappings
     (date-common cyan)
     (date-deadline red)
     (date-deadline-subtle red-faint)
     (date-event fg-alt)
     (date-holiday magenta-warmer)
     (date-holiday-other blue)
     (date-range fg-alt)
     (date-scheduled yellow-cooler)
     (date-scheduled-subtle yellow-faint)
     (date-weekday cyan)
     (date-weekend magenta)

     ;; Link mappings
     (fg-link blue-warmer)
     (underline-link blue-warmer)
     (fg-link-symbolic cyan)
     (underline-link-symbolic cyan)
     (fg-link-visited magenta)
     (underline-link-visited magenta)

     ;; Mail mappings
     (mail-cite-0 blue-warmer)
     (mail-cite-1 yellow-cooler)
     (mail-cite-2 cyan-cooler)
     (mail-cite-3 red-cooler)
     (mail-part blue)
     (mail-recipient magenta-cooler)
     (mail-subject magenta-warmer)
     (mail-other magenta-faint)

     ;; Mark mappings
     (bg-mark-delete bg-red-subtle)
     (fg-mark-delete red-cooler)
     (bg-mark-select bg-cyan-subtle)
     (fg-mark-select cyan)
     (bg-mark-other bg-yellow-subtle)
     (fg-mark-other yellow)

     ;; Prompt mappings
     (fg-prompt cyan-cooler)

     ;; Prose mappings
     (fg-prose-code cyan-cooler)
     (fg-prose-macro magenta-cooler)
     (fg-prose-verbatim magenta-warmer)
     (prose-done green)
     (prose-todo red)
     (prose-metadata fg-dim)
     (prose-metadata-value fg-alt)
     (prose-table fg-alt)
     (prose-table-formula magenta-warmer)
     (prose-tag magenta-faint)

     ;; Rainbow mappings
     (rainbow-0 fg-main)
     (rainbow-1 magenta-intense)
     (rainbow-2 cyan-intense)
     (rainbow-3 red-warmer)
     (rainbow-4 yellow-intense)
     (rainbow-5 magenta-cooler)
     (rainbow-6 green-intense)
     (rainbow-7 blue-warmer)
     (rainbow-8 magenta-warmer)

     ;; Search mappings
     (bg-search-current "#e8af7466")
     (bg-search-lazy "#74ade866")
     (bg-search-static bg-magenta-subtle)
     (bg-search-replace bg-red-intense)

     (fg-search-current yellow-warmer)
     (fg-search-lazy blue-warmer)
     (fg-search-static magenta)
     (fg-search-replace red)

     (bg-search-rx-group-0 bg-blue-intense)
     (bg-search-rx-group-1 bg-green-intense)
     (bg-search-rx-group-2 bg-red-subtle)
     (bg-search-rx-group-3 bg-magenta-subtle)

     (fg-search-rx-group-0 blue)
     (fg-search-rx-group-1 green)
     (fg-search-rx-group-2 red)
     (fg-search-rx-group-3 magenta)

     ;; Heading mappings
     (fg-heading-0 cyan-cooler)
     (fg-heading-1 fg-main)
     (fg-heading-2 yellow-faint)
     (fg-heading-3 blue-faint)
     (fg-heading-4 magenta)
     (fg-heading-5 green-faint)
     (fg-heading-6 red-faint)
     (fg-heading-7 cyan-faint)
     (fg-heading-8 fg-dim))
   modus-themes-common-palette-mappings)
  "The entire palette of the `hiro-one-dark' theme.
Based on Zed Editor's One Dark color scheme.")

(modus-themes-theme
 hiro-one-dark
 hiro-themes
 "Zed One Dark inspired theme with a dark background.
Built on modus-themes for comprehensive face coverage."
 dark
 hiro-one-dark-palette
 nil
 nil)

(provide-theme 'hiro-one-dark)
(provide 'hiro-one-dark-theme)

;;; hiro-one-dark-theme.el ends here
