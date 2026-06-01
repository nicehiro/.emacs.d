;;; hiro-one-light-theme.el --- Zed One Light inspired theme built on modus-themes -*- lexical-binding:t -*-

;; Copyright (C) 2025

;; Author: Hiro
;; URL: https://github.com/fangyuan/dotfiles
;; Keywords: faces, theme

;;; Commentary:
;;
;; A light theme based on Zed Editor's "One Light" color palette,
;; built on top of the modus-themes infrastructure for comprehensive
;; face coverage and accessibility.

;;; Code:

(require 'modus-themes)

(defconst hiro-one-light-palette
  (append
   '(
     ;; Basic values (from Zed One Light)
     (bg-main          "#fafafa")
     (bg-dim           "#ebebec")
     (fg-main          "#242529")
     (fg-dim           "#a2a3a7")
     (fg-alt           "#58585a")
     (bg-active        "#cacacb")
     (bg-inactive      "#dfdfe0")
     (border           "#c9c9ca")

     ;; Common accent foregrounds (from Zed One Light syntax)
     (red             "#e45649")
     (red-warmer      "#d3604f")
     (red-cooler      "#b92b46")
     (red-faint       "#7f0000")
     (red-intense     "#de3e35")
     (green           "#50a14f")
     (green-warmer    "#649f57")
     (green-cooler    "#669f59")
     (green-faint     "#2a5045")
     (green-intense   "#3f953a")
     (yellow          "#c18401")
     (yellow-warmer   "#ad6e26")
     (yellow-cooler   "#a48819")
     (yellow-faint    "#624416")
     (yellow-intense  "#d2b67c")
     (blue            "#4078f2")
     (blue-warmer     "#5c78e2")
     (blue-cooler     "#5b79e3")
     (blue-faint      "#003497")
     (blue-intense    "#2f5af3")
     (magenta         "#a626a4")
     (magenta-warmer  "#a449ab")
     (magenta-cooler  "#a349ab")
     (magenta-faint   "#7c318f")
     (magenta-intense "#950095")
     (cyan            "#0184bc")
     (cyan-warmer     "#3882b7")
     (cyan-cooler     "#3a82b7")
     (cyan-faint      "#005077")
     (cyan-intense    "#008899")

     ;; Uncommon accent foregrounds
     (rust       "#ad6e26")
     (gold       "#c18401")
     (olive      "#669f59")
     (slate      "#5c78e2")
     (indigo     "#a449ab")
     (maroon     "#d3604f")
     (pink       "#a449ab")

     ;; Common accent backgrounds
     (bg-red-intense     "#ff8f88")
     (bg-green-intense   "#8adf80")
     (bg-yellow-intense  "#f3d000")
     (bg-blue-intense    "#bfc9ff")
     (bg-magenta-intense "#dfa0f0")
     (bg-cyan-intense    "#a4d5f9")

     (bg-red-subtle      "#fbdfd9")
     (bg-green-subtle    "#dfeadb")
     (bg-yellow-subtle   "#faf2e6")
     (bg-blue-subtle     "#e2e2fa")
     (bg-magenta-subtle  "#ffddff")
     (bg-cyan-subtle     "#bfefff")

     (bg-red-nuanced     "#ffe8e8")
     (bg-green-nuanced   "#e0f6e0")
     (bg-yellow-nuanced  "#f8f0d0")
     (bg-blue-nuanced    "#ecedff")
     (bg-magenta-nuanced "#f8e6f5")
     (bg-cyan-nuanced    "#e0f2fa")

     ;; Uncommon accent background and foreground pairs
     (bg-clay     "#f1c8b5")
     (fg-clay     "#63192a")

     (bg-ochre    "#f0e3c0")
     (fg-ochre    "#573a30")

     (bg-lavender "#dfcdfa")
     (fg-lavender "#443379")

     (bg-sage     "#c0e7d4")
     (fg-sage     "#124b41")

     ;; Graphs
     (bg-graph-red-0     "#ef7969")
     (bg-graph-red-1     "#ffaab4")
     (bg-graph-green-0   "#45c050")
     (bg-graph-green-1   "#75ef30")
     (bg-graph-yellow-0  "#ffcf00")
     (bg-graph-yellow-1  "#f9ff00")
     (bg-graph-blue-0    "#7f90ff")
     (bg-graph-blue-1    "#a6c0ff")
     (bg-graph-magenta-0 "#e07fff")
     (bg-graph-magenta-1 "#fad0ff")
     (bg-graph-cyan-0    "#70d3f0")
     (bg-graph-cyan-1    "#afefff")

     ;; Special purpose
     (bg-completion       "#5c79e23d")
     (bg-popup            "#ebebec")
     (bg-hover            "#b2e4dc")
     (bg-hover-secondary  "#f5d0a0")
     (bg-hl-line          "#ebebec")
     (bg-region           "#d4e5ff")
     (fg-region           "#000000")

     (bg-mode-line-active        "#dcdcdd")
     (fg-mode-line-active        "#242529")
     (border-mode-line-active    "#9f9f9f")
     (bg-mode-line-inactive      "#ebebec")
     (fg-mode-line-inactive      "#7e8086")
     (border-mode-line-inactive  "#c9c9ca")

     (modeline-err     "#e45649")
     (modeline-warning "#c18401")
     (modeline-info    "#4078f2")

     (bg-tab-bar      "#ebebec")
     (bg-tab-current  "#fafafa")
     (bg-tab-other    "#dcdcdd")

     ;; Diffs
     (bg-added           "#dfeadb")
     (bg-added-faint     "#e8f8e1")
     (bg-added-refine    "#c8dcc1")
     (bg-added-fringe    "#27a657")
     (fg-added           "#669f59")
     (fg-added-intense   "#3f953a")

     (bg-changed         "#faf2e6")
     (bg-changed-faint   "#fff9ef")
     (bg-changed-refine  "#f4e7d1")
     (bg-changed-fringe  "#d3b020")
     (fg-changed         "#a48819")
     (fg-changed-intense "#c18401")

     (bg-removed         "#fbdfd9")
     (bg-removed-faint   "#ffe9e9")
     (bg-removed-refine  "#f6c6bd")
     (bg-removed-fringe  "#e06c76")
     (fg-removed         "#d36151")
     (fg-removed-intense "#de3e35")

     (bg-diff-context    "#f3f3f3")

     ;; Paren match
     (bg-paren-match        "#5fcfff")
     (bg-paren-expression   "#efd3f5")
     (underline-paren-match unspecified)

     ;; General mappings (Zed semantic style)
     (cursor blue)
     (keybind blue-cooler)
     (name magenta)
     (identifier yellow-faint)

     (err red)
     (warning yellow-warmer)
     (info cyan-cooler)

     (underline-err red-intense)
     (underline-warning yellow-intense)
     (underline-note cyan-intense)

     (bg-prominent-err bg-red-intense)
     (fg-prominent-err fg-main)
     (bg-prominent-warning bg-yellow-intense)
     (fg-prominent-warning fg-main)
     (bg-prominent-note bg-cyan-intense)
     (fg-prominent-note fg-main)

     (bg-active-argument bg-yellow-nuanced)
     (fg-active-argument yellow-warmer)
     (bg-active-value bg-cyan-nuanced)
     (fg-active-value cyan-warmer)

     ;; Code mappings (Zed One Light syntax style)
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
     (date-deadline red-cooler)
     (date-deadline-subtle red-faint)
     (date-event fg-alt)
     (date-holiday red)
     (date-holiday-other blue)
     (date-range fg-alt)
     (date-scheduled yellow)
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
     (mail-cite-0 blue-faint)
     (mail-cite-1 yellow-warmer)
     (mail-cite-2 cyan-cooler)
     (mail-cite-3 red-cooler)
     (mail-part cyan)
     (mail-recipient magenta-cooler)
     (mail-subject magenta-warmer)
     (mail-other magenta-faint)

     ;; Mark mappings
     (bg-mark-delete bg-red-subtle)
     (fg-mark-delete red)
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
     (bg-search-current "#d0a92366")
     (bg-search-lazy "#5c79e266")
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
     (fg-heading-3 fg-alt)
     (fg-heading-4 magenta)
     (fg-heading-5 green-faint)
     (fg-heading-6 red-faint)
     (fg-heading-7 cyan-warmer)
     (fg-heading-8 fg-dim))
   modus-themes-common-palette-mappings)
  "The entire palette of the `hiro-one-light' theme.
Based on Zed Editor's One Light color scheme.")

(modus-themes-theme
 'hiro-one-light
 'ef-themes
 "Zed One Light inspired theme with a light background.
Built on modus-themes for comprehensive face coverage."
 'light
 'hiro-one-light-palette
 nil
 nil)

(provide-theme 'hiro-one-light)
(provide 'hiro-one-light-theme)

;;; hiro-one-light-theme.el ends here
