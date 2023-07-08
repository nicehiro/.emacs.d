;; -*- lexical-binding: t; -*-

(use-package org-static-blog
  :load-path "lib/org-static-blog"
  :config
  (require 'oc-csl)
  (setq org-static-blog-publish-title "Fangyuan's Blog")
  (setq org-static-blog-publish-url "https://nicehiro.github.io")
  (setq org-static-blog-publish-directory "~/Documents/blog/")
  (setq org-static-blog-posts-directory "~/Documents/blog/posts/")
  (setq org-static-blog-drafts-directory "~/Documents/blog/drafts/")
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers t)
  (setq org-static-blog-index-length 5)
  (setq org-static-blog-enable-tags t)
  (setq org-static-blog-use-preview t)

  (setq org-static-blog-page-header
        "<meta name=\"author\" content=\"Fangyuan\">
    <meta name=\"referrer\" content=\"no-referrer\">
    <link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
    <script src=\"https://kit.fontawesome.com/4afcf67bd2.js\" crossorigin=\"anonymous\"></script>
    <link rel=\"icon\" href=\"static/favicon.ico\">")

  (setq org-static-blog-page-preamble
        "<div>
    <a href=\"https://nicehiro.github.io\">Fangyuan's Blog</a>
    ◌
    <a href=\"archive.html\"><i class=\"fa-solid fa-box-open\"></i>Archive</a>
    ◌
    <a href=\"about.html\"><i class=\"fa-solid fa-address-card\"></i> About</a>
    ◌
    <a href=\"rss.xml\"><i class=\"fa-solid fa-rss\"></i> RSS</a>
    </div>")

  (setq org-static-blog-page-postamble
        "<div id=blog-tail><center><span>Powered by Org-mode and Emacs</center></div>"))

(provide 'init-blog)
;;; init-blog.el ends here
