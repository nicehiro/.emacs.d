;; -*- lexical-binding: t; -*-

(use-package elfeed
  :config
  (setq elfeed-feeds
        '(("https://zu1k.com/index.xml")
          ("https://jhuo.ca/index.xml")
          ("http://laike9m.com/blog/rss")
          ("https://archive.casouri.cat/note/atom.xml")
          ("https://nicehiro.github.io/rss.xml")
          ("https://deepmind.com/blog/feed/basic/" research)
          ("http://ai.googleblog.com/feeds/posts/default?alt=rss" research)
          ("https://bair.berkeley.edu/blog/feed.xml" research)
          ("https://lilianweng.github.io/lil-log/feed.xml" research)
          ("https://manateelazycat.github.io/feed.xml" emacs)
          ("https://sachachua.com/blog/feed" emacs)
          ("https://karthinks.com/software/index.xml" emacs)
          ("https://danieltakeshi.github.io/feed.xml" research)))
  (setq-default elfeed-search-filter "@1-years-old +unread "))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
