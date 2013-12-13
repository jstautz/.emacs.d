(setq sml/theme 'respectful
      sml/name-width 40
      sml/mode-width 40)

(setq sml/replacer-regexp-list
      (quote
       (("^~/org/" ":Org:")
        ("^~/\\.emacs\\.d/" ":ED:")
        ("^~/[Gg]it/" ":Git:")
        ("^~/Dropbox/Writing/01-composting/" ":Compost:")
        ("^~/Dropbox/Writing/02-draft_in_progress/" ":Drafts:")
        ("^~/Dropbox/Writing/03-revision_in_progress/" ":Revs:")
        ("^~/Dropbox/Writing/04-submitted/" ":Submitted:")
        ("^~/Dropbox/Writing/05-published/" ":Published:")
        ("^~/Dropbox/Writing/06-cold_storage/" ":ColdStore:")
        ("^~/Dropbox/Writing/" ":Writing:")
        ("^~/Dropbox/finance/taxes 2013/" ":Tax2013:")
        ("^~/Dropbox/" ":DB:")
        ("^~/Documents/Writing/01-composting/" ":Compost:")
        ("^~/Documents/Writing/02-draft_in_progress/" ":Drafts:")
        ("^~/Documents/Writing/03-revision_in_progress/" ":Revs:")
        ("^~/Documents/Writing/04-submitted/" ":Submitted:")
        ("^~/Documents/Writing/05-published/" ":Published:")
        ("^~/Documents/Writing/06-cold_storage/" ":ColdStore:")
        ("^~/Documents/Writing/" ":Writing:")
        ("^~/Documents/" ":Docs:")
        ("^~/Documents/Work/" ":Work:")
        ("^~/dev" ":dev:")
        ("^~/Sites" ":www:")
        ("^~/Downloads/" ":DLs:"))))

(sml/setup)


