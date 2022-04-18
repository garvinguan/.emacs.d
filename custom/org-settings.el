(setq org-log-done 'time)
(setq org-capture-templates
      '(
        ("e" "Emacs Todo" entry (file+headline "~/org/todos.org" "Emacs")
        "* TODO %U %?\n")
        ("w" "Work Todo" entry (file+headline "~/org/todos.org" "Work")
        "* TODO %?")
        ("n" "Investigate" entry (file+headline "~/org/todos.org" "Investigate")
        "* TODO %?")
        ("r" "Reminder" entry (file+headline "~/org/todos.org" "Reminder")
        "* TODO %U %?\n%a")
        ("b" "Buy" checkitem (file+headline "~/org/todos.org" "Buy")
        "[ ] %U %?")
        ("o" "1on1" checkitem (file+headline "~/org/todos.org" "1on1")
        "[ ] %U %?")
        ))
