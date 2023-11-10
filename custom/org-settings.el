(setq org-log-done 'time)
(setq org-capture-templates
      '(
        ("e" "Emacs Todo" entry (file+headline "~/org/todos.org" "Emacs")
        "* TODO %U %?\n")
        ("t" "Today" entry (file+headline "~/org/todos.org" "Today")
        "* TODO %U %?\n")
        ("w" "Work Todo" entry (file+headline "~/org/todos.org" "Work")
        "* TODO %?\n")
        ("n" "Investigate" entry (file+headline "~/org/todos.org" "Investigate")
        "* TODO %?\n")
        ("r" "Reminder" entry (file+headline "~/org/todos.org" "Reminder")
        "* TODO %U %?\n%a")
        ("b" "Buy" checkitem (file+headline "~/org/todos.org" "Buy")
        "[ ] %U %?\n")
        ("s" "1on1 Steve" checkitem (file+headline "~/org/todos.org" "1on1 Steve")
        "[ ] %U %?\n")
        ("n" "1on1 Shane" checkitem (file+headline "~/org/todos.org" "1on1 Shane")
        "[ ] %U %?\n")
        ("a" "1on1 Alex" checkitem (file+headline "~/org/todos.org" "1on1 Alex")
        "[ ] %U %?\n")
        ))
