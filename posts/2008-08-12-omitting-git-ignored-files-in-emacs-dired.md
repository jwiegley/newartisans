---
title: Omitting Git-ignored files in Emacs dired
category: Uncategorized
---

Adding the following snippet to your `.emacs` file will cause Emacs’ dired mode to omit all files ignored by Git.  This only works if you have dired-omit-mode on, which is ordinarily bound to `Meta-o`.

<!--more-->
# The code

Here is the code you would add to your `.emacs` file:

    (add-hook ‘dired-load-hook #’(lambda nil (load “dired-x” t)))

    (eval-after-load “dired-x”
      ‘(progn
         (defvar dired-omit-regexp-orig (symbol-function ‘dired-omit-regexp))
    
         (defun dired-omit-regexp ()
           (let ((file (expand-file-name “.git”))
                 parent-dir)
             (while (and (not (file-exists-p file))
                         (progn
                           (setq parent-dir
                                 (file-name-directory
                                  (directory-file-name
                                   (file-name-directory file))))
                           ;; Give up if we are already at the root dir.
                           (not (string= (file-name-directory file)
                                         parent-dir))))
               ;; Move up to the parent dir and try again.
               (setq file (expand-file-name “.git” parent-dir)))
             ;; If we found a change log in a parent, use that.
             (if (file-exists-p file)
                 (let ((regexp (funcall dired-omit-regexp-orig)))
                   (assert (stringp regexp))
                   (concat
                    regexp
                    (if (> (length regexp) 0)
                        “\\|” “”)
                    “\\(“
                    (mapconcat
                     #’(lambda (str)
                         (concat “^”
                                 (regexp-quote
                                  (substring str 13
                                             (if (= ?/ (aref str (1- (length str))))
                                                 (1- (length str))
                                               nil)))
                                 “$”))
                     (split-string (shell-command-to-string
                                    “git clean -d -x -n”)
                                   “\n” t)
                     “\\|”)
                    “\\)”))
               (funcall dired-omit-regexp-orig))))))

A note to fellow Emacs coders: I tried writing this as a piece of `defadvice`, rather than hijacking the definition of `dired-omit-regexp`, but for some reason it never called this function.

