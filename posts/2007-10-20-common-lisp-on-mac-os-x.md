---
title: Common Lisp on Mac OS X
category: Uncategorized
---

You could be having fun with Common Lisp on your Mac right now; you know that, don't you? ;)

Lately I have been having a ball doing Common Lisp programming on my MacBook Pro.  But as with all great starts, this was not without its pitfalls.  After many frustrating hours, and questions asked on the #lisp IRC channel, I've come to realize that perhaps others may benefit from treading a path already trodden.

<!--more-->
**NOTE**: I have recently found a *much* easier solution for Mac users wanting to enter the world of Lisp.  I highly recommend that you download [LispWorks Personal Edition][].  It has a limitation that it can only run for five hours at a time, but it is a free download, has a great environment, and some really superb debugging and analysis tools.  I'm using this version as my main debugging environment now, which I access during regular coding using Emacs and SLIME (using the same settings as below).  Anyway, this is a far quicker way to get started than slogging through all the settings in this article.  I only recommend following these directions if you want to setup a fully free software-based Lisp environment on your Mac.

[LispWorks Personal Edition]: http://www.lispworks.com/downloads/

## Installing MacPorts

The first step towards enjoying Common Lisp on your Mac is to install [MacPorts][].  Really, if you haven't installed this beautiful system by now, you've been missing out.  It puts (most of) the world of free software right at your fingertips.

Once MacPorts is installed, just run the following commands:

    sudo port install emacs +carbon
    sudo port install sbcl slime

This will do three things for you:

1. It installs the Carbon version of Emacs 22, which OS X does not ship with.  Emacs 22 is surprisingly stable, and feature rich.  I use Emacs heavily every minute of every day, and this version has not crashed on me a single time.

1. It installs the latest version of Steel Bank Common Lisp (SBCL), a branch off the source tree for CMU Common Lisp (CMUCL).  This version is more actively developed, and seems to be what current Lisp hackers are excited about.  For you, the end user, this means better integration with nerdy development environments, like Emacs and SLIME.

2. It installs SLIME, the "Superior Lisp Interaction Mode for Emacs".  SLIME gives you an interactive REPL (read: Lisp console) that you can interact with while you develop your Lisp code.  This is such a wonderful thing I won't even try to describe it in this simple article.  It also allows you to interactively debug programs, inspect and evaluation values at runtime, and quickly access reams of documentation and type information relating to your code.  This is one of the best Lisp IDEs out there -- although the graphical stepper from LispWorks is pretty sexy too.

[MacPorts]: http://www.macports.org/

## Configuring SLIME

Now you have SBCL and SLIME installed.  You could, at this point, run Emacs and type `M-x slime`.  When it prompts for a command to run, just pick `sbcl`.  Boom, you are now in a Lisp REPL and can type things like this:

    CL-USER> (format t "Hello, world!")
    Hello, world!NIL
    CL-USER>

This is great and all; but life can be so much more wonderful than this.

### cldoc

First, there is [cldoc][], a very cool module for Emacs that knows a lot about the arguments and return values for all the ANSI Common Lisp functions.  Although SLIME itself could show you the arguments for functions, this handy piece of work will show you the return values for standard functions.  This is very helpful.  Just throw this into your `.emacs.el` file after installing `cldoc.el` into your `site-lisp`:

    (autoload 'turn-on-cldoc-mode "cldoc" nil t)
    
    (dolist (hook '(lisp-mode-hook
                    slime-repl-mode-hook))
      (add-hook hook 'turn-on-cldoc-mode))

[cldoc]: http://homepage1.nifty.com/bmonkey/emacs/elisp/cldoc.el

### paredit

[paredit][] is a curious mode that you may either come to love (as I have) or you'll hate it and never look back.  It tries to orient your editing behavior around sexps, instead of text.  The way it does this is by preventing you from ever having a mismatched number of open and closed parentheses in your source file.  If you try to hit backspace and delete a closing parenthesis, paredit will just ignore you and move the cursor inside the parentheses.  It goes out of its way to ensure that there for every open parenthesis, there is a matching closed parenthesis.

Sometimes this can get in the way.  But once you use it for a while, and start to get into the "zen of paredit" (as I think about it), it starts becoming incredibly helpful.  The best part is that it provides some utilities for manipulating sexps that are not found in the stock Emacs.  Two of the most useful of these it calls "barfing" and "slurping".

You barf a sexp when you push it out from its containing sexp.  Let's say I'm editing the following list.  I'll use the pipe character to show where my point is:

    (format |t "Hello, world!" (+ 10 20))

Here I have a format call which takes an argument I don't need.  But instead of deleting it, I want to return it as the value of my function.  Easy, just hit `Control-}` to barf the last sexp out of the current one.  This is what results:

    (format |t "Hello, world!")
    (+ 10 20)

All without every moving my cursor.  Slurping is the reverse operation.  I find these two most useful for pushing sexps outside of an enclosing `let`, or sucking them in.  This is how I have my paredit configured:

    (autoload 'paredit-mode "paredit"
      "Minor mode for pseudo-structurally editing Lisp code." t)
    
    (dolist (hook '(emacs-lisp-mode-hook
                    lisp-mode-hook
                    slime-repl-mode-hook))
      (add-hook hook #'(lambda nil (paredit-mode 1))))
    
    (eval-after-load "paredit"
      '(progn
         (define-key paredit-mode-map [?\)] 'paredit-close-parenthesis)
         (define-key paredit-mode-map [(meta ?\))]
                     'paredit-close-parenthesis-and-newline)))

[paredit]: http://mumble.net/~campbell/emacs/paredit.el

### Configuring SLIME itself

Now we come to SLIME.  There are a lot of things that SLIME can do, so there's lots to configure.  I'm just going to share my current configuration with you here, leaving it to the reader to correct pathnames as necessary, or delete the stuff he doesn't want.  Many of these settings are purely personal (like binding `RET` to `paredit-newline`, which many may not want), so unless you like how it behaves, it may be better to start without all this stuff, and just add in the bits that seem useful as time goes by.

    (add-to-list 'load-path "~/Library/Emacs/site-lisp/slime")
    (add-to-list 'load-path "~/Library/Emacs/site-lisp/slime/contrib")
    
    (require 'slime)
    
    (slime-setup
     '(inferior-slime
       slime-asdf
       slime-autodoc
       slime-banner
       slime-c-p-c
       slime-editing-commands
       slime-fancy-inspector
       slime-fancy
       slime-fuzzy
       slime-highlight-edits
       slime-parse
       slime-presentation-streams
       slime-presentations
       slime-references
       slime-scratch
       slime-tramp
       slime-typeout-frame
       slime-xref-browser))  ; fixed per suggestion from tcr on #lisp
    
    ;;(setq slime-net-coding-system 'utf-8-unix)
    
    (setq slime-lisp-implementations
          '((sbcl ("sbcl" "--core"
                   "/home/johnw/Library/Lisp/sbcl.core-with-slime")
                  :init (lambda (port-file _)
                          (format
    "(swank:start-server %S :coding-system \"utf-8-unix\")\n"
                                  port-file))
                  :coding-system utf-8-unix)
            (cmucl ("lisp"))
            (ecl ("ecl"))
            (allegro ("/usr/local/stow/AllegroCL/alisp"))
            (clisp ("clisp") :coding-system utf-8-unix)
            (lispworks (""))
            (openmcl ("dx86cl64"))))
    
    (setq slime-default-lisp 'sbcl)
    
    (defun start-slime ()
      (interactive)
      (unless (slime-connected-p)
        (save-excursion (slime))))
    
    (add-hook 'slime-mode-hook 'start-slime)
    (add-hook 'slime-load-hook
              #'(lambda () (require 'slime-fancy)))
    (add-hook 'inferior-lisp-mode-hook
              #'(lambda () (inferior-slime-mode t)))
    
    (setq special-display-regexps
         (quote (("slime-repl" (height . 40) (width . 80)
                               (top . 85) (left . 50))
                 ("sldb" (height . 30) (width . 50)
                         (left . 10) (top . 25)))))
    
    (eval-after-load "hyperspec"
      '(progn
         (setq common-lisp-hyperspec-root
               "~/Reference/Computing/Languages/Common Lisp/HyperSpec/")))
    
    (defun indent-or-complete (&optional arg)
      (interactive "p")
      (if (or (looking-back "^\\s-*") (bolp))
          (call-interactively 'lisp-indent-line)
        (call-interactively 'slime-indent-and-complete-symbol)))
    
    (eval-after-load "lisp-mode"
      '(progn
         (define-key lisp-mode-map [tab] 'indent-or-complete)
         (define-key lisp-mode-map [(meta ?q)] 'slime-reindent-defun)))
    
    (eval-after-load "slime"
      '(progn
         (define-key slime-mode-map [return] 'paredit-newline)
         (define-key slime-repl-mode-map [tab] 'indent-or-complete)
         (define-key inferior-slime-mode-map [(control ?c) (control ?p)]
                     'slime-repl-previous-prompt)))

## Installing new packages

SBCL by itself is quite useful, but it has very few builtin packages.  Over time, you're going to find yourself wanting some things, like Perl-style regular expression support.  Here is the absolute quickest way to get that going with SBCL:

    Hermes:/usr/local $ sbcl
    This is SBCL 1.0.10, an implementation of ANSI Common Lisp.
    More information about SBCL is available at .
    
    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.
    * (require 'asdf)
    * (require 'asdf-install)
    * (asdf-install:install 'cl-ppcre)

You will see some output between this commands, which you can safely ignore.  At this point, the system will ask you whether you want to install CL-PPCRE as a system-wide or a local installation.  Pick whichever is appropriate for you.  It will then go out to the Internet and download CL-PPCRE, and then ask you if it's OK to skip the GnuPG signature key.  Just type 0 (zero) to indicate that it's OK to go ahead.  Or, if you love security, install the key and setup your system right.

Once installed, CL-PPCRE is now ready for use.  But what happens if you exit SBCL and restart?  Yep, it's gone.  At that point you will have to load it again like this:

    * (asdf:operate 'asdf:load-op :cl-ppcre)

But, you're wondering, isn't there a better way?  Why yes, my friend.  I'm so glad you asked.

## Bootstrapping SBCL

At any point in time you can save your running SBCL environment out to disk, and then reload it back in exactly where you left off.  This means that you can preload all the packages you love most, then dump SBCL so that the next time you start, they are all available without having to load them again.

The best way to do this is to write a file called `bootstrap.lisp`.  Put all the commands you need to initialize your environment into this file, and then run the following command:

    $ sbcl --load bootstrap.lisp

If you've written your file correctly, there will now be a core file in the current directory.  You can restart SBCL then like this:

    $ sbcl --core sbcl.core

This is not only a much easier way to preload the packages you need, it's also much, much faster.  In fact, I'm going to show you how to not only preload packages, but preload SLIME itself, so that the next time you type `M-x slime`, SBCL will load in just under a heartbeat.

### Example bootstrap.lisp file

Here's the `bootstrap.lisp` file that I use.  You'll need to change the pathnames to match your system.  It's main advantage is that it will install all the packages you need from the Internet, but thereafter will load them from disk if you've already downloaded them.  Feel free to comment out the `load-or-install` lines which load packages you don't care about.  Oh, and if you choose to go ahead and install CL-SQL, always choose "Continue" when you see errors about failing to load the libraries for databases you don't have installed.  I use PostgreSQL, so that's the only file that compiled without problems for me.

Also, be sure to fix the pathnames that point to Swank, the SLIME integration library for talking to SLIME.  By preloading Swank this way, I find that SBCL loads in about a third of a second from Emacs.

    (mapc 'require
          '(sb-bsd-sockets
            sb-posix
            sb-introspect
            sb-cltl2
            asdf
            asdf-install))
    
    (defvar *lisp-packages-directory*
      (merge-pathnames "Library/Lisp/" (user-homedir-pathname)))
    
    (push (list (merge-pathnames "site/" *lisp-packages-directory*)
                (merge-pathnames "systems/" *lisp-packages-directory*)
                "Local installation")
          asdf-install:*locations*)
    
    (push (merge-pathnames "systems/" *lisp-packages-directory*)
          asdf:*central-registry*)
    
    (defmacro load-or-install (package)
      `(handler-case
           (progn
             (asdf:operate 'asdf:load-op ,package))
         (asdf:missing-component ()
           (asdf-install:install ,package))))
    
    (load-or-install :xlunit)
    (load-or-install :cl-ppcre)
    
    (load-or-install :uffi)
    (load-or-install :md5)
    (load-or-install :clsql)
    (push "/usr/local/lib/postgresql82/"
          clsql-sys:*foreign-library-search-paths*)
    (load-or-install :clsql-postgresql-socket)
    (load-or-install :clsql-postgresql)
    
    (load-or-install :cffi)
    (push "/usr/local/lib" cffi:*foreign-library-directories*)
    (load-or-install :trivial-gray-streams)
    (load-or-install :flexi-streams)
    (load-or-install :url-rewrite)
    (load-or-install :rfc2388)
    (load-or-install :cl-base64)
    (load-or-install :chunga)
    (push  :hunchentoot-no-ssl *features*)
    (load-or-install :hunchentoot)
    (load-or-install :cl-who)
    
    (load (merge-pathnames
           "Library/Emacs/site-lisp/slime/swank-loader"
           (user-homedir-pathname)))
    
    (dolist (module '("swank-arglists"
                      "swank-asdf"
                      "swank-c-p-c"
                      "swank-fancy-inspector"
                      "swank-fuzzy"
                      "swank-presentation-streams"
                      "swank-presentations"))
      (load (merge-pathnames
             (merge-pathnames "Library/Emacs/site-lisp/slime/contrib/"
                              module)
             (user-homedir-pathname))))
    
    (sb-ext:save-lisp-and-die "sbcl.core-with-slime")

## Further information

At this point, I highly recommend you to read some of the documentation that comes with SLIME, and with SBCL.  When you start having problems, head over to the #lisp channel on IRC, or to the [CLiki][] website.  Or feel free to send me a note.  I'd be happy to help you get started with Common Lisp on OS X.

[CLiki]: http://www.cliki.net/index

