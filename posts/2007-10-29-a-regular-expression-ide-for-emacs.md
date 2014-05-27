---
title: A regular expression IDE for Emacs
description: desc here
tags: 
date: [2007-10-29 Mon 07:48]
category: Uncategorized
id: 185
---

I've started work on a new mode for Emacs, which intends to be something like an "IDE" for regular expressions.  There are similar tools out there, such as [Reggy][] for OS X and [The Regex Coach][] for Windows.  But the former doesn't show me subgroup matches, and the latter won't run on OS X.  Which made me wonder, wouldn't this be easy to do in Emacs?  Three hours later says yes.

<!--more-->
After downloading [regex-tool.el][], put it somewhere where your Emacs can see it (typically `/usr/local/share/emacs/site-lisp`), and then add this to your `.emacs` file:

	(load "regex-tool" t)    ; load regex-tool if it's available

Then type `M-x load-file` and load it manually for your current session.  Once that is done, type `M-x regex-tool` to start the show.  It will create a new Emacs frame for you looking something like this:

<span class="mt-enclosure mt-enclosure-image"><img src="http://www.newartisans.com/images/regex-tool.png" width="495" alt="regex-tool.png" height="532" class="mt-image-center" style="text-align: center;margin: 0 auto 20px" /></span>

The `*Regex*` section starts out blank, awaiting you to type in your regular expression.  The default syntax is Emacs; if you would prefer `regex-tool` to use full Perl regular expressions, type the command `M-x customize-group`, choose the group `regex-tool`, and then change the backend to "perl" by clicking on the Value Menu.  When configured to use Perl, `regex-tool` will actually invoke a real Perl subprocess to run your matches against, so you can use whatever syntax your installed version of Perl accepts.

The matched parts of the sample string are highlighted in bold red, and all the matching subgroups (including group 0, to show the whole match) are indicated in the `*Groups*` buffer.

I'm sure there are many ways this little tool could be extended, so please don't hesitate to send me a note with your suggestions.  Or find me in the #emacs IRC channel, on the server `irc.freenode.net`.  I'm `johnw` on there, and am online most evenings.

[Reggy]: http://code.google.com/p/reggy/
[The Regex Coach]: http://www.weitz.de/regex-coach/
[regex-tool.el]: /downloads_files/regex-tool.el

