title: Managing time (and life) with Org-mode
description: desc here
tags: emacs
date: [2014-12-21 Sun 08:09]
category: Emacs

The following article summarizes how I use the Emacs Org-mode to manage time.
I am writing this after seven years of nearly continuous, daily use, during
which I've dealt with over 11,200 tasks (with recurring tasks counting only
once overall).  That said, I've settled into a rhythm that works quite well
for me, although I cannot guarantee it will do so for anyone else.

# Philosophy

"To do" managers are often presented as if they exist to accomplish things;
for example, the phrase "Getting things done".  I take a somewhat different
view: A todo list exists so that nothing significant is missed.  It's better
to have ten items go uncompleted in my list, *knowing* there is no eleventh,
than to finish most of them, only to discover that there were really twenty,
and the listed ten were the least important of them.

If I were to reduce this to a dictum, it would be:

> Whatever you achieve, do it in full awareness of what is not being done.

Surely I hope to get some things done along the way, but psychology is as much
a part of time management as the tools you use.  Feeling like you are "failing
your todo list" is a common reason for giving up altogether.  In order to keep
up with a system, you need to create an environment where you succeed simply
by using it: not by how much is finished at the end of each day.

If this sounds like a license for laziness, think of it instead as an
alternative to doing nothing whatsoever.  Do what motivates you, and build on
that.  I've found that good time management is an excellent beginning to
motivation, and a great resource manager -- but a very poor life coach.

There are enough nagging voices about responsibility and duty; instead, make
your time management tool that a refinement of who you *are*; and maybe after,
it will help you can develop a clearer picture of who you want to become.

# Starting out with Org-mode

I assume only a little familiarity with Org-mode in this article, but I'm not
writing this to replace the manual.  I want to talk about ideas and strategies
to help you use the tool, so I'll leave most of the mechanics to the package's
capable authors.

So, using Org-mode, there are only a few activities I engage in with
frequency:

  1. Capturing tasks and information.
  2. Categorizing and tagging tasks.
  3. Viewing the daily agenda.
  4. Reviewing unscheduled tasks.

## Capturing

The first discipline to develop is *capture*.  Capture it all.  Also, it must
be incredibly easy to capture on a moment's notice.  There is a chapter in the
Org-mode manual called "Capture" that documents how to set it up and use the
capture system.

(I have the `org-capture` command bound to `M-m`, and use a global system
macro (via QuicKeys) so that I can press `M-m` in any application on my Mac,
and it will switch to Emacs and repeat the keypress there).

The idea behind capturing is to get the thought down immediately, but not to
deal with it then.  It shouldn't be a distraction.  Discharge the mental
obligation right away, and rest assured the task will not be lost.  A good
capture mechanism should give you a place to type -- and little else -- but
return you to what you were doing immediately afterward.  This way, there is
no impedance to capturing every little thing that passes through your mind.

The capture mode in Org-mode allows you to define a "template" for new tasks,
and a default location to store them in.  This is what my customization
settings look like for `org-capture-templates`:

    Hide Org Capture Templates:
    [INS] [DEL] Choice: [Value Menu] Template entry:
                Keys           : t
                Description    : Task
                Capture Type   : [Value Menu] Org entry
                Target location: [Value Menu] File & Headline:
                  File    : ~/Documents/Tasks/todo.txt
                  Headline: Inbox
                Template: [Value Menu] String: * TODO %?
    SCHEDULED: %t
    :PROPERTIES:
    :ID:       %(shell-command-to-string "uuidgen"):CREATED:  %U
    :END:
            Plist:
            [X] Key: :prepend t
            (everything else is unchecked)

The core ideas of this template are:

  - New tasks are filed under a heading called "Inbox" in `todo.txt`.
  - New tasks started in a `TODO` state.
  - New tasks are scheduled for today (so I don't accidentaly forget them
    completely).
  - Each task is given a UUID, so I have a way to reference it from anywhere,
    forever.
  - Each task gets a timestamp, so I know when it was first created.

## Categorizing

Later, when things are quieter, I pull up a custom agenda report that shows
all tasks in my `Inbox`, and repeatedly use the `r` key (for "refile") to
assign them to a more appropriate category.  I also use `:` and `,` in this
view to tag and prioritize tasks, as needed.  (I rebound this key myself,
since the default of `C-c C-w` is too much to type so frequently).

It's important to have good top-level categories, since these can be used to
establish the "global ordering" of tasks in the agenda views.  My pattern is
to use "role & responsibilities", so that my categories look like:

    * Finances
    * Health
    * Auto
    * Home
    * Family
    * Friends
    * Contracts
    ** Client One
    *** PROJECT foo
    *** PROJECT bar
    ** Client Two

Each of the top-level headings, and anything labeled as a `PROJECT`,
automatically becomes a refiling target because I have the following values
set in `org-refile-targets`:

    Hide Org Refile Targets:
    [INS] [DEL] Cons-cell:
                Choice: [Value Menu] File: ~/Documents/Tasks/todo.txt
                Identify target headline by: [Value Menu] Level number:
                :level
                Integer: 1
    [INS] [DEL] Cons-cell:
                Choice: [Value Menu] File: ~/Documents/Tasks/todo.txt
                Identify target headline by: [Value Menu] TODO keyword:
                :todo
                String: PROJECT

## Tagging

Tags are pretty optional.  I use them to identify resources needed to
accomplish a task, so I can filter them out when that resource is unavailable
(such as the Internet, or being within regular business hours).  The tags I
use regularly are:

  - Net
  - Call
  - Errand
  - Home
  - Wife

When I work regularly with a co-worker, I add them as a tag too, so I can
quickly see which tasks I should follow up on when I have him or her on the
phone.

Org-mode has some fancy ways of interacting with tasks: creating, assigning,
filtering, etc.  There's even a way to have the system determine which
resources are available, for example, by using a script to test for the
presence of a Net connection.  If you're interesting, check out my Org-mode
configuration[^1] and look for `org-my-auto-exclude-function`.  The
configuration variable to edit is `org-agenda-auto-exclude-function`.  The key
binding is `/ RET`.

## Viewing the Agenda

The heart of soul of a task manager, of course, is to show you a list of tasks
each day.  In Org-mode, this is accomplished by type `C-c a a`.

The objective of this daily list is to have the following qualities:

  1. Be relevant: what you see relates to what you can or should do.
  2. Complete: nothing is missing.  *This is the most important of all*!
  3. Achievable: a demoralizing list can be worse than none at all.
  4. Informative: provides the context you need to get things done.
  5. Granular: the pieces are bite-sized and easily digested.

Corresponding to these attributes are a few principles I've discovered over
the years:

### The One List Principle

> A man cannot serve two masters, nor the mind respect two task lists.

If you have tasks in Org-mode, tasks on GitHub, tasks on your fridge, and
tasks in sticy notes surrounding your monitor, only one of these sources will
assert itself in your deeper mind.  The others will slowly be ignored, as you
rush to fight fires from the primary list each day.

The solution: One List to Rule Them All.  If you have tasks that are tracked
by a separate list, enter a shadow task in Org-mode that links to that list.
Turn Org-mode into a list of lists, if need be.  As long as there is one
authoritative task list that you can turn to each day, you stand a better
chance of sticking with it.

### Sacrifical Tasks

For a while I lived on a sailboat, and on that boat were two shiny plates of
zinc, fixed to the hull.  The reason is that since a boat sits in salt water
all day, and has metal on it, corrosion is essentially continuous.  The zinc
plates, called "sacrifical plates", offer the electrolytic process in sea
water something to munch on, so it leaves the brass and other fixtures on the
boat alone (they ultimately become plated by zinc, through electro-plating).

Similarly, I find that no matter how hard I try, at best I can accomplish 70%
of what I set out to do each day.  What's even stranger is that if I increase
the number of tasks, it's still 70%.  In other words, I can get more by done
by "over-allocating", since apparently some part of my mind wants a few undone
tasks left for tomorrow.

These extra tasks -- that I schedule but have no intention of doing -- are my
"sacrifical tasks".  The key is not to feel badly about them, but to take a
holistic view about what scheduling a task means.

### Task List Zero

The sacrifical task approach only works up to a point, however.  For example,
the maximum number of tasks I can cope with in any day -- no matter how big or
small each task is -- is around 14.  Scheduling more than that is identical to
not scheduling it at all.  Well, not exactly: seeing a mountain of tasks at
the start of the day is actually demoralizing, and reduces the amount of work
I get done.

Thus when my task list starts getting big, instead of pushing myself harder to
keep up with it, I unschedule everything and start over.  I pick the 14 best
tasks (by importance, brevity, or enjoyability), and schedule those for today,
leaving the rest for my weekly review of unscheduled tasks.

> It's important for things to appear manageable, even if it means giving up
> on most of them.

### Be responsible to people, not tasks

Perhaps the greatest difficulty I face when applying task management is to
cancel an incomplete task that is either unreasonable, unachievable, or just
too boring to ever happen.  Somehow, it's natural for the mind to become
attached to a written task, feeling a sense of responsibility as if it had a
life of its own.

I counter this by remembering that I owe my tasks nothing; it's the people I'm
doing them for that matter.  Just as you owe nothing to an unfinished book to
read it, or an incomplete puzzle to finish it, or a delayed project to
continue it at all: focus on the *why* of your tasks, and free yourself to
cancel those that don't have a good answer to that question.

## Review

At the beginning of this article I mentioned that the foundational principle
of good task management is that the important things are not forgotten.  The
fulfillment of this principle is the "weekly review".

[^1]: https://github.com/jwiegley/dot-emacs
