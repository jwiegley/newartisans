---
title: Using org-mode as a Day Planner
category: Uncategorized
---

The following document describes how to use Carsten Dominik's excellent `org-mode` Emacs package after the fashion of a pen-and-paper day planner.

For those curious, I was not brought up on time management in the era of the current GTD fad.  I started with a hard-bound book filled with daily planning sheets, along with training and books supporting this method.  I found it incredibly useful for the three or so years that I stuck with it, but ultimately discovered that only digital media can truly keep up with my ever-changing world.  Thus began my quest for the ultimate, computer day planning package.

I've tried everything available for the Apple Mac, and have even started some of my own systems (see Emacs Planner), but nothing has fully satisfied me -- which is to say, nothing has been able to keep me "on task", rather than finally gathering dust on a digital bookshelf.

Enter `org-mode`.  This handy system uses a fairly simple, single-file outlining paradigm, upon which it overlays concepts like due dates and priorities.  I find its method both non-intrusive and easy to edit by hand, which are absolute necessities for me.

<!--more-->
## Setting up your Emacs

### My configuration

I will present my usage of `org-mode` as a day planner first by giving some templates you can use straight away, and then by explaining my methodology via example uses of `org-mode` that employ this configuration.  First, you should add the following code to your `.emacs` file, if you have no other configuration:

	(require 'org-install)

	(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

	(define-key mode-specific-map [?a] 'org-agenda)

	(eval-after-load "org"
	  '(progn
	     (define-prefix-command 'org-todo-state-map)

	     (define-key org-mode-map "\C-cx" 'org-todo-state-map)

	     (define-key org-todo-state-map "x"
	       #'(lambda nil (interactive) (org-todo "CANCELLED")))
	     (define-key org-todo-state-map "d"
	       #'(lambda nil (interactive) (org-todo "DONE")))
	     (define-key org-todo-state-map "f"
	       #'(lambda nil (interactive) (org-todo "DEFERRED")))
	     (define-key org-todo-state-map "l"
	       #'(lambda nil (interactive) (org-todo "DELEGATED")))
	     (define-key org-todo-state-map "s"
	       #'(lambda nil (interactive) (org-todo "STARTED")))
	     (define-key org-todo-state-map "w"
	       #'(lambda nil (interactive) (org-todo "WAITING")))

	     (define-key org-agenda-mode-map "\C-n" 'next-line)
	     (define-key org-agenda-keymap "\C-n" 'next-line)
	     (define-key org-agenda-mode-map "\C-p" 'previous-line)
	     (define-key org-agenda-keymap "\C-p" 'previous-line)))

	(require 'remember)

	(add-hook 'remember-mode-hook 'org-remember-apply-template)

	(define-key global-map [(control meta ?r)] 'remember)

	(custom-set-variables
	 '(org-agenda-files (quote ("~/todo.org")))
	 '(org-default-notes-file "~/notes.org")
	 '(org-agenda-ndays 7)
	 '(org-deadline-warning-days 14)
	 '(org-agenda-show-all-dates t)
	 '(org-agenda-skip-deadline-if-done t)
	 '(org-agenda-skip-scheduled-if-done t)
	 '(org-agenda-start-on-weekday nil)
	 '(org-reverse-note-order t)
	 '(org-fast-tag-selection-single-key (quote expert))
	 '(org-agenda-custom-commands
	   (quote (("d" todo "DELEGATED" nil)
		   ("c" todo "DONE|DEFERRED|CANCELLED" nil)
		   ("w" todo "WAITING" nil)
		   ("W" agenda "" ((org-agenda-ndays 21)))
		   ("A" agenda ""
		    ((org-agenda-skip-function
		      (lambda nil
			(org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
		     (org-agenda-ndays 1)
		     (org-agenda-overriding-header "Today's Priority #A tasks: ")))
		   ("u" alltodo ""
		    ((org-agenda-skip-function
		      (lambda nil
			(org-agenda-skip-entry-if (quote scheduled) (quote deadline)
						  (quote regexp) "\n]+>")))
		     (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
	 '(org-remember-store-without-prompt t)
	 '(org-remember-templates
	   (quote ((116 "* TODO %?\n  %u" "~/todo.org" "Tasks")
		   (110 "* %u %?" "~/notes.org" "Notes"))))
	 '(remember-annotation-functions (quote (org-remember-annotation)))
	 '(remember-handler-functions (quote (org-remember-handler))))

The beginning of this code loads `org-mode` into memory, and customizes some of its default behaviors.  In particular, it adds certain key bindings both to regular `org-mode` (when you're editing a todo file) and also the agenda view. For example, I don't like the meaning of `C-n` and `C-p` to be overridden, so I modify them back to their original Emacs behavior.

After this, I load the `remember` package, which is a great companion to use with `org-mode`.  Finally, I configure both packages, which I describe here:

`org-agenda-files` is a list of all the files where todo items can be found.  This can be many -- although all the files must be present when `org-mode` is first invoked.

`org-default-notes-files` is used by `remember.el` in certain situations where it needs a path to store simple notes in.

`org-agenda-ndays` specifies how many days ahead the default agenda view should look.  It is important to set this to seven!

`org-deadline-warning-days` specifies how many days early a deadline item will begin showing up in your agenda list.  This number is a matter of preference; I like two weeks as a default.  Starting with `org-mode` 5.05, you can now specify the exact warning amount individually for each deadline, so it's not really necessary to change this default at all.

Setting `org-agenda-show-all-dates` to true means that in the agenda view, days that have no associated tasks will still have a line showing the date.  I like this because it lets me easily pick out "free days".

Setting `org-agenda-skip-deadline-if-done` means that deadline items which are marked as complete will not show up in your agenda view.  It's important to keep the daily agenda view as uncluttered as possible!

Setting `org-agenda-skip-scheduled-if-done` means that scheduled items marked as complete will not show up in your agenda view.

Setting `org-agenda-start-on-weekday` to `nil` means that the agenda view -- even in the 7-days-at-a-time view -- will always begin on the current day.  This is important, since while using `org-mode` as a day planner, you never want to think of days gone past.  That's something you do in other ways, such as when reviewing completed tasks.

Setting `org-reverse-note-order` to true means that notes are stored in descending date order -- in other words, the most recent note is always at the top.  This makes it easier to see your notes for the past week: by simply opening up your notes file.

Setting `org-fast-tag-selection-single-key` to `expert` makes it very quick and easy to assign common tags to entries.  I'll show how to use this feature later on, in the section titled "Tagging and metadata".

`org-agenda-custom-commands` contains custom agenda and todo list view commands,which can be accessed by type `C-c a` followed by the key letter of their definition.  Just use the setting I have if you don't understand it; I'll discuss each one more thoroughly in the section entitled "Creating custom
views".

Setting `org-remember-store-without-prompt` to true makes it possible to use `remember` without having to specify the file and parent subtree each time. That is, once you type `C-c C-c`, it squirrels away the info into the default location and immediate returns you back to what you were doing before.  When entering new tasks on the fly, speed -- and minimal distraction -- is of the essence.

`org-remember-templates` defines two templates I use with `remember-mode`.  Because I bind the `remember` function to `C-M-r`, with these templates I can quickly type either `C-M-r t` to joy a new task, or `C-M-r n` to create a brief note.

### A day planning template

The following is a file template you can use for starting your day planner. Just save this file as `todo.org` in your home directory:

	MY PROJECTS  -*- mode: org; fill-column: 78 -*-

	* Tasks
	#+CATEGORY: Tasks


	* Sample project milestone
	#+CATEGORY: MilestoneX
	  DEADLINE: 


	* School
	#+CATEGORY: School
	* Health
	#+CATEGORY: Health
	* Finances
	#+CATEGORY: Finance
	* Miscellaneous
	#+CATEGORY: Misc


	* Anniversaries and Holidays
	#+CATEGORY: Holiday
	%%(org-calendar-holiday)
	%%(diary-date 10 25 t) Grenada's Thanksgiving
	#+CATEGORY: Birthday
	%%(diary-anniversary  1 1 1960) Someone is %d years old


	#+STARTUP: content
	#+STARTUP: lognotestate
	#+SEQ_TODO: TODO STARTED WAITING DELEGATED APPT | DONE DEFERRED CANCELLED
	#+TAGS: { SCHOOL(s) WORK(w) } CALL(c) ERRAND(e)

Once you have this template, you can begin adding tasks to it.  I recommend using `remember` to add new tasks, as this can be done quickly and without interrupting the flow of your other work.  Soon you will have many new tasks added, lacking only in organization.  That is where the day planning process comes in.

## A typical day

The following "typical day" will show how I use `org-mode` to implement the concept of a digital day planner.  This day follows the life cycle of a specific group of tasks, from creation to conclusion.

### Step 1: Creation

Each task begins by first being entered into the system.  The key at this stage of the game is *speed*.  If your mechanism for entering new tasks is too slow, you will tend to avoid entering them -- and then you will forget them.

To maximize speed in this case, I use the Emacs `remember` package.  Bind it to an easily accessed global key.  I use `C-M-r`, replacing the default behavior which is `isearch-backward-regexp`.  This is just fine for me, because you can also type `C-u C-r` to do the exact same search.

So, let's enter a group of tasks.  If you're using "My configuration" from above, hit `C-M-r` followed by the `t` key to input a new task.  Fire off several tasks, it doesn't matter whether they are real ones or not.  The top of your `todo.org` file will end up looking something like this:

	MY PROJECTS  -*- mode: org; fill-column: 78 -*-

	* Tasks
	#+CATEGORY: Tasks
	** TODO This is task alpha
	   [2007-08-23 Thu]
	** TODO This is task beta
	   [2007-08-23 Thu]
	** TODO This is task gamma
	   [2007-08-23 Thu]
	** TODO This is task delta
	   [2007-08-23 Thu]
	** TODO This is task epsilon
	   [2007-08-23 Thu]

Each task has a description, an initial state of "TODO", and a date tag showing when it was added to the todo file.  I love date tagging because when it gets to be several months down the road, you'll often find yourself longing for all the contextual information you can get on some of your older-lived tasks.

*It's important that you not deal with your tasks just now*.  Simply let them accumulate.  Rapid-fire them into your todo file using `remember`.  Build up a huge list.  And then leave it there.  Go back to whatever else you were doing. Unless the task has a very high urgency and must be completed before the end of today, you should leave it alone.  Don't try to finish today the tasks you created today.  If you do, you're likely to get overwhelmed by the sudden storm of data processing, and that too will end up with your todo list becoming useless.

### Step 2: Filing your tasks

At the very end of each day, when you've concluded your efforts to "get things done", should come a quiet time of reflection and organizing.  Some people prefer to do this the following morning, which is just fine.  For me, night-time works best -- it's quieter, and I enter a lazy mood where I find it pleasant to just push bits around.  Do whatever works for you.

During this phase, your job is to go to your `Tasks` group at the top of your todo file and categorize your tasks.  This means two things: scheduling time to get them done, and moving them down to the correct category tree.

To schedule a task for a particular day, press `C-c C-s` and pick a date.  Don't forget that you can specify "+7" in order to do the task one week from now.  *It is critical that every task be assigned a date*.  If a task has no date assigned it means it will never get done, because in my system I never look at my todo file during the day.  In fact, if you don't want to assign a date to a task, just go ahead and change the status to "DEFERRED" right now.  Then type `C-c C-x C-s` to move it to your archive file.

If you are interested in doing the task, find a plausible day.  If you can't think of a day, just push it into the future somewhere; but get it out of today's queue!  When that future day comes, you can re-assess the task, and decide whether you want to defer it or assign an even better day.

Once a date has been scheduled, use cut and paste to move the task to the appropriate category.  I'm going to do this real quick with the example file, so that it now looks like this:

	MY PROJECTS  -*- mode: org; fill-column: 78 -*-

	* Tasks
	#+CATEGORY: Tasks


	* Sample project milestone
	#+CATEGORY: MilestoneX
	  DEADLINE: 
	** TODO This is task alpha
	   SCHEDULED: 
	   Here are some supporting notes I've added.
	   [2007-08-23 Thu]
	** TODO This is task epsilon
	   SCHEDULED: 
	   [2007-08-23 Thu]


	* School
	#+CATEGORY: School
	** TODO This is task beta
	   SCHEDULED: 
	   [2007-08-23 Thu]
	** TODO This is task gamma
	   SCHEDULED: 
	   [2007-08-23 Thu]
	* Health
	#+CATEGORY: Health
	* Finances
	#+CATEGORY: Finance
	** TODO This is task delta
	   SCHEDULED: 
	   [2007-08-23 Thu]
	* Miscellaneous
	#+CATEGORY: Misc

In the above, I've scheduled every task for a specific day and moved each one to its appropriate category tree or milestone.  You'll also note that I haven't set a priority yet for any of the tasks.  That happens later, in step four.

### Step 3: Briefly review the upcoming week

The next step to using your day planner is to jump to the weekly agenda view.  I do this by using `C-c a a`, since my configuration binds `C-c a` to the `org-agenda` command.  I've also configured `org-agenda-ndays` to 7 and `org-agenda-start-on-weekday` to nil, so my agenda view always starts out by showing me the next seven days.

What I do at this point is to walk through the seven days, making myself roughly aware of what I expect to do today, tomorrow, and what's coming up in a few days.  I may, at this point, decide to reschedule certain tasks for today, or push some from today into the near future.  I may even defer or cancel some tasks altogether, after seeing how many other things I have to do.

Here's the report I get from the sample data above for today, Thursday the 23rd:

	Week-agenda:
	Thursday  23 August 2007
	Friday    24 August 2007
	Saturday  25 August 2007
	Sunday    26 August 2007
	Monday    27 August 2007
	  MilestoneX: Scheduled:  TODO This is task epsilon
	  MilestoneX: Scheduled:  TODO This is task alpha
	Tuesday   28 August 2007
	  School:     Scheduled:  TODO This is task beta
	Wednesday 29 August 2007
	  School:     Scheduled:  TODO This is task gamma

As you can see, the next four days are completely free.  This may not be optimal, so I'm going to pick the "epsilon" task and reschedule it for today.  This is done by placing my cursor on the task, typing `C-c C-s`, and then hitting return to select today.  I then hit `r` to refresh the agenda display, which now looks like this:

	Week-agenda:
	Thursday  23 August 2007
	  MilestoneX: Scheduled:  TODO This is task epsilon
	Friday    24 August 2007
	Saturday  25 August 2007
	Sunday    26 August 2007
	Monday    27 August 2007
	  MilestoneX: Scheduled:  TODO This is task alpha
	Tuesday   28 August 2007
	  School:     Scheduled:  TODO This is task beta
	Wednesday 29 August 2007
	  School:     Scheduled:  TODO This is task gamma

Great!  I've got something to do today.  Are all the other tasks also scheduled for reasonable days?  If not, reschedule them.  I don't really spend much time at this point, since my interest is more in getting today right than in worrying about the future.  I pretty much just scan the tasks briefly, looking for anything that jumps out at me as wrong.

If everything looks OK at first blush, I hit `.` to move to the current day, and then `d` to switch to today's daily view.  It is in this view that you will spend most of your time, as you work toward getting everything accomplished.

### Step 4: Getting ready for the day

After hitting `d` in the last step, I'm now presented with my "home" for today: the daily agenda view.  It looks like this for the 23rd of August:

	Day-agenda:
	Thursday  23 August 2007
	  MilestoneX: Scheduled:  TODO This is task epsilon

Pretty clean, huh?  At most, I maybe have 15 tasks in this view.  If there are more, I usually reschedule several for the future.  I prefer to have less than ten, otherwise the chances of my finishing them all is too slim.  It's better to delay a few tasks today -- thus avoiding an impending sense of pressure and failure -- than to try to accomplish them all but fail anyway because you couldn't find the time.

When you first enter the daily view, all you'll see are the tasks scheduled for today.  As time goes by, it's quite likely that someday you will enter the daily view and find that older, unfinished tasks have crept up on you.  This is because `org-mode`, when a scheduled task is left undone, reschedules that task automatically for the current day along with a marker to show it has done so, which looks like this:

	Day-agenda:
	Thursday  23 August 2007
	  Computer:   Sched. 2x:  TODO [#B] Run DiskWarrior against MBP

The "Sched. 2x" means that the task has been scheduled two times: first on the day you intended it (the 22nd) and again today.  For every day that it's late, the number goes up by one.  Also, the coloring of the task changes from green to red, to indicate it's "past the scheduled date".

Although this feature might be useful to some, I don't like seeing late tasks.  The past is past.  Having a large set of late tasks pile up on you is a sure way to feel like you're getting far behind -- when in fact you might not be doing badly at all.  Use Deadlines (see the next section) for a sense of impending doom, and not scheduling.

In consequence of this, the first thing I do after switching to the daily view is to reschedule all my late tasks.  Typically, I just reschedule them for today by typing `C-c C-s RET`, but sometimes I like to push them into the future, or move other tasks into the future to make sure today doesn't get too crazy.

It's important not to overwhelm yourself at the daily level.  If you do, you'll most likely begin to suffer from a consistent sense of failure; a feeling that you "can't ever get ahead".  Such a depressive influence may cause you to avoid your task list altogether, and then you won't get anything done -- or at least, not the things you intended to!  Thus it's crucial to keep your daily task list small and achievable.  Start out really small, in fact: leave yourself as much free time as you can.  Once you discover your natural balance, you'll know instinctively what constitutes an unreasonable day and what an achievable one looks like.

Now that I have my daily view in this example, the next step is to arrange the tasks into priority order.  I must decide whether they are A: urgent and important; B: of moderate urgency or importance; or C: pretty much optional.  C tasks are typically very quick or fun to do, or else they're freebie tasks you're doing for someone else.  C tasks can always be scheduled for another day without much worry.  If the thought of rescheduling a task causes you to worry, upgrade it to a B or an A.

Use the `,` key to set your tasks' priorities.  I'm going to make my "epsilon" task an A task, so my daily list look like this:

	Day-agenda:
	Thursday  23 August 2007
	  MilestoneX: Scheduled:  TODO [#A] This is task epsilon

### Step 5: Doing the work

Your main goal each day should be to finish all your A tasks.  If you can manage this, it means you're on top of all the important things in your life.  *Remember: not all urgent tasks are important*.  If a task is urgent but not important, consider downgrading it to a B or a C.  If you can't get to it in time, its window of opportunity may "close" -- but then if it wasn't really important that shouldn't matter much.  Try to mark as "A" only those tasks which are both important *and* have to be done on the day you schedule them for.  Then, if you close all your As, you'll know that if you did nothing else today, no one would suffer.

This means that the great majority of your tasks will be B tasks.  That's OK.  My todo list contains a whole host of tasks of moderate importance and lukewarm urgency.  I fully intend to get them done, but the exact day isn't as critical as with an A task.  That way, when an A task's day does come up, I know to pay fuller attention to that one above the rest.  Also, when I have a full day and see several C tasks, I can immediate reschedule them for later in the week without even thinking about it.

I try to get as many B's done each day as my time and energy allow, but not finishing a B is not the end of the world.  B might as well stand for "bread and butter", since these are the kind of tasks that make up your day to day life.  A's should be relatively rare.  If you find yourself accosted by a horde of A's each day, you are being too anxious about priorities.  It's like a person who marks all their e-mails "high priority", thinking people will read them faster; in fact, all it really does is to render the meaning of priorities useless.

If you have a task list in your daily view right now, step 5 is about doing them!  You shouldn't be playing with your `org-mode` file any more.  In fact, during the day *do not touch it*.  Wait until your next review session at the end of the day -- or the beginning of the next day -- before you touch your `org-mode` file again.  The true sign of a functional task system is that it gets out of the way once work begins.

The next few steps will cover what to do as you're working on a task, and how to move it from stage to stage toward completion.

### Step 6: Moving to the next step

My "epsilon" task for today is marked "TODO".  The "TODO" state means it's a new task I haven't done anything about yet, and it's waiting for some kind of action.

Let's say I begin doing the work.  By the very fact of beginning I will transition the state of the task to "STARTED".  You'll notice I use the `lognotestate` logging facility of `org-mode`, which prompts for a note every time a task's state is changed.  I can't explain why, but entering comments whenever I change a task's state is very satisfying to me.  It feels like I'm getting something done -- even if I've done very little.  You may find this to be overkill, in which case I recommend using "logdone", or no logging at all.  But I've discovered that logging each state actually motivates me to change my tasks' states more often, which in turn motivates me to want to see them marked as "DONE".

Let's say "epsilon" has something to do with writing this article.  In fact, I'm going to change the title right now so the rest of the example makes more sense.  I've even changed the title of the milestone, and the milestone category, to make things a bit more realistic.  The daily view now reads:

	Day-agenda:
	Thursday  23 August 2007
	  org-mode:   Scheduled:  TODO [#A] Write article on using org-mode as a day planner

Now, I've already started typing out this article, so I'm going to change its state to "STARTED", and write a quick note about what I've done:

	# Insert note for state change, finish with C-c C-c.

	I've written the first half of the article so far.

This is what I wrote in the buffer that I was prompted with when I changed the task's state.  I changed its state by typing `C-u t` in the agenda buffer, and then typing "ST" followed by a return.  I don't use the default behavior of the `t` key -- todo cycling -- because it's not always appropriate.

After filling out the buffer, I press `C-c C-c` to record the state change.  The alters the task itself in my todo file, to look something like this:

	* Document org-mode
	#+CATEGORY: org-mode
	** STARTED [#A] Write article on using org-mode as a day planner
	   SCHEDULED: 
	   - State "STARTED"    [2007-08-23 Thu 15:27] \\
	     I've written the first half of the article so far.
	   [2007-08-23 Thu]

Here you can see the task, its new state, and the note I associated with the state change.  What's so nice is that all this information is kept forever!  When the task is finally done (or cancelled or deferred), I'll move it to the archive file, where all that lovely tracking information persists until the day I destroy my task files altogether.

But what if I'm doing research now for the rest of my article, and I find I have to write the `org-mode` author, Carsten, for a quick tip?  In that case I switch the task to a "WAITING" state and make a note about my situation:

	** WAITING [#A] Write article on using org-mode as a day planner
	   SCHEDULED: 
	   - State "WAITING"    [2007-08-23 Thu 16:30] \\
	     I wrote an e-mail to Carsten asking for some pointers.
	   - State "STARTED"    [2007-08-23 Thu 15:27] \\
	     I've written the first half of the article so far.
	   [2007-08-23 Thu]

The task is now "WAITING", which means I can't do anything until the event I'm waiting for has happened.  Carsten may not respond today, so I go ahead and immediately reschedule the task for tomorrow.  Whenever I see a "WAITING" task in the daily task list, I always look at the notes for the task (by selecting the task and hitting TAB) to see what it is I'm waiting for.  If the event still hasn't happened, I just keep rescheduling it until either I give up or the awaited event occurs.

In this example, I rescheduled this task for Friday.  Let's say it's now Friday and Carsten wrote back a nice message answering my question.  This means it's time to resume the task.  I switch the state back to the "STARTED" and make a note describing Carsten's respond, with a link back to the e-mail he wrote (see the manual on how to store and insert links to messages).

When the article is finally finished, I'll change the task's state to "DONE".  If I'd chosen to put it off indefinitely, I would mark it "DEFERRED".  If I'd chosen never to do, I would mark it "CANCELLED".  All three of these are "completion states", which mean the task never again shows up in any agenda view.

### Step 7: Archiving tasks

It's no good to let your `todo.org` file get cluttered up with completed tasks.  During each day's "review and categorize" process (see step 2), I walk through all my completed tasks and archive each one by putting my cursor on it and pressing `C-c C-x C-s`.  This magically appends the completed task to the end of my archive file, and cleans up the todo file.  Since you'll almost never actually visit your archive file -- except to ferret out a choice bit of information someday -- it's OK to let the archive file grow without bounds.

To easily find which tasks should be archived, use a custom report which looks for done, cancelled and deferred tasks.  You can find the definition for such a report in "My configuration" above; or more fully described in a later section titled "Creating custom views".  If you're using my configuration, all you have to type is `C-c a c`.

### Step 8: When new tasks come up

This step is really just a repeat and reminder of step 1: If you're working on your daily tasks for the given day and a new task pops up, don't switch to your todo file and try to schedule it right then!  Not even if it would be a simple and quick thing to do.  Rather, use the `remember` package to fire the task into your inbound queue, for processing later that evening (or the next morning).

The reason for this is that entering new tasks should be impulsive, not reasoned.  Your reasoning skills are required for the task at hand, not every new tidbit.  You may even find that during the few hours that transpire between creating a task and categorizing it, you've either already done it or discovered it doesn't need to be done at all!  So shuffle away those new tasks using `remember`, and leave the categorizing and scheduling for a time when your mind is free to think about them.  New stuff can almost always be done tomorrow; in fact, it's better for your mental sanity to delay interruptions until you can deal with them on your time, not theirs.

And for when I don't have my computer handy, I use a digital voice recorder to quickly note down new tasks.  I speak only the minimum information required to create the task and provide a bit of context -- no more.  Then, during my review and categorization process, I play back the notes and enter tasks for them using `remember`.  Only after all that do I decide if I want to schedule them at all.  Typically, more than a third of my voice notes never even get scheduled, as they're sometimes more a "spur of the moment desire" than an actual thing I want to get done.

### Review and summary

To summarize what we've discussed, here are the steps I use to manage tasks "day-planner style" using `org-mode`:

 1. Rapidly (almost "mindlessly") create new tasks using `remember`.

 2. Sit down each night and schedule/categorize those remembered
    tasks.  Also, I move all completed tasks to the archive to clean
    up my todo file.

 3. Each morning, start up the agenda view with a 7 day view on the
    future, and briefly scan to see if my week looks/feels right.
    At best, I maybe push a few tasks around to make things more
    balanced; but most of the time, I prefer to leave the future
    alone.

 4. Switch to daily view and set priorities for the day's tasks.
    Is there anything I can defer to another day?  I like to see
    less than 15 tasks in this view.

 5. Paying special attention to my A tasks, I begin doing what I
    can to complete the day's work load.  I switch task states
    frequently, adding notes on what I've done each time.  This is
    the most satisfying part of using `org-mode` for me, though I
    can't really explain why.

 6. During the day, if anything new comes up I use `remember` to
    jot down the task and then promptly -- intentionally -- forget
    about it.  Don't clutter your brain!  I use a digital voice
    recorder when `remember` isn't handy.

## Scheduling, deadlines and appointments

There are four ways of associating a date or time, or range of dates and
times, with a task:

 1. Scheduling the task for a particular day or time.  This indicates
    your intention to work on that task on that day.  You might not
    finish it then -- in which case it gets rescheduled for when
    you plan to continue -- but at least you hope to work on it a
    bit that day.

 2. Setting a deadline for a task.  This means the task has to be
    completed by the given day.  Sometimes you will have non-task
    deadlines just to help keep you aware, with regular, associated
    tasks each scheduled on the days leading up to the deadline.
    Either way, the deadline task starts appearing in your agenda
    view based on its "lead time", and is shown every day from that
    point until resolution.  After the due date, it appears each
    day in bolded red, to indicate you must either finish the task
    or cancel it ASAP.

 3. Associating a date or time with a task.  This is different from
    a scheduled date, which indicates a desire to work on the task
    that day; and it's different from a deadline, which says that
    work must be finished by that day (but should be done before
    it).  A dated task means that the task is only meaningful during
    the exact dates and times associated with it.  I use this kind
    of dating to indicate appointments, vacations, conference times,
    classes, etc.  It doesn't even have to be a "task" necessarily;
    I use the "APPT" keyword to note such items, but even that is
    optional.  If you just want to be aware of when a particular
    thing is going to happen, create an outline entry and put a
    date on it.

 4. Associating an inactive date or time with an item.  This is
    just like the previous type, except that inactively dated items
    never appear in your agenda view.  They are used for historical
    tracking only, like the dates that are stored when changing the
    state of a todo item.

Here are simple examples of the above four types:

	* Document org-mode
	#+CATEGORY: org-mode
	** TODO This is a scheduled task
	   SCHEDULED: 
	** TODO This is a weekly scheduled task
	   SCHEDULED: 
	** TODO This is deadline task, with notification starting 2 weeks before
	   DEADLINE: 
	** TODO This is a dated task, it only has meaning on the given day
	   
	** APPT This is a dated appointment, with a time range
	   
	** A dated event, but requiring nothing from me; it's just awareness
	   
	** This is a passively dated task, more like a log entry
	   [2007-08-27 Mon]

At first these differences can seem confusing, and you'll wonder how to decide which style to use for different kinds of items.  But just try it for a while, and soon you'll discover what works best for your workflow.  I use a mixture of all the examples above, depending on what kind of meaning I want my agenda view to reflect.

## Creating custom views

I find that the true power of `org-mode` lies in this: that it combines a simple input methodology -- maintaining a regular Emacs outline -- with a rich and flexible output methodology -- a set of fully customizable views.  In this section I want to show you some of the custom commands I've created in more detail, to give you ideas for others you might want to define on your own.

First of all, the variable `org-agenda-custom-commands` is a fairly complicated list with lots of options.  By far the best way to manipulate this list is using Emacs' customization feature.  I'm going to use it in my examples here.  To custom this variable, type `M-x customize-option` and enter the variable name `org-agenda-custom-commands`.  Here is what each of my own custom reports looks like on this screen, following by a brief description of each.

	Choice: [Value Menu] Single command:
	Key: d
	Choice: [Value Menu] TODO keyword search (all agenda files)
	Match: DELEGATED
	Local options:
	[INS]
	[ ] Export:
	    [INS]

This custom command is bound to the letter `d`, which means I have to type `C-a a d` to invoke this report (since `C-a a` is my `org-mode` command prefix, set in the sample settings at the beginning of this document).  I define this custom report to be a TODO keyword search so that it searches all the agenda files listed in `org-agenda-files`, looking for TODO items which have a matching state.  `org-mode` calls TODO states "keywords".  So, WAITING and DELEGATED are both todo keywords.  The "Match" definition for this report looks for the DELEGATED keyword.

To put it all together, whenever I type `C-a a d`, I see a screen listing every DELEGATED todo item within each agenda file mentioned in `org-agenda-files`.  This allows me to quickly see how much work I've "farmed out", and whether I need to start pinging people for responses.

One note about DELEGATED tasks: I find it very useful to schedule delegated tasks for my own agenda list on particular days.  This tells me that my "work" on the task for that day will be to ping the delegated person and check on their status.  If more time is needed, I reschedule the delegated task for another day when I intend to ask them again.

	Choice: [Value Menu] Single command:
	Key: w
	Choice: [Value Menu] TODO keyword search (all agenda files)
	Match: WAITING
	Local options:
	[INS]
	[ ] Export:
	    [INS]

Like the DELEGATED report, this report shows all WAITING tasks -- or tasks for which I'm waiting on an event, resource, or the completion of some other activity.  The difference between delegated and waiting tasks is that delegated tasks involve some kind of agreement between myself and the person I'm waiting on.  If I delegate, the person know I've given the task to them and am expecting a response.

	Choice: [Value Menu] Single command:
	Key: c
	Choice: [Value Menu] TODO keyword search (all agenda files)
	Match: DONE|DEFERRED|CANCELLED
	Local options:
	[INS]
	[ ] Export:
	    [INS]

This todo report looks for all tasks which are at some kind of completion state: done, deferred or cancelled.  I archive these at the end of each day, once I look through them and assure myself they can rightly disappear from the agenda and todo views forever.

	Choice: [Value Menu] Single command:
	Key: W
	Choice: [Value Menu] Agenda
	Match: 
	Local options:
	[INS] [DEL] List:
	            Option: org-agenda-ndays
	            Value: 21
	[INS]
	[ ] Export:
	    [INS]

This next report is an agenda report, not a todo list report.  It provides a customized version of the same agenda view I normally use for looking at the coming week or the present day.

In this rendition of the report (tied to the letter `W`), I'm asking it to show me the next 21 days, instead of the default 7.  I use this sometimes when lots of things are going on, and I want to a "heads up" on deadlined tasks soon to creep up on me.  But I rarely look at this report, as compared to the others.

	Choice: [Value Menu] Single command:
	Key: A
	Choice: [Value Menu] Agenda
	Match: 
	Local options:
	[INS] [DEL] List:
	            Option: org-agenda-skip-function
	            Value: 
	(lambda nil
	  (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]"))
	[INS] [DEL] List:
	            Option: org-agenda-ndays
	            Value: 1
	[INS] [DEL] List:
	            Option: org-agenda-overriding-header
	            Value: "Today's Priority #A tasks: "
	[INS]
	[ ] Export:
	    [INS]

This report is much more complicated, though it's meaning is quite simple: Show me only priority A tasks for the current day.  You can see from this definition that I'm defining an agenda report; I'm setting `org-agenda-skip-function` to a custom lambda form, whose job is to skip all tasks in my `org-mode` files whose PRIORITY keyword is not set to `[#A]`; finally, I set `org-agenda-overriding-header` to change the title of the agenda report -- lest I forget and fail to remember to look for my B and C tasks.

	Choice: [Value Menu] Single command:
	Key: u
	Choice: [Value Menu] TODO list
	Match: 
	Local options:
	[INS] [DEL] List:
	            Option: org-agenda-skip-function
	            Value: 
	(lambda nil
	  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))
	[INS] [DEL] List:
	            Option: org-agenda-overriding-header
	            Value: "Unscheduled TODO entries: "
	[INS]
	[ ] Export:
	    [INS]

This report is similar to the previous one, except it creates a todo list showing all unscheduled, undeadlined, and undated tasks in any todo file.  I can then decide either to schedule them, or archive them, without having to individually visit each file.

## Tagging and metadata

Metadata is quite simply "data about your data".  For example, you may have a task called "Do laundry".  Some of the metadata associated with this task might be:

 - When the task was first created
 - When it was last completed
 - When it's scheduled to be done next
 - How often it should get done
 - What priority task is it
 - What state is the task presently in
 - Are there any instructions defined for the task
 - Is it being done on behalf of anyone
 - Etc., etc.

Many of these details are managed by `org-mode` automatically, using different schemes.  But it's also possible to add your own metadata, with entirely unique meanings customized to your usage of `org-mode`.  These are called "tags", and they are set using the command `C-c C-c` while the cursor is on a task.

One person might use tags to define contexts where a task should be performed.  Another might use them to identify tasks being done for the sake of different people.  Another might distinguish between personal, school and work tasks. Or you may want to use all these together!

The quickest way to start using tags is to define them right at the bottom of your todo file.  Here's something similar to what I use:

	#+TAGS: { FAMILY(f) WORK(w) } CALL(c) ERRAND(e)

The braces delimit exclusive tags: only one from each brace group can be set at a time.  The other tags are not exclusive and can be joined together.  The single letters in parentheses after each tag define a "quick letter" which can be typed immediately after typing `C-c C-c`.  The reason this works is that in "My configuration", I configured the variable `org-fast-tag-selection-single-key` to `expert`.

When you tag a task, its title gets appended with a tag string, like ":FAMILY:".  You can search for all tasks in the current todo file by typing `C-c \` followed by the tag string.  You can see a list of all todo items in all files matching a tag by typing `C-c a m` for the "items matching tags" report.

Tags can be joined together when setting them.  Also, tag searching can be made fairly complex, using an expression logic described in the `org-mode` manual.  Read further there in the section on Tags.  Some people use tags to emulate a GTD type environment, although I haven't found the idea of "contexts" useful for me.  I prefer to use tags to call out special exceptions to the general rule, such as quickly finding all telephone calls I need to make today -- or seeing which tasks should be completed for Work, as opposed to personal tasks.

## Conclusion

This is the system I use, and for the past month is has been working wonders for me.  Previous to `org-mode`, the best system I had found was the Mac and Windows application [Life Balance][].  However, I also found that Life Balance, as nice as it is, too often devolves into a plain, gargantuan outline which I then promptly avoided.  By using `org-mode` to provide me with a meaningful daily view, I was able to prevent that overwhelming feeling and find a task management system which is actually rewarding and pleasant to use.

[Life Balance]: http://www.llamagraphics.com/LB/LifeBalanceTop.html

