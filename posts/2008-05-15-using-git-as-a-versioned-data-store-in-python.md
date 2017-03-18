---
title: Using Git as a versioned data store in Python
category: Uncategorized
---

Git has sometimes been described as a versioning file-system which happens to support the underlying notions of version control.  And while most people do simply use Git as a version control system, it remains true that it can be used for other tasks as well.

For example, if you ever need to store mutating data in a series of snapshots, Git may be just what you need.  It's fast, efficient, and offers a large array of command-line tools for examining and mutating the resulting data store.

To support this kind of usage -- for the upcoming purpose of maintaining issue tracking data in a Git repository -- I've created a Python class that wraps Git as a basic `shelve` object.

<!--more-->
Here is how you normally use the standard `shelve` module:

    import shelve

    data = shelve.open('data.db')

	# data.db may or may not have existed on disk before now.  If not,
	# We're Manipulating an Empty Dictionary.  If so, we can examine or
	# modify the previous run's state data.  In both cases, the database
	# is manipulated like a standard Python dictionary.

	data[key] = "Hello, world!"
	data.sync()        # Write out changes to the dictionary

	del data[key]
	data.close()       # Close and clean up, sync'ing only if necessary

This provides the simplest kind of database, without any query language or notion of whether previous state did or did not exist.  Both of those are services you'd have to layer on top of the `shelve` object if you wanted them.

Now consider `gitshelve`.  Whereas the Python `shelve` module stores your data by pickling all of the dictionary values, I pass whatever data you place in the dictionary straight on to Git's standard input.  In the default mode, this means you work strictly with string data:

    import gitshelve

    data = gitshelve.open(repository = '/tmp/data.git')

    data[key] = "Hello, world!"
	Data.Sync()                  # Repository is created if it doesn't exist
	
    del data[key]
	data.close()
	
The interface is identical, but with the Git version you can now examine the resulting repository's yourself, using regular Git commands:

    $ GIT_DIR=/tmp/data.git git log

By default, the commits have no associated comment text, but the `sync` method doesn't accept parameters.  If you wish to add transaction notes, use the `commit` method instead:

    data.commit("This is a comment")

You can store data this way either in a separate repository, or in named branches within any repository.  If the `repository` argument is not given, the named branch within the current Git repository is used.  An exception will be raised, however, if you do this and there is no Git repository related to the current directory.

    # I'm expecting to use the 'data' branch of the current repository, but
    # I ran the script in a directory unknown to Git!
    data = gitshelve.open(branch = 'data')

    # It appears to work, because no Git commands are run until the last
    # possible moment
    data['foo/bar/hello.txt'] = "Hello!"

    # This raises an exception, because there is no current repository.  To fix
    # it, either run "git init", or use a specific 'repository' argument above.
    data.commit("I just said hello")

The really nice thing about using Git this way is that you get all of its best features for free.

# Added non-text values

If you have a need to store non-textual values, you'll have to let gitshelve know how to deal with them.  I don't do any such handling by default, because of the big chance of doing the wrong thing, and having you not find out about it until it's much too late.  Just pickling data like `shelve` does isn't very smart, for example, because it will wreak havoc on Git's merge algorithms should you ever need to incorporate new data from another source.

So, let's see how to add a custom data translator.  First, you need to subclass a new type of `gitbook`, which is the wrapper used to interface with the blobs in the Git repository.  There are only two methods you need to override:

    class my_gitbook(gitshelve.gitbook):
        def serialize_data(self, data):
            return object_to_string(data)

        def deserialize_data(self, data):
            return object_from_string(data)

Now you must define `object_to_string` and `object_from_string`, which should examine the types of the objects passed and turn them into merge-friendly string as appropriate.  Certain forms of XML work well for this job, as do ini-style configuration files in some cases.  It's up to you and what works best for your usage.

Once you have this new class type, you must pass it to the `gitshelve.open` function:

    data = gitshelve.open(repository = '/tmp/foo', book_type = my_gitbook)

# Making things even faster

Every time you open a `gitshelve`, it must walk through the assoicated branch and determine its contents in order to build the key/value relationships in the dictionary.  If you find that this ever gets slow, what you can do is just pickle the gitshelve!  The only caveat is that you must take care to delete it if the HEAD you created it from is different from the current HEAD.  Here's an example:

    import gitshelve
    import cPickle
    import os

    data = None
    if os.path.isfile('data.cache'):
        fd = open('data.cache', 'rb')
        data = cPickle.load(fd)

        # I'm using an arbitrary file name here, __HEAD__
        if data['__HEAD__'] != data.current_head():
		    data = None       # Out of date, we can't use it

    if not data:
        data = gitshelve.open(branch = 'data')
        data['__HEAD__'] = data.current_head()

    # ... for data sets with enormous quantities of tiny files, this
    #     could really speed things up ...

# Where can you get it?

The `gitshelve` module is being maintained as part of the `git-issue` project, which is yet another attempt to bring distributed bug tracking to Git.  Actually, I tend to support multiple repositories as data backends, but right now Git is my initial focus.  You can clone the project and test it out as such:

    git clone git://github.com/jwiegley/git-issues.git
    cd git-issues
    python t_gitshelve.py

If see "OK" at the end of the unit tests, you're good to go!  There isn't much documentation on gitshelve.py itself right now, beyond this blog entry, but then again the `shelve`-like interface is simple enough that you really shouldn't need much more.

Or if you prefer, you can just browse the project at the [GitHub project page](http://github.com/jwiegley/git-issues/).

