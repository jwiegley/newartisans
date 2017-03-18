---
title: Moving to Movable Type
---

The blog has now fully moved over to Movable Type, including all past articles and their comments.  It took a bit of Perl, Python and mucking with SQL, but now the transfer is complete.

The reason for the move is that the app I was using, RapidWeaver, was beginning to introduce a bit too much inertia to the blogging process.  And one thing I know about myself: if something isn't dead simple, even after months of being away from it, I'll avoid it forever.

I write these blog posts using [ecto](http://illuminex.com/ecto/) now, which couldn't be easier.  There's no separate publishing step, it's like writing and sending an e-mail.

I actually liked the way WordPress looks a bit more, but Movable Type supports PostgreSQL, which is what ever other service on this server uses.  And for some reason MT's XML-RPC script doesn't work with FastCGI and Apache, which is something I guess I can live with.

