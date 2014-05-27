---
title: The feature I avoided for half a year
description: desc here
tags: algorithm, commodity
date: [2009-01-21 Wed 21:15]
category: Uncategorized
id: 208
---

The other day I finally implemented a feature in Ledger which I'd avoided doing for a full half-year.  The reason?  Every time I thought about it, my brain kept shutting down.  It seems my brain doesn't care for math much, or for mathy problems, so it always seemed as if something better needed doing...

<!--more-->
The problem turned out to be a fairly straightforward one, it just required sitting down and mapping it out for a couple of hours before the coding began.  Here's the synopsis:

You have a network of N nodes, each of which can be connected to N-1 other nodes.  There can be multiple connections between any two nodes, where each connection has a date -- but no two connections between the same nodes can have the same date.

Given a start node, a query date, and a set of target nodes (which may be zero, one or many), find the shortest and youngest path that is not older than the query date, from the start node to each of the target nodes.

Ledger uses this algorithm to record price conversions between commodities, and to later render each commodity into a market value relative to another known commodity.  Sometimes such renderings are not possible, or sometimes they require multiple conversion steps before a value can be found.

For example, if I bought 10 shares of AAPL for $30.00, and later exchanged $10.00 for 9.83 CAD, and at one point exchanged 80 EUR for 100 CAD, then how many EUR are my shares of AAPL worth?

Previously Ledger could only render AAPL in terms of dollars, but now it can finally report any commodity in terms of any other, provided there exists a path of traversal between the two nodes which is older than or equal to the query date.

