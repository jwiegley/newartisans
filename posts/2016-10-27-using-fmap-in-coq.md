---
title: Using the FMap library in Coq
description: desc here
tags: coq
date: 2016-10-27 13:49
category: Coq
---

One of the most common structures used in programming are key/value maps, also
called hash maps, dictionaries, association lists, or simply functions. These
maps generally provide a way to add new values, lookup keys, iterate over the
collection, etc. Yet in Coq, even though this facility exists in the standard
library under the module `FMap`, it can be quite difficult to get started
with. This post intends to clarify the typical patterns in a way that is easy
to copy into your own project, based on the four different ways this library
is typically used.

## Using a known key type and map structure

Very often, one maps from a known, ordered type, like `nat`, to some other
type, using one of the concrete structures offered by the `FMap` library. In
that case, the code you want to start with looks like this:

``` coq
Require Import
  Coq.FSets.FMapList
  Coq.Structures.OrderedTypeEx.

Module Import M := FMapList.Make(Nat_as_OT).
```

You can now create a map using `M.t A`, where `A` is your value type. You can
prefix the map-related functions with `M.`, or just call them directly. Some
common function to use on maps are as follows:

  - `empty`
  - `add`
  - `remove`
  - `find`
  - `mem`
  - `is_empty`
  - `map`
  - `mapi`
  - `map2`
  - `fold`

There are also several relations you can use to phrase theorems about maps and
map membership:

  - `In`
  - `MapsTo`
  - `Equal`
  - `Equiv`
  - `Equivb`
  - `Empty`

## Additional functions and lemmas

In order to complete most proofs concerning maps, there are additional lemmas
and functions you'll want to include:

``` coq
Require Import
  Coq.FSets.FMapFacts.

Module P := WProperties_fun N_as_OT M.
Module F := P.F.
```

This provides two new prefixes, `P.` and `F.`, which bring into scope many
more helper functions and lemmas:

  - `P.of_list`
  - `P.to_list`
  - `P.filter`
  - `P.for_all`
  - `P.exists_`
  - `P.partition`
  - `P.update`
  - `P.restrict`
  - `P.diff`

Helper lemmas in the `F` module are generally best found using `SearchAbout`
for the specific lemma you need. There are too many to list here, and they're
often quite specific in their use, such as `F.find_mapsto_iff` to reflect
between the fact of a successful `find` operation, and its equivalent `MapsTo`
relation.

## Proofs involving maps

There are several induction principles you will need for completing inductive
proofs over maps:

  - `P.map_induction`
  - `P.map_induction_bis`
  - `P.fold_rec`
  - `P.fold_rec_bis`
  - `P.fold_rec_nodep`
  - `P.fold_rec_weak`

The `P.map_induction` induction principle treats each intermediate map as an
`Add` relation over a previous map, until it reaches the base `Empty` map. The
`_bis` variant expresses the same information as successive calls to `add`
down to an `empty` map.

`P.fold_rec` should be applied if the goal has the form of a call to `M.fold`
over a map. If you use this, be sure to `revert` into the goal any hypotheses
referring to the same map, since you'll likely want to use those facts as part
of the induction.

Note that these two sets of principles are used somewhat differently from each
other:

``` coq
-- Applies to any evidence in the context involving [m].
induction m using P.map_induction bis.

-- Applies only to evidence in the goal, thus sometimes
-- requiring use of [revert].
apply P.fold_rec.
```

## Rewriting with maps

Since the internal structure of maps is not exposed by the `FMap` interface,
rewriting can sometimes be a little confusing. Equality between maps is
expressed by the equivalence `Equal`, which states that anything found in the
first map is found at the same key in the second map.  In other words:

``` coq
forall k v, M.MapsTo k v m1 <-> M.MapsTo k v m2
```

This isn't a problem if the terms you're rewriting involve functions from the
`FMap` modules, but if you create a new function that operates on maps, you'll
need to accompany it with a proof relating it to `Equal`.  For example:

``` coq
Definition map_operation `(m : M.t A) : M.t A := ...

Lemma map_operation_Proper :
  Proper (Equal ==> Equal) map_operation.
```

Now you can `rewrite` the arguments in a `map_operation`, provided you know
they are `Equal`.

Also, if you find yourself facing difficulties using `rewrite` with folds,
note that in addition to establishing a proof that the fold function is
`Proper` for its arguments and result, you must also show that the final
result is independent of the order of evaluation, since it's not known from
the `FMap` interface whether the contents of a map are reordered during
insertion or not.

## Abstracting the map implementation

Often when using maps, it's not necessary to pick an implementation, you just
need the map interface over a known key type. To do this, you just need to
place your code in a module that itself requires and passes along the
implementation module:

``` coq
Require Import
  Coq.FSets.FMapFacts
  Coq.Structures.OrderedTypeEx.

Module MyModule (M : WSfun Nat_as_OT).

Module P := WProperties_fun Nat_as_OT M.
Module F := P.F.
...
End MyModule.
```

To later instantiate such a module functor using a map implementation, you'd
write:

``` coq
Require Import
  Coq.FSets.FMapFacts
  MyModule.

Module Import M := FMapList.Make(Nat_as_OT).
Module Import MyMod := MyModule M.
```

## Abstracting over both map and key

When implementing generic algorithms that are applicable to any map, you'll
also need to abstract over the key type. In this case, you have two choices:
Do you need to know that the key type is ordered, or do you only need to know
that it's decidable? Often the latter suffices, making the algorithm even more
general.

In both cases, you may refer to the key type as either `E.key` or `M.key`
(since the `M` module re-exports `key`), and you can check for key equality
using `E.eq`:

``` coq
Require Import
  Coq.FSets.FMapFacts
  Coq.Structures.DecidableTypeEx.

Module MoreFacts (E : DecidableType) (M : WSfun E).

Global Program Instance filter_Proper {elt} : forall P,
  Proper (E.eq ==> eq ==> eq) P
    -> Proper (M.Equal (elt:=elt) ==> M.Equal) (@P.filter elt P).
...

End MoreFacts.
```

To require an ordered type, which makes `E.lt` available, use:

``` coq
Require Import
  Coq.FSets.FMapFacts
  Coq.Structures.OrderedTypeEx.

Module MoreFacts (E : OrderedType) (M : WSfun E).
...
End MoreFacts.
```

## Putting it all together

Since you probably came here just wondering how to construct a map, add stuff
to it, and then search for what you added, here is a complete example you can
cut and paste to start off with:

``` coq
Require Import
  Coq.FSets.FMapAVL
  Coq.FSets.FMapFacts
  Coq.Structures.OrderedTypeEx
  PeanoNat.

Module Import M := FMapAVL.Make(Nat_as_OT).

Module P := WProperties_fun Nat_as_OT M.
Module F := P.F.

Compute M.find 1 (M.add 1 10 (M.empty _)).
Compute P.for_all (fun k _ => k <? 10) (M.add 1 10 (M.empty _)).
```

Also note that there is `N_as_OT`, which is much faster to compute with if you
are using large constants, but it requires familiarity with the `NArith`
library.
