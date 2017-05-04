---
title: Monads are monoid objects
tags: haskell
---

Lately I've been working again on my Category Theory formalization in Coq, and
just now proved, in a completely general setting, the following statement:

> Monads are monoid (objects) in the (monoidal) category of endofunctors
> (which is monoidal with respect to functor composition).

The proof, using no axioms,
is
[here](https://github.com/jwiegley/category-theory/blob/master/Isomorphism/Monoid/Monad.v#L22).

Now, just how much category theory was needed to establish this fact?

### Categories

We start with concept of a category, which has objects of some Type, and
arrows between objects of some other Type. In this way, objects and arrows can
be almost anything, except they must provide: identity arrows on every object,
and composition of arrows, with composition being associative and identity
arrows having no effect on composition.

All arrows between two objects forms a set of arrows, called a "hom-set". In
my library, these are actually constructive hom-setoids, allowing a
category-specific definition of what it means for two members of a hom-setoid
to be "equivalent". The fact that it is constructive means that the witness to
this equivalence must be available to later functions and proofs, and not only
the fact that a witness had been found.

### Functors

Given two categories, which may have different objects, arrows and hom
equivalences, it is sometime possible to map objects to objects, arrows to
arrows, and equivalences to equivalences, so long as identity arrows,
composition, and the related laws are preserved. In this case we call such a
mapping a "functor".

### Natural transformations

While functors map between categories, natural transformations map between
functors, along with a "naturality" condition that performing the
transformation before or after utilizing the related functors has no effect on
the result.

### Isomorphisms

Two objects in a category are said to be isomorphic if there are arrows for
one to the other, and back, and the composition of these two arrows is
equivalent to identity in both directions.

Note that since the type of objects and arrows is unknown in the general case,
the "meaning" of isomorphism can vary from category to category, as we will
see below in the case of Cat, the category of all categories.

### Cartesian categories

Although objects are just abstract symbols, sometimes it's possible to reveal
additional structure about a category through the identification of arrows
that give us details about the internal structure of some object.

One such structure is "cartesian products". This identifies a product object
in the category, in terms of introduction and elimination arrows, along with a
universal property stating that all product-like objects in the category must
be mappable (in terms of their being a product) to the object identified by
the cartesian structure.

For example, I could pick tuples `(a, b)` in Haskell as a product , or some
custom data type `Tuple`, or even a larger data structure `(a, b, c)`, and all
of these would be products for `a` and `b`. However, only tuples and `Tuple`
are universal, in the sense that every other product has a mapping to them,
but not vice versa. Further, the mapping between tuple and `Tuple` must be a
isomorphism. This leaves me free to choose either as the product object for
the Haskell category.

Note: Cartesian categories are not used in the final result, I just want to
clarify the difference between this, and a product category. Cartesian can be
seen as an internal product, and product categories as an external product.

### Product categories

Whereas cartesion categories tell us more about the internal structure of some
product object in a category, product categories are a construction on top of
some category, without adding anything to our knowledge of its internals. In
particular, a product category is a category whose objects are pairs of
objects from some other category, and whose arrows are pairs of the
corresponding arrows between those two objects. Arrow equivalence, identity
and composition, follow similarly. Thus, every object in a product category is
a product, and arrows must always "operate on products".

### Bifunctors

If a functor maps from a product category to some other category (which could
also be another product category, but doesn't have to be), we call it a
bifunctor. Another way to think of it is as a "functor of two arguments".

### Endofunctors

A functor that maps a category to itself (though it may map objects to
different objects, etc) is called an endofunctor on that category.

### The category of endofunctors

The category of endofunctors on some category has as objects every
endofunctor, and as arrows natural transformations between these endofunctors.
Here identity is the identity transformation, and composition is composition
between natural transformations. We can designate the category of endofunctors
using the name `[C, C]`, for some category `C`.

### Monoidal categories

A monoidal category reveals the structure of a tensor operation in the
category, plus a special object, the unit of the tensor operation. Along with
these come laws expressed in terms of isomorphisms between the results of the
tensor:

    tensor : C × C ⟶ C where "x ⨂ y" := (tensor (x, y));
    I : C;
  
    unit_left  {X} : I ⨂ X ≅ X;
    unit_right {X} : X ⨂ I ≅ X;
  
    tensor_assoc {X Y Z} : (X ⨂ Y) ⨂ Z ≅ X ⨂ (Y ⨂ Z)
    
Note that the same category may be monoidal in multiple different ways. Also,
we needed product categories, since the tensor is a bifunctor from the product
of some category `C` to itself.

We could also have specified the tensor in curried form, as a functor from `C`
to the category of endofunctors on `C`:

    tensor : C ⟶ [C, C]
    
However, this adds no information (the two forms are isomorphic), and just
made some of the later proofs a bit more complicated.

### Monoidal composition

The category of endofunctors on `C` is a monoidal category, taking the
identity endofunctor as unit, and endofunctor composition as the tensor. It is
monoidal in other ways too, but this is the structure of interest concerning
monads.

### Monoid categories

A monoid object in a monoidal category is an object in the category, plus a
pair of arrows. Let's call the arrows `mappend` and `mempty`. These map from a
tensor product of the monoid object to itself, and from the monoidal unit to
the monoid object, along with preservation of the monoid laws in terms of
arrow equivlances. In Coq it looks like this:

    Context `{C : Category}.
    Context `{@Monoidal C}.

    (* Here [mon] is the monoid object. *)
    Class Monoid (mon : C) := {
      mappend : mon ⨂ mon ~> mon;
      mempty : I ~> mon;
    
      mempty_left : (* I ⨂ mon ≈ mon *)
        mappend ∘ bimap mempty id ≈ to (@unit_left C _ mon);
      mempty_right : (* mon ⨂ I ≈ mon *)
        mappend ∘ bimap id mempty ≈ to (@unit_right C _ mon);
    
      (* (mon ⨂ mon) ⨂ mon ≈ mon ⨂ (mon ⨂ mon) *)
      mappend_assoc :
        mappend ∘ bimap mappend id
          ≈ mappend ∘ bimap id mappend ∘ to tensor_assoc
    }.

### Monads are monoid objects

Given all of the above, we can now state that every monad is a monoid object
in the monoidal category of endofunctors, taking composition as the tensor
product. `return` is the `mempty` natural transformation of that object, and
`join`, the `mappend` natural transformation:

    Context `{C : Category}.
    Context `{M : C ⟶ C}.
    
    Definition Endofunctors `(C : Category) := ([C, C]).
    
    Program Definition Monoid_Monad
            (m : @Monoid (Endofunctors C) Composition_Monoidal M) : 
      Monad := {|
      ret  := transform[mempty[m]];
      join := transform[mappend[m]]
    |}.
    
This makes no assumptions about the structure of the category `C`, other than
what has been stated above, and no other aspects of category theory are
needed. The proof, again,
is
[here](https://github.com/jwiegley/category-theory/blob/master/Isomorphism/Monoid/Monad.v#L22).

Note that there is another way to arrive at monads, from the adjunction of two
functors, which
I
[also have a proof for](https://github.com/jwiegley/category-theory/blob/master/Isomorphism/Adjunction/Monad.v#L23),
but this can wait until another post.

Footnotes: 
[1] We say small here to avoid the paradox of `Cat` not containing itself.
