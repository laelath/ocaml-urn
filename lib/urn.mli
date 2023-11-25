(** Urns for weighted random sampling.

    This module implements the urn data structure given a weight type that
    is a totally ordered group with a random sampling function.
  *)

module type WeightType =
  sig
    type t
    (** The type of weights. *)

    val compare : t -> t -> int
    (** A total ordering function over weights.
        [compare x y] should return zero if the weights [x] and [y] are equal,
        [compare x y] should be negative if [x] is less than [y], and
        [compare x y] should be positive if [x] is greater than [y]. *)

    val zero : t
    (** Weight zero value. It should be the case that forall [x],
        [add x zero = x]. *)

    val add : t -> t -> t
    (** Weight addition. Should be commutative, associative, etc. *)

    val sub : t -> t -> t
    (** Weight subtraction. It should be the inverse of [add], i.e.
        forall [x] and [y], [sub (add x y) y] equals [x]. *)

    val sample : t -> t
    (** Weight sampling function. [sample w] should produce a value uniformly
        chosen between [zero] (inclusive) and [w] (exclusive).
        Example: [Random.int] is a suitable sampling function for integer
        weights. *)
  end
(** Input signature of the functor {!Make}. *)

module type U =
  sig

    type weight
    (** The type of weights. *)

    type !+'a t
    (** The type of urns. *)

    (** {1:creation Creating Urns} *)

    val singleton : weight -> 'a -> 'a t
    (** [singleton w x] returns the one-element urn containing [x] with
        weight [w]. Time complexity O(1).
        
        @raise Invalid_argument if [w <= 0]. *)

    val of_list : (weight * 'a) list -> 'a t option
    (** [of_list was] creates an urn from a list of pairs of weights and
        values. Time complexity O(n).

        @raise Invalid_argument if any of the weights are [<= 0]. *)

    (** {1:adding Adding Elements to Urns} *)

    val add : weight -> 'a -> 'a t -> 'a t
    (** [add w a ur] returns an urn containing the same weights and elements
        as [ur] but additionally containing [a] with weight [w].
        Time complexity O(log n).

        @raise Invalid_argument if [w <= 0] *)

    val add_seq : (weight * 'a) Seq.t -> 'a t -> 'a t
    (** Add the weight-value pairs in the sequence to the urn.
        Time complexity O(m log n), where m is the length of the sequence.

        @raise Invalid_argument if any of the weights are [<= 0]. *)

    val add_list : (weight * 'a) list -> 'a t -> 'a t
    (** Add the weight-value pairs in the list to the urn.
        Time complexity O(m log n), where m is the length of the list.

        @raise Invalid_argument if any of the weights are [<= 0]. *)

    (** {1:sampling Sampling Urns} *)

    val sample : 'a t -> 'a
    (** [sample ur] samples an element of the urn [ur] and returns it.
        Time complexity O(log n). *)

    val remove : 'a t -> (weight * 'a) * 'a t option
    (** [remove ur] samples an element of the urn [ur] and returns it along
        with a new urn with that element removed, or [None] if the new urn
        would be empty. Time complexity O(log n). *)

    val replace : weight -> 'a -> 'a t -> (weight * 'a) * 'a t
    (** [replace w a ur] samples the urn [ur], and returns the sampled element
        and its weight along with a new urn with the sampled elements removed
        and [a] with weight [w] added. Time complexity O(log n).
        
        @raise Invalid_argument if [w <= 0]. *)

    val update :
      (weight -> 'a -> weight * 'a) -> 'a t ->
      (weight * 'a) * (weight * 'a) * 'a t
    (** [update f ur] samples an element of the urn [ur], then takes the
        chosen element [a] and its weight [w], and replaces it with [f w a],
        returning a triple of [(w, a), f w a, ur'] where [ur'] is the urn
        containing all the elements and weights of [ur] but with the chosen
        [w, a] replaced by [f w a]. Time complexity O(log n).

        @raise Invalid_argument if the weight produced by [f w a] is [<= 0]. *)

    val update_opt :
      (weight -> 'a -> (weight * 'a) option) -> 'a t ->
      (weight * 'a) * (weight * 'a) option * 'a t option
    (** [update f ur] samples an element of the urn [ur], then takes the
        chosen element [a] and its weight [w], and applies [f] to them.
        If [f w a] returns [None] then the element is removed from the urn.
        If [f w a] returns [Some (w', a')] then the chosen elements weight
        and value is replaced by [w'] and [a'] respectively. The elements of
        the returned triple are as in [update]. Time complexity O(log n).

        @raise Invalid_argument if [w' <= 0]. *)

    (** {1:misc Misc} *)

    val size : 'a t -> int
    (** [size ur] returns the total number of elements in the urn [ur].
        Time complexity O(1). *)

    val weight : 'a t -> weight
    (** [weight ur] returns the sum of all the weights in the urn [ur].
        Time complexity O(1). *)

  end
(** Output signature of the functor {!Make}. *)

module Make(Weight : WeightType) : U with type weight = Weight.t
(** Functor building an implementation of the urn structure
   given a weight type. *)

module IntWeight : WeightType with type t = int
(** A module for [int] weights using [Random.int] to sample
    random integers. *)

module FloatWeight : WeightType with type t = float
(** A module for [float] weights using [Random.float] to sample
    random floating point numbers. *)

