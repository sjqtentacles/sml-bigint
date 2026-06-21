# sml-bigint

[![CI](https://github.com/sjqtentacles/sml-bigint/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-bigint/actions/workflows/ci.yml)

Arbitrary-precision signed integers for Standard ML: pure, deterministic, and
dependency-free.

`sml-bigint` is the number-theory workhorse of the collection -- the piece you
reach for when machine `int` runs out of bits: factorials, combinatorics, RSA-
style modular exponentiation, GCDs, and probabilistic primality testing.
Everything is pure Standard ML over the Basis library, with no FFI, threads,
clock, or RNG, so a given input always produces the same output.

The arithmetic core is verified against the Basis `IntInf` structure on a
seeded pseudo-random stream of inputs, on **MLton** and **Poly/ML**; the suite
produces identical results across both compilers.

## How it works

| Concern | Approach |
| --- | --- |
| Representation | sign-magnitude over little-endian base-2^32 `Word32` limbs |
| Intermediate products | `Word64` (so `a*b + c + d` never overflows for 32-bit terms) |
| Multiplication | Karatsuba above a limb cutoff, schoolbook below |
| Division | binary long division (floored `divMod`, truncated `quotRem`) |
| GCD | binary (Stein) algorithm |
| Modular exponentiation | square-and-multiply with reduction each step |
| Primality | deterministic Miller-Rabin over fixed small witness bases |

String output follows the Basis convention of a leading `~` for negatives, so
`toString` agrees character-for-character with `IntInf.toString`.

## API

```sml
structure BigInt : sig
  type int

  val fromInt    : Int.int -> int
  val toInt      : int -> Int.int option       (* NONE when out of Int range *)
  val fromString : string -> int option        (* base 10, optional ~ / - / + *)
  val toString   : int -> string                (* base 10, ~-prefixed *)
  val toStringRadix : int -> int -> string      (* radix (2..36) -> n -> string *)

  val ~   : int -> int
  val +   : int * int -> int
  val -   : int * int -> int
  val *   : int * int -> int
  val add : int * int -> int                    (* prefix spellings *)
  val sub : int * int -> int
  val mul : int * int -> int

  val divMod  : int * int -> int * int          (* floored;   like IntInf.divMod  *)
  val quotRem : int * int -> int * int          (* truncated; like IntInf.quotRem *)
  val compare : int * int -> order
  val sign : int -> int
  val abs  : int -> int

  val pow    : int * int -> int                 (* Domain on negative exponent *)
  val gcd    : int * int -> int                 (* always non-negative *)
  val modpow : int * int * int -> int           (* (b^e) mod m, e >= 0, m > 0 *)
  val isProbablePrime : int * int -> bool        (* (n, rounds): Miller-Rabin *)
end
```

### Example

```sml
val f100 = (* 100! *)
  let fun go (i, acc) = if i > 100 then acc else go (i + 1, BigInt.mul (acc, BigInt.fromInt i))
  in go (1, BigInt.fromInt 1) end
val () = print (BigInt.toString f100 ^ "\n")
(* 9332621544394415268169...000000000000000000000000  (158 digits) *)

(* textbook RSA with n=3233, e=17, d=413 *)
val c = BigInt.modpow (BigInt.fromInt 65, BigInt.fromInt 17, BigInt.fromInt 3233)   (* 2790 *)
val m = BigInt.modpow (c, BigInt.fromInt 413, BigInt.fromInt 3233)                  (* 65   *)

val () = print (Bool.toString (BigInt.isProbablePrime (valOf (BigInt.fromString "2305843009213693951"), BigInt.fromInt 12)) ^ "\n")
(* true: 2^61 - 1 is prime *)
```

`examples/demo.sml` runs all of the above; `make example` builds and runs it.

## Build & test

Requires [MLton](http://mlton.org/) and/or [Poly/ML](https://polyml.org/).

```sh
make test        # build + run the suite under MLton
make test-poly   # run the suite under Poly/ML
make all-tests   # both
make example     # build + run examples/demo.sml (100! and an RSA round-trip)
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-bigint
smlpkg sync
```

Reference `lib/github.com/sjqtentacles/sml-bigint/bigint.mlb` from your own
`.mlb` (MLton / MLKit), or feed `sources.mlb` to `tools/polybuild` (Poly/ML).

## Layout

```
sml.pkg                                       smlpkg manifest
Makefile                                      MLton + Poly/ML targets
.github/workflows/ci.yml                      CI: MLton + Poly/ML
lib/github.com/sjqtentacles/sml-bigint/
  bigint.sig/.sml  arbitrary-precision signed integers
  sources.mlb      ordered source list
  bigint.mlb       public basis
examples/
  demo.sml         100!, 2^128, RSA round-trip, primality
  sources.mlb
test/
  harness.sml  shared assertion harness
  test.sml     IntInf cross-check + goldens (539 checks)
  entry.sml / main.sml
tools/polybuild  Poly/ML build wrapper
```

## Tests

539 deterministic checks: an `IntInf` cross-check of `add`/`sub`/`mul`/`~`/
`abs`/`compare`/`divMod`/`quotRem`/`gcd` over a seeded random stream (including
values well beyond `Int` range and negatives), `fromString`/`toString` round-
trips in base 10 and radix 16/2, factorial goldens (20!, 50!, 100!), RSA-style
`modpow` vectors, and Miller-Rabin over known primes, composites, and
Carmichael numbers (561, 1105, 1729, ...). Run `make all-tests` to verify
identical output under both compilers.

## License

MIT. See [LICENSE](LICENSE).
