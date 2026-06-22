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

  (* roots, bitwise ops and byte serialization -- see below *)
  val isqrt : int -> int
  val sqrt  : int -> int
  val nthRoot : Int.int * int -> int
  val andb : int * int -> int   val orb  : int * int -> int
  val xorb : int * int -> int   val notb : int -> int
  val shl  : int * Int.int -> int   val shr : int * Int.int -> int
  val bit : int * Int.int -> bool   val testBit : int * Int.int -> bool
  val setBit : int * Int.int -> int   val clearBit : int * Int.int -> int
  val popcount : int -> Int.int   val bitLength : int -> Int.int
  val toBytes : int -> Word8Vector.vector
  val fromBytes : Word8Vector.vector -> int
end
```

### Roots, bit operations, and byte serialization

`isqrt n` is the floor of the square root (`Domain` for `n < 0`); `sqrt` is an
alias, and `nthRoot (k, n)` is the floor of the k-th root for `k >= 1` and
`n >= 0`. Both satisfy the usual flooring identity, e.g. `isqrt n` is the
greatest `r` with `r*r <= n`.

The bitwise operators `andb`, `orb`, `xorb` and `notb` treat each operand as an
**infinite two's-complement** bit string, so they agree with
`IntInf.andb`/`orb`/`xorb`/`notb` for every sign. Shifts are arithmetic:
`shl (n, k)` multiplies by `2^k` and `shr (n, k)` is a floored divide by `2^k`
(matching `IntInf.<<` and `IntInf.~>>`). `bit`/`testBit`, `setBit` and
`clearBit` address individual two's-complement bits (0-based from the LSB),
while `popcount` and `bitLength` report the set-bit count and bit-length of the
magnitude `|n|`.

`toBytes n` serializes the magnitude `|n|` to a **minimal big-endian, unsigned**
byte vector (no leading zero bytes; `0` becomes the empty vector), and
`fromBytes` reads such a vector back as a value `>= 0` -- convenient for crypto
interop. The round-trip `fromBytes (toBytes n) = n` holds for all `n >= 0`
(e.g. `256` <-> `[0wx01, 0wx00]`).

```sml
val () = print (BigInt.toString (BigInt.isqrt (valOf (BigInt.fromString "100000000000000000000"))) ^ "\n")
(* 10000000000 *)
val n  = BigInt.fromInt 256
val bs = BigInt.toBytes n                       (* #[0wx01, 0wx00] *)
val () = print (BigInt.toString (BigInt.fromBytes bs) ^ "\n")   (* 256 *)
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
  test.sml     IntInf cross-check + goldens (1138 checks)
  entry.sml / main.sml
tools/polybuild  Poly/ML build wrapper
```

## Tests

1138 deterministic checks: an `IntInf` cross-check of `add`/`sub`/`mul`/`~`/
`abs`/`compare`/`divMod`/`quotRem`/`gcd` over a seeded random stream (including
values well beyond `Int` range and negatives), `fromString`/`toString` round-
trips in base 10 and radix 16/2, factorial goldens (20!, 50!, 100!), RSA-style
`modpow` vectors, Miller-Rabin over known primes, composites, and Carmichael
numbers (561, 1105, 1729, ...), integer roots (`isqrt`/`nthRoot` against an
`IntInf` oracle and exact powers), two's-complement bit operations and
arithmetic shifts cross-checked against `IntInf`, and big-endian
`toBytes`/`fromBytes` round-trips. Run `make all-tests` to verify identical
output under both compilers.

## License

MIT. See [LICENSE](LICENSE).
