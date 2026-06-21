(* demo.sml

   A small command-line demo for sml-bigint: prints 100!, a few derived
   quantities, and a textbook RSA encrypt/decrypt round-trip done entirely
   with modular exponentiation over big integers. *)

structure B = BigInt

fun line s = print (s ^ "\n")

fun fact n =
  let fun go (i, acc) = if i > n then acc else go (i + 1, B.mul (acc, B.fromInt i))
  in go (1, B.fromInt 1) end

val () = line "sml-bigint demo"
val () = line "==============="
val () = line ""

val f100 = fact 100
val () = line "100! ="
val () = line ("  " ^ B.toString f100)
val () = line ("  (" ^ Int.toString (String.size (B.toString f100)) ^ " decimal digits)")
val () = line ("  hex: " ^ B.toStringRadix (B.fromInt 16) f100)
val () = line ""

(* A power of two, in three radices. *)
val p = B.pow (B.fromInt 2, B.fromInt 128)
val () = line "2^128 ="
val () = line ("  dec: " ^ B.toString p)
val () = line ("  hex: " ^ B.toStringRadix (B.fromInt 16) p)
val () = line ""

(* Textbook RSA round-trip with the classic p=61, q=53 key.
     n = 3233, e = 17, d = 413, message m = 65.            *)
val n = B.fromInt 3233
val e = B.fromInt 17
val d = B.fromInt 413
val m = B.fromInt 65
val c = B.modpow (m, e, n)            (* encrypt: c = m^e mod n *)
val m' = B.modpow (c, d, n)           (* decrypt: m = c^d mod n *)
val () = line "RSA round-trip (n=3233, e=17, d=413):"
val () = line ("  message   m = " ^ B.toString m)
val () = line ("  cipher    c = m^e mod n = " ^ B.toString c)
val () = line ("  recovered     c^d mod n = " ^ B.toString m')
val () = line ("  round-trip ok: " ^ Bool.toString (B.compare (m, m') = EQUAL))
val () = line ""

(* A larger modular exponentiation and a primality check. *)
val () = line ("4^13 mod 497 = " ^ B.toString (B.modpow (B.fromInt 4, B.fromInt 13, B.fromInt 497)))
val mersenne = valOf (B.fromString "2305843009213693951")  (* 2^61 - 1 *)
val () = line ("isProbablePrime(2^61 - 1, 12) = "
               ^ Bool.toString (B.isProbablePrime (mersenne, B.fromInt 12)))
val () = line ("isProbablePrime(561, 12)      = "
               ^ Bool.toString (B.isProbablePrime (B.fromInt 561, B.fromInt 12)))
