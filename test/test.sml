(* test.sml

   Deterministic test suite for sml-bigint.

   The arithmetic core is cross-checked against the Basis [IntInf] structure
   on a seeded pseudo-random stream of inputs (deterministic xorshift64, so
   MLton and Poly/ML exercise byte-for-byte identical cases).  Values are
   funnelled through [fromString]/[toString], so those round-trips are tested
   implicitly on every case in addition to the explicit checks below. *)

structure Tests =
struct

  structure B = BigInt

  (* ---- deterministic PRNG (xorshift64) ---- *)
  val seed = ref (0wx9E3779B97F4A7C15 : Word64.word)
  fun nextW () =
    let
      val x0 = !seed
      val x1 = Word64.xorb (x0, Word64.<< (x0, 0w13))
      val x2 = Word64.xorb (x1, Word64.>> (x1, 0w7))
      val x3 = Word64.xorb (x2, Word64.<< (x2, 0w17))
    in
      seed := x3; x3
    end

  val two64 = IntInf.<< (1, 0w64)

  fun randMag nwords =
    let
      fun loop (i, acc) =
        if i >= nwords then acc
        else loop (i + 1, acc * two64 + Word64.toLargeInt (nextW ()))
    in
      loop (0, 0)
    end

  (* A signed IntInf with 1..4 64-bit limbs, i.e. well beyond Int range. *)
  fun randVal () =
    let
      val nw = 1 + Word64.toInt (Word64.mod (nextW (), 0w4))
      val m = randMag nw
      val neg = Word64.andb (nextW (), 0w1) = 0w1
    in
      if neg then IntInf.~ m else m
    end

  fun s (i : IntInf.int) = IntInf.toString i
  fun toB (i : IntInf.int) = valOf (B.fromString (IntInf.toString i))
  fun fromS x = valOf (B.fromString x)
  fun ordStr LESS = "LESS" | ordStr EQUAL = "EQUAL" | ordStr GREATER = "GREATER"

  (* ---- cross-check against IntInf ---- *)
  fun crossCheck () =
    let
      val () = Harness.section "IntInf cross-check (add/sub/mul/neg/compare/div/gcd)"
      fun one k =
        let
          val a = randVal ()
          val b = randVal ()
          val ba = toB a
          val bb = toB b
          val si = Int.toString k
          val () = Harness.checkString ("add #" ^ si) (s (a + b), B.toString (B.add (ba, bb)))
          val () = Harness.checkString ("sub #" ^ si) (s (a - b), B.toString (B.sub (ba, bb)))
          val () = Harness.checkString ("mul #" ^ si) (s (a * b), B.toString (B.mul (ba, bb)))
          val () = Harness.checkString ("neg #" ^ si) (s (IntInf.~ a), B.toString (B.~ ba))
          val () = Harness.checkString ("abs #" ^ si) (s (IntInf.abs a), B.toString (B.abs ba))
          val () = Harness.checkString ("cmp #" ^ si)
                     (ordStr (IntInf.compare (a, b)), ordStr (B.compare (ba, bb)))
          val b' = if b = 0 then 1 else b
          val bb' = toB b'
          val () = Harness.checkString ("divMod q #" ^ si) (s (IntInf.div (a, b')), B.toString (#1 (B.divMod (ba, bb'))))
          val () = Harness.checkString ("divMod r #" ^ si) (s (IntInf.mod (a, b')), B.toString (#2 (B.divMod (ba, bb'))))
          val () = Harness.checkString ("quotRem q #" ^ si) (s (IntInf.quot (a, b')), B.toString (#1 (B.quotRem (ba, bb'))))
          val () = Harness.checkString ("quotRem r #" ^ si) (s (IntInf.rem (a, b')), B.toString (#2 (B.quotRem (ba, bb'))))
          val ag = IntInf.abs a
          val bg = IntInf.abs b
          fun gcdI (x, y) = if y = 0 then x else gcdI (y, x mod y)
          val () = Harness.checkString ("gcd #" ^ si) (s (gcdI (ag, bg)), B.toString (B.gcd (ba, bb)))
        in () end
      fun loop k = if k > 40 then () else (one k; loop (k + 1))
    in loop 1 end

  (* ---- conversions and string round-trips ---- *)
  fun conversions () =
    let
      val () = Harness.section "Conversions and string round-trips"
      val () = Harness.checkString "toString 0" ("0", B.toString (B.fromInt 0))
      val () = Harness.checkString "toString 12345" ("12345", B.toString (B.fromInt 12345))
      val () = Harness.checkString "toString ~9999" ("~9999", B.toString (B.fromInt ~9999))
      val () = Harness.checkBool   "toInt 42" (true, B.toInt (B.fromInt 42) = SOME 42)
      val () = Harness.checkBool   "toInt 0" (true, B.toInt (B.fromInt 0) = SOME 0)
      val () = Harness.checkBool   "toInt ~123456" (true, B.toInt (B.fromInt ~123456) = SOME ~123456)
      val () = Harness.checkBool   "fromString empty -> NONE" (true, not (isSome (B.fromString "")))
      val () = Harness.checkBool   "fromString 12a -> NONE" (true, not (isSome (B.fromString "12a")))
      val () = Harness.checkBool   "fromString '-' -> NONE" (true, not (isSome (B.fromString "-")))
      val () = Harness.checkBool   "fromString '~~1' -> NONE" (true, not (isSome (B.fromString "~~1")))
      val () = Harness.checkString "fromString +7" ("7", B.toString (fromS "+7"))
      val () = Harness.checkString "fromString ~7" ("~7", B.toString (fromS "~7"))
      val () = Harness.checkString "fromString -7" ("~7", B.toString (fromS "-7"))
      val () = Harness.checkString "fromString ~0 normalizes" ("0", B.toString (fromS "~0"))
      val () = Harness.checkString "fromString leading zeros" ("42", B.toString (fromS "00042"))
      val big = "123456789012345678901234567890123456789"
      val () = Harness.checkString "round-trip big base10" (big, B.toString (fromS big))
      val bigNeg = "~98765432109876543210987654321"
      val () = Harness.checkString "round-trip neg base10" (bigNeg, B.toString (fromS bigNeg))
    in () end

  (* ---- radix output ---- *)
  fun radix () =
    let
      val () = Harness.section "Radix output (16 and 2)"
      val () = Harness.checkString "hex 255" ("ff", B.toStringRadix (B.fromInt 16) (B.fromInt 255))
      val () = Harness.checkString "hex 0" ("0", B.toStringRadix (B.fromInt 16) (B.fromInt 0))
      val () = Harness.checkString "hex 4096" ("1000", B.toStringRadix (B.fromInt 16) (B.fromInt 4096))
      val () = Harness.checkString "hex neg 255" ("~ff", B.toStringRadix (B.fromInt 16) (B.fromInt ~255))
      val () = Harness.checkString "bin 10" ("1010", B.toStringRadix (B.fromInt 2) (B.fromInt 10))
      val () = Harness.checkString "bin neg 6" ("~110", B.toStringRadix (B.fromInt 2) (B.fromInt ~6))
      fun one k =
        let
          val v = randVal ()
          val hex = String.map Char.toLower (IntInf.fmt StringCvt.HEX v)
          val bin = IntInf.fmt StringCvt.BIN v
          val si = Int.toString k
        in
          Harness.checkString ("hex vs IntInf #" ^ si) (hex, B.toStringRadix (B.fromInt 16) (toB v));
          Harness.checkString ("bin vs IntInf #" ^ si) (bin, B.toStringRadix (B.fromInt 2) (toB v))
        end
      fun loop k = if k > 10 then () else (one k; loop (k + 1))
    in loop 1 end

  (* ---- pow / gcd ---- *)
  fun powGcd () =
    let
      val () = Harness.section "pow and gcd"
      val () = Harness.checkString "2^10" ("1024", B.toString (B.pow (B.fromInt 2, B.fromInt 10)))
      val () = Harness.checkString "10^20" ("100000000000000000000", B.toString (B.pow (B.fromInt 10, B.fromInt 20)))
      val () = Harness.checkString "3^0" ("1", B.toString (B.pow (B.fromInt 3, B.fromInt 0)))
      val () = Harness.checkString "(~2)^3" ("~8", B.toString (B.pow (B.fromInt ~2, B.fromInt 3)))
      val () = Harness.checkString "(~2)^4" ("16", B.toString (B.pow (B.fromInt ~2, B.fromInt 4)))
      val () = Harness.checkString "gcd(1071,462)" ("21", B.toString (B.gcd (B.fromInt 1071, B.fromInt 462)))
      val () = Harness.checkString "gcd(0,5)" ("5", B.toString (B.gcd (B.fromInt 0, B.fromInt 5)))
      val () = Harness.checkString "gcd(5,0)" ("5", B.toString (B.gcd (B.fromInt 5, B.fromInt 0)))
      val () = Harness.checkString "gcd(0,0)" ("0", B.toString (B.gcd (B.fromInt 0, B.fromInt 0)))
      val () = Harness.checkString "gcd(~12,18)" ("6", B.toString (B.gcd (B.fromInt ~12, B.fromInt 18)))
      val () = Harness.checkString "gcd(2^128-1, 2^96-1)"
                 ("4294967295",
                  B.toString (B.gcd (fromS "340282366920938463463374607431768211455",
                                     fromS "79228162514264337593543950335")))
    in () end

  (* ---- modpow ---- *)
  fun modpowTests () =
    let
      val () = Harness.section "modpow (RSA-style vectors)"
      fun mp (b, e, m) = B.toString (B.modpow (B.fromInt b, B.fromInt e, B.fromInt m))
      val () = Harness.checkString "4^13 mod 497" ("445", mp (4, 13, 497))
      val () = Harness.checkString "65^17 mod 3233" ("2790", mp (65, 17, 3233))
      val () = Harness.checkString "2790^413 mod 3233" ("65", mp (2790, 413, 3233))
      val () = Harness.checkString "2^1000 mod 1e9+7" ("688423210", mp (2, 1000, 1000000007))
      val () = Harness.checkString "3^1000000 mod 1e9+7" ("64935414", mp (3, 1000000, 1000000007))
      val () = Harness.checkString "modpow base reduced" ("445",
                 B.toString (B.modpow (B.fromInt 4, B.fromInt 13, B.fromInt 497)))
      (* (~4)^2 = 16; the base is reduced mod 497 first, giving the same 16 *)
      val () = Harness.checkString "modpow negative base" ("16",
                 B.toString (B.modpow (B.fromInt ~4, B.fromInt 2, B.fromInt 497)))
    in () end

  (* ---- factorial goldens ---- *)
  fun factorials () =
    let
      val () = Harness.section "factorial goldens"
      fun fact n =
        let fun go (i, acc) = if i > n then acc else go (i + 1, B.mul (acc, B.fromInt i))
        in go (1, B.fromInt 1) end
      val g20 = "2432902008176640000"
      val g50 = "30414093201713378043612608166064768844377641568960512000000000000"
      val g100 = "93326215443944152681699238856266700490715968264381621468592963895217"
               ^ "599993229915608941463976156518286253697920827223758251185210916864"
               ^ "000000000000000000000000"
      val () = Harness.checkString "20!" (g20, B.toString (fact 20))
      val () = Harness.checkString "50!" (g50, B.toString (fact 50))
      val () = Harness.checkString "100!" (g100, B.toString (fact 100))
    in () end

  (* ---- Miller-Rabin ---- *)
  fun primality () =
    let
      val () = Harness.section "Miller-Rabin primality"
      val rounds = B.fromInt 12
      val primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 97, 101, 7919, 104729, 1000003]
      val () = app (fn p =>
                 Harness.checkBool ("prime " ^ Int.toString p)
                   (true, B.isProbablePrime (B.fromInt p, rounds))) primes
      (* small composites and Carmichael numbers (which fool Fermat) *)
      val composites = [0, 1, 4, 6, 8, 9, 15, 21, 25, 100, 561, 1105, 1729, 2465, 2821, 6601, 1000000]
      val () = app (fn c =>
                 Harness.checkBool ("composite " ^ Int.toString c)
                   (false, B.isProbablePrime (B.fromInt c, rounds))) composites
      val () = Harness.checkBool "large prime 2^61-1"
                 (true, B.isProbablePrime (fromS "2305843009213693951", rounds))
      val () = Harness.checkBool "large composite 1000000007*1000000009"
                 (false, B.isProbablePrime (B.mul (fromS "1000000007", fromS "1000000009"), rounds))
    in () end

  (* ---- integer roots ---- *)
  fun isqrtOracle (n : IntInf.int) =
    if n < 0 then raise Domain
    else if n = 0 then 0
    else
      let fun loop x =
            let val y = IntInf.div (x + IntInf.div (n, x), 2)
            in if y < x then loop y else x end
      in loop n end

  fun roots () =
    let
      val () = Harness.section "integer roots (isqrt / nthRoot)"
      (* small n against the IntInf oracle *)
      fun smallSqrt n =
        Harness.checkString ("isqrt " ^ Int.toString n)
          (s (isqrtOracle (IntInf.fromInt n)), B.toString (B.isqrt (B.fromInt n)))
      val () = List.app smallSqrt [0,1,2,3,4,5,8,9,10,15,16,24,25,26,99,100,101,1000000]
      (* exact squares and one-below *)
      fun exact k =
        let
          val n = B.mul (B.fromInt k, B.fromInt k)
        in
          Harness.checkString ("isqrt " ^ Int.toString k ^ "^2") (Int.toString k, B.toString (B.isqrt n));
          if k > 0 then
            Harness.checkString ("isqrt " ^ Int.toString k ^ "^2-1")
              (Int.toString (k - 1), B.toString (B.isqrt (B.sub (n, B.fromInt 1))))
          else ()
        end
      val () = List.app exact [0,1,2,7,10,99,123,1000,46341]
      (* large isqrt against oracle on random magnitudes *)
      fun bigSqrt j =
        let val v = IntInf.abs (randVal ())
        in Harness.checkString ("isqrt big #" ^ Int.toString j)
             (s (isqrtOracle v), B.toString (B.isqrt (toB v))) end
      val () = let fun loop j = if j > 10 then () else (bigSqrt j; loop (j + 1)) in loop 1 end
      (* nthRoot: exact k-th powers and one-below *)
      fun nthExact (k, b) =
        let
          val n = B.pow (B.fromInt b, B.fromInt k)
        in
          Harness.checkString ("nthRoot(" ^ Int.toString k ^ ", " ^ Int.toString b ^ "^k)")
            (Int.toString b, B.toString (B.nthRoot (k, n)));
          Harness.checkString ("nthRoot(" ^ Int.toString k ^ ", " ^ Int.toString b ^ "^k-1)")
            (Int.toString (b - 1), B.toString (B.nthRoot (k, B.sub (n, B.fromInt 1))))
        end
      val () = List.app nthExact [(3,2),(3,10),(4,7),(5,3),(7,2),(3,1000),(2,99)]
      val () = Harness.checkString "nthRoot(1,n)=n" ("12345", B.toString (B.nthRoot (1, B.fromInt 12345)))
      val () = Harness.checkString "nthRoot(3,0)" ("0", B.toString (B.nthRoot (3, B.fromInt 0)))
      val () = Harness.checkRaises "isqrt(~1) raises" (fn () => B.isqrt (B.fromInt ~1))
      val () = Harness.checkRaises "nthRoot(0,n) raises" (fn () => B.nthRoot (0, B.fromInt 5))
      val () = Harness.checkRaises "nthRoot(2,~4) raises" (fn () => B.nthRoot (2, B.fromInt ~4))
    in () end

  (* ---- bitwise operations vs IntInf ---- *)
  fun bitwise () =
    let
      val () = Harness.section "bitwise ops vs IntInf (two's complement)"
      fun one k =
        let
          val a = randVal ()
          val b = randVal ()
          val ba = toB a and bb = toB b
          val si = Int.toString k
        in
          Harness.checkString ("andb #" ^ si) (s (IntInf.andb (a, b)), B.toString (B.andb (ba, bb)));
          Harness.checkString ("orb #"  ^ si) (s (IntInf.orb  (a, b)), B.toString (B.orb  (ba, bb)));
          Harness.checkString ("xorb #" ^ si) (s (IntInf.xorb (a, b)), B.toString (B.xorb (ba, bb)));
          Harness.checkString ("notb #" ^ si) (s (IntInf.notb a), B.toString (B.notb ba))
        end
      val () = let fun loop k = if k > 30 then () else (one k; loop (k + 1)) in loop 1 end
      (* fixed sign-mix vectors *)
      fun fixed (a, b) =
        let val nm = Int.toString a ^ "," ^ Int.toString b in
          Harness.checkString ("andb " ^ nm) (s (IntInf.andb (IntInf.fromInt a, IntInf.fromInt b)),
            B.toString (B.andb (B.fromInt a, B.fromInt b)));
          Harness.checkString ("orb " ^ nm) (s (IntInf.orb (IntInf.fromInt a, IntInf.fromInt b)),
            B.toString (B.orb (B.fromInt a, B.fromInt b)));
          Harness.checkString ("xorb " ^ nm) (s (IntInf.xorb (IntInf.fromInt a, IntInf.fromInt b)),
            B.toString (B.xorb (B.fromInt a, B.fromInt b)))
        end
      val () = List.app fixed [(12,10),(~12,10),(12,~10),(~12,~10),(0,~1),(255,256),(~1,~1)]
    in () end

  (* ---- shifts vs IntInf ---- *)
  fun shifts () =
    let
      val () = Harness.section "shifts vs IntInf (<< and ~>>)"
      val amts = [0,1,2,7,8,31,32,33,63,64]
      fun one k =
        let
          val a = randVal ()
          val ba = toB a
          val si = Int.toString k
          fun perAmt amt =
            let val w = Word.fromInt amt in
              Harness.checkString ("shl #" ^ si ^ " by " ^ Int.toString amt)
                (s (IntInf.<< (a, w)), B.toString (B.shl (ba, amt)));
              Harness.checkString ("shr #" ^ si ^ " by " ^ Int.toString amt)
                (s (IntInf.~>> (a, w)), B.toString (B.shr (ba, amt)))
            end
        in List.app perAmt amts end
      val () = let fun loop k = if k > 8 then () else (one k; loop (k + 1)) in loop 1 end
      val () = Harness.checkRaises "shl negative amount raises" (fn () => B.shl (B.fromInt 1, ~1))
      val () = Harness.checkRaises "shr negative amount raises" (fn () => B.shr (B.fromInt 1, ~1))
    in () end

  (* ---- single-bit ops, popcount, bitLength ---- *)
  fun bitProbes () =
    let
      val () = Harness.section "bit / setBit / clearBit / popcount / bitLength"
      fun bitOracle (a, i) = IntInf.andb (IntInf.~>> (a, Word.fromInt i), 1) = 1
      fun popOracle a =
        let fun loop (x, acc) = if x = 0 then acc
              else loop (IntInf.~>> (x, 0w1), acc + (if IntInf.andb (x, 1) = 1 then 1 else 0))
        in loop (IntInf.abs a, 0) end
      fun blOracle a =
        let fun loop (x, acc) = if x = 0 then acc else loop (IntInf.~>> (x, 0w1), acc + 1)
        in loop (IntInf.abs a, 0) end
      fun one k =
        let
          val a = IntInf.abs (randVal ())
          val ba = toB a
          val si = Int.toString k
          val () = List.app (fn i =>
                     Harness.checkBool ("bit #" ^ si ^ " @" ^ Int.toString i)
                       (bitOracle (a, i), B.bit (ba, i))) [0,1,5,30,31,32,63,64,100]
          val () = Harness.checkInt ("popcount #" ^ si) (popOracle a, B.popcount ba)
          val () = Harness.checkInt ("bitLength #" ^ si) (blOracle a, B.bitLength ba)
          (* setBit/clearBit against IntInf masks *)
          val i = 1 + Word64.toInt (Word64.mod (nextW (), 0w80))
          val mask = IntInf.<< (1, Word.fromInt i)
          val () = Harness.checkString ("setBit #" ^ si) (s (IntInf.orb (a, mask)), B.toString (B.setBit (ba, i)))
          val () = Harness.checkString ("clearBit #" ^ si)
                     (s (IntInf.andb (a, IntInf.notb mask)), B.toString (B.clearBit (ba, i)))
        in () end
      val () = let fun loop k = if k > 15 then () else (one k; loop (k + 1)) in loop 1 end
      val () = Harness.checkInt "bitLength 0" (0, B.bitLength (B.fromInt 0))
      val () = Harness.checkInt "popcount 0" (0, B.popcount (B.fromInt 0))
      val () = Harness.checkInt "bitLength 255" (8, B.bitLength (B.fromInt 255))
      val () = Harness.checkInt "popcount 255" (8, B.popcount (B.fromInt 255))
      val () = Harness.checkBool "testBit 6@1" (true, B.testBit (B.fromInt 6, 1))
      val () = Harness.checkBool "testBit 6@0" (false, B.testBit (B.fromInt 6, 0))
    in () end

  (* ---- byte serialization ---- *)
  fun bytes () =
    let
      val () = Harness.section "toBytes / fromBytes (big-endian, unsigned)"
      fun toInts v = List.tabulate (Word8Vector.length v, fn i => Word8.toInt (Word8Vector.sub (v, i)))
      fun ofInts xs = Word8Vector.fromList (List.map Word8.fromInt xs)
      (* known vectors *)
      val () = Harness.checkIntList "toBytes 0" ([], toInts (B.toBytes (B.fromInt 0)))
      val () = Harness.checkIntList "toBytes 1" ([1], toInts (B.toBytes (B.fromInt 1)))
      val () = Harness.checkIntList "toBytes 255" ([255], toInts (B.toBytes (B.fromInt 255)))
      val () = Harness.checkIntList "toBytes 256" ([1,0], toInts (B.toBytes (B.fromInt 256)))
      val () = Harness.checkIntList "toBytes 65535" ([255,255], toInts (B.toBytes (B.fromInt 65535)))
      val () = Harness.checkIntList "toBytes 16777216" ([1,0,0,0], toInts (B.toBytes (B.fromInt 16777216)))
      val () = Harness.checkIntList "toBytes |~256|" ([1,0], toInts (B.toBytes (B.fromInt ~256)))
      (* fromBytes known *)
      val () = Harness.checkString "fromBytes []" ("0", B.toString (B.fromBytes (ofInts [])))
      val () = Harness.checkString "fromBytes [1,0]" ("256", B.toString (B.fromBytes (ofInts [1,0])))
      val () = Harness.checkString "fromBytes leading zeros" ("1", B.toString (B.fromBytes (ofInts [0,0,1])))
      val () = Harness.checkString "fromBytes [255,255]" ("65535", B.toString (B.fromBytes (ofInts [255,255])))
      (* round-trips for n >= 0 *)
      fun one k =
        let
          val a = IntInf.abs (randVal ())
          val ba = toB a
          val si = Int.toString k
        in
          Harness.checkString ("roundtrip #" ^ si) (s a, B.toString (B.fromBytes (B.toBytes ba)))
        end
      val () = let fun loop k = if k > 20 then () else (one k; loop (k + 1)) in loop 1 end
    in () end

  fun runAll () =
    ( conversions ()
    ; crossCheck ()
    ; radix ()
    ; powGcd ()
    ; modpowTests ()
    ; factorials ()
    ; primality ()
    ; roots ()
    ; bitwise ()
    ; shifts ()
    ; bitProbes ()
    ; bytes () )

  fun run () = (Harness.reset (); runAll (); Harness.run ())
end
