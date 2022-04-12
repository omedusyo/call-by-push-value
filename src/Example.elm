module Example exposing (..)

import Calculus exposing (..)



-- id : A -> Returns[A]
-- id = freeze{
--   x <- pop;
--   return(x)
-- }


example0 =
    -- x <- pop;
    -- return(x)
    Pop { var = { name = "x", type_ = BoolType }, computation = Return (ValueNameUse "x") }



-- and : (Bool * Bool) -> Returns[Bool] comp
-- and =
--   pair <- pop;
--   match pair { (x * y) ->
--     match x {
--       true -> returns(y)
--     | false -> returns(false)
--     }
--   }


example1 =
    Pop
        { var = { name = "pair", type_ = TensorProduct BoolType BoolType }
        , computation =
            MatchTensorProduct (ValueNameUse "pair")
                { var0 = "x"
                , var1 = "y"
                , computation =
                    MatchBool (ValueNameUse "x")
                        { computation = Return (ValueNameUse "y") }
                        { computation = Return FalseConstant }
                }
        }



-- or : (Bool * Bool) -> Returns[Bool] comp
-- or =
--   (x, y) <- pop;
--   match x {
--     true -> returns(true)
--   | false -> returns(y)
--   }


example2 =
    Pop
        { var = { name = "pair", type_ = TensorProduct BoolType BoolType }
        , computation =
            MatchTensorProduct (ValueNameUse "pair")
                { var0 = "x"
                , var1 = "y"
                , computation =
                    MatchBool (ValueNameUse "x")
                        { computation = Return TrueConstant }
                        { computation = Return (ValueNameUse "y") }
                }
        }



-- not : Bool -> Returns[Bool] comp
-- not =
--   b <- bool
--   match b {
--     true -> returns(false)
--   | false -> returns(true)
--   }


example3 =
    Pop
        { var = { name = "b", type_ = BoolType }
        , computation =
            MatchBool (ValueNameUse "b")
                { computation = Return TrueConstant }
                { computation = Return FalseConstant }
        }



-- curry : Freeze[A * B -> T] -> (A -> (B -> T))
-- curry =
--   f <- pop;
--   a <- pop;
--   b <- pop;
--   push(a * b);
--   force(f)


example4 =
    Pop
        { var = { name = "f", type_ = Frozen (Arrow (TensorProduct (AtomicValueType "A") (AtomicValueType "B")) (AtomicComputationType "T")) }
        , computation =
            Pop
                { var = { name = "a", type_ = AtomicValueType "A" }
                , computation =
                    Pop
                        { var = { name = "b", type_ = AtomicValueType "B" }
                        , computation =
                            Push
                                (TensorProductPair (ValueNameUse "a") (ValueNameUse "b"))
                                (Force (ValueNameUse "f"))
                        }
                }
        }



-- uncurry : Freeze[A -> (B -> T)] -> ((A * B) -> T)
-- uncurry =
--   f <- pop;
--   (a, b) <- pop;
--   push(b); // the ordering is interesting, I don't think I can switch push(b) with push(a) here
--   push(a);
--   force(f)


example5 =
    Debug.todo ""



-- compose : Freeze[A -> Returns[B]] * Freeze[B -> T] -> A -> T
-- compose =
--   (f * g) <- pop;
--   a <- pop;
--   b <- {
--     push(a);
--     force(f)
--   };
--   push(b);
--   force(g)


example6 =
    Debug.todo ""



-- distr : (A * B + A * C) -> Returns[A * (B + C)]
-- distr =
--   u <- pop;
--   match u {
--     left(a * b) ->
--       return(a * left(b))
--     right(a * c) ->
--       return(a * left(c))
--   }


example7 =
    Debug.todo ""



-- distr : (A * (B + C)) -> Returns[A * B + A * C]
-- distr =
--   (a * u) <- pop
--   match u {
--     left(b) ->
--       return(a * b)
--     right(c) ->
--       return(a * c)
--   }


example8 =
    Debug.todo ""



-- f : Freeze[(S, T)] -> (S, T)
-- f =
--   u <- pop;
--   pair <- { force(u) }
--   pair


example9 =
    Debug.todo ""
