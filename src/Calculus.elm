module Calculus exposing (..)

import Dict exposing (Dict)



-- ===Types===


type alias TypeName =
    String


type ValueType
    = -- sum type
      ValueSum ValueType ValueType
    | ZeroType
      -- tensor type
    | TensorProduct ValueType ValueType
    | UnitValueType
    | -- forget : Computation -> Value
      Frozen ComputationType
      -- ===Specific types===
    | BoolType
    | AtomicValueType TypeName


type ComputationType
    = -- product type
      CartesianProduct ComputationType ComputationType
    | TerminalType
      -- arrow type
    | Arrow ValueType ComputationType
    | -- free : Value -> Computation
      Returns ValueType
      -- ===Specific types===
    | AtomicComputationType TypeName



-- ===Terms===


type alias ValueName =
    String


type alias ValueNameIntro =
    { name : ValueName
    , type_ : ValueType
    }


type Value
    = ValueNameUse ValueName
      -- ===value intro===
      -- tensor value type intro
    | TensorProductPair Value Value
      -- sum type intro
    | Left Value ValueType -- Left val B := left(val: A) : A + B
    | Right ValueType Value -- Right A val := right(val: B) : A + B
      -- freeze type intro
    | Freeze Computation
      -- ===Specific types===
    | TrueConstant
    | FalseConstant


type Computation
    = --===value elim===
      -- tensor value type elim
      MatchTensorProduct Value { var0 : ValueName, var1 : ValueName, computation : Computation }
      -- sum type elim
    | MatchSum Value { var : ValueName, computation : Computation } { var : ValueName, computation : Computation }
    | MatchZero { body : Computation }
      -- freeze type elim
    | Force Value
      -- ===Specific types===
    | MatchBool Value { computation : Computation } { computation : Computation }
      --===computation intro===
      -- function type intro
      -- Note that this can't be just ValueName, otherwise we wouldn't be able to easily infer the type
    | Pop { var : ValueNameIntro, computation : Computation }
      -- cartesian product intro
    | CartesianProductPair Computation Computation
    | UnitComputation
      -- return intro
    | Return Value
      --===computation elim===
      -- function type elim
    | Push Value Computation
      -- cartesian product elim
    | First Computation
    | Second Computation
      -- return elim
    | Sequence Computation { var : ValueName, computation : Computation }
