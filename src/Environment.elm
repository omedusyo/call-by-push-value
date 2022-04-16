module Environment exposing (..)

import Calculus exposing (Computation, ComputationType, Value, ValueName, ValueType)
import Context exposing (Context)
import Dict exposing (Dict)


type
    TerminalValue
    --Note that
    --   1. There is no var use
    --   2. The Frozen Values capture the environment
    = -- tensor value type intro
      TerminalTensorProductPair TerminalValue TerminalValue
      -- sum type intro
    | TerminalLeft TerminalValue ValueType -- Left val B := left(val: A) : A + B
    | TerminalRight ValueType TerminalValue -- Right A val := right(val: B) : A + B
      -- freeze type intro
    | TerminalFreeze Env Computation
      -- ===Specific types===
    | TerminalTrueConstant
    | TerminalFalseConstant


valueToTerminalValue : Value -> Env -> TerminalValue
valueToTerminalValue value env =
    case value of
        Calculus.ValueNameUse valueName ->
            (env |> get valueName).value

        Calculus.TensorProductPair value0 value1 ->
            TerminalTensorProductPair (env |> valueToTerminalValue value0) (env |> valueToTerminalValue value1)

        Calculus.Left valueLeft typeRight ->
            TerminalLeft (env |> valueToTerminalValue valueLeft) typeRight

        Calculus.Right typeLeft valueRight ->
            TerminalRight typeLeft (env |> valueToTerminalValue valueRight)

        Calculus.Freeze computation ->
            TerminalFreeze env computation

        Calculus.TrueConstant ->
            TerminalTrueConstant

        Calculus.FalseConstant ->
            TerminalFalseConstant



-- ===Environments===


type alias TypedTerminalValue =
    { value : TerminalValue
    , type_ : ValueType
    }


type alias TypedComputation =
    { computation : Computation
    , type_ : ComputationType
    }


typedTerminalValue : TerminalValue -> ValueType -> TypedTerminalValue
typedTerminalValue value type_ =
    { value = value, type_ = type_ }


typedComputation : Computation -> ComputationType -> TypedComputation
typedComputation computation type_ =
    { computation = computation, type_ = type_ }



-- I actually don't need to have the types for evaluation, but it will be easier to have the types when rendering


type alias Env =
    Dict ValueName TypedTerminalValue


empty : Env
empty =
    Dict.empty


insert : ValueName -> TypedTerminalValue -> Env -> Env
insert valueName tvalue env =
    env
        |> Dict.insert valueName tvalue


get : ValueName -> Env -> TypedTerminalValue
get valName env =
    case env |> Dict.get valName of
        Just tvalue ->
            tvalue

        Nothing ->
            Debug.todo (String.concat [ "Lookup Error: unknown value name `", valName, "`" ])


toContext : Env -> Context
toContext env =
    env |> Dict.map (\_ tval -> tval.type_)
