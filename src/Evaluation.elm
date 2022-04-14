module Evaluation exposing (..)

import Calculus exposing (Computation, ComputationType, Value, ValueName, ValueNameIntro, ValueType)
import Dict exposing (Dict)


type Either a b
    = Left a
    | Right b


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
            (env |> getEnv valueName).value

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



-- TODO: I actually don't need to have the types for evaluation


type alias Env =
    Dict ValueName (List TypedTerminalValue)


emptyEnv : Env
emptyEnv =
    Dict.empty


insertEnv : ValueName -> TypedTerminalValue -> Env -> Env
insertEnv valueName tvalue env =
    env
        |> Dict.update valueName
            (\maybeValues ->
                case maybeValues of
                    Just values ->
                        Just (tvalue :: values)

                    Nothing ->
                        Just [ tvalue ]
            )


insertsEnv : List ( ValueName, TypedTerminalValue ) -> Env -> Env
insertsEnv bindings env =
    List.foldl (\( varName, val ) envState -> insertEnv varName val envState)
        env
        bindings


getEnv : ValueName -> Env -> TypedTerminalValue
getEnv valName env =
    case env |> Dict.get valName |> Maybe.andThen List.head of
        Just tvalue ->
            tvalue

        Nothing ->
            Debug.todo (String.concat [ "Lookup Error: unknown value name `", valName, "`" ])



-- ===Extraction===


extractTensorFromValue : TerminalValue -> ( TypedTerminalValue, TypedTerminalValue )
extractTensorFromValue value =
    case value of
        TerminalTensorProductPair value0 value1 ->
            ( typedTerminalValue value0 (typeCheckTerminalValue value0)
            , typedTerminalValue value1 (typeCheckTerminalValue value1)
            )

        _ ->
            Debug.todo "Extraction Error: you are trying to extract from a value of type `TensorProduct`"


extractTensor : TypedTerminalValue -> ( TypedTerminalValue, TypedTerminalValue )
extractTensor tvalue =
    case tvalue.type_ of
        Calculus.TensorProduct type0 type1 ->
            case tvalue.value of
                TerminalTensorProductPair value0 value1 ->
                    ( typedTerminalValue value0 type0, typedTerminalValue value1 type1 )

                _ ->
                    Debug.todo "Value Error: expected a tensor product of two values"

        _ ->
            Debug.todo "Type Error: you are trying to access a binding of `TensorProduct` type"


extractSumFromValue : TerminalValue -> Either TypedTerminalValue TypedTerminalValue
extractSumFromValue value =
    case value of
        TerminalLeft valueLeft _ ->
            Left (typedTerminalValue valueLeft (typeCheckTerminalValue valueLeft))

        TerminalRight _ valueRight ->
            Right (typedTerminalValue valueRight (typeCheckTerminalValue valueRight))

        _ ->
            Debug.todo "Extraction Error: you are trying to extract from a value of type `Sum`"


extractSum : TypedTerminalValue -> Either TypedTerminalValue TypedTerminalValue
extractSum tvalue =
    case tvalue.type_ of
        Calculus.ValueSum leftType rightType ->
            case tvalue.value of
                TerminalLeft leftValue _ ->
                    Left (typedTerminalValue leftValue leftType)

                TerminalRight _ rightValue ->
                    Right (typedTerminalValue rightValue rightType)

                _ ->
                    Debug.todo "Value Error: expected `left val` or `right val`"

        _ ->
            Debug.todo "Type Error: you are trying to access a binding of `Sum` type"


extractBoolFromValue : TerminalValue -> Env -> Either () ()
extractBoolFromValue value env =
    case value of
        TerminalTrueConstant ->
            Left ()

        TerminalFalseConstant ->
            Right ()

        _ ->
            Debug.todo "Extraction Error: you are trying to extract from a value of type `Sum`"


extractBool : TypedTerminalValue -> Either () ()
extractBool tvalue =
    case tvalue.value of
        TerminalTrueConstant ->
            Left ()

        TerminalFalseConstant ->
            Right ()

        _ ->
            Debug.todo "Value Error: expected a boolean value"


extractFreeze : TerminalValue -> ( Env, Computation )
extractFreeze value =
    case value of
        TerminalFreeze env computation ->
            ( env, computation )

        _ ->
            Debug.todo "Computation Error: expected a frozen computation"



-- ===Stacks===


type Stack
    = Nil
    | Push TerminalValue Stack
    | First Stack
    | Second Stack
    | Sequence Env { var : ValueName, computation : Computation } Stack


type alias State =
    { env : Env
    , stack : Stack
    , currentComputation : Computation
    }


type ComputationStep
    = Active State
    | TerminatedWith TerminalValue State
    | TerminalComputation Computation State



-- ===Type Checking===


typeCheckTerminalValue : TerminalValue -> ValueType
typeCheckTerminalValue value =
    case value of
        TerminalTensorProductPair value0 value1 ->
            Calculus.TensorProduct (typeCheckTerminalValue value0) (typeCheckTerminalValue value1)

        TerminalLeft valueLeft typeRight ->
            Calculus.ValueSum (typeCheckTerminalValue valueLeft) typeRight

        TerminalRight typeLeft valueRight ->
            Calculus.ValueSum typeLeft (typeCheckTerminalValue valueRight)

        TerminalFreeze env computation ->
            Calculus.Frozen (env |> typeCheckComputation computation)

        TerminalTrueConstant ->
            Calculus.BoolType

        TerminalFalseConstant ->
            Calculus.BoolType


typeCheckValue : Value -> Env -> ValueType
typeCheckValue value env =
    typeCheckTerminalValue (env |> valueToTerminalValue value)



-- TODO: Here I'm using full Environment which is not necessary. Replace this with Context later


typeCheckComputation : Computation -> Env -> ComputationType
typeCheckComputation computation env =
    case computation of
        Calculus.MatchTensorProduct value body ->
            let
                ( typedTerminalValue0, typedTerminalValue1 ) =
                    extractTensor (typedTerminalValue (env |> valueToTerminalValue value) (typeCheckTerminalValue (env |> valueToTerminalValue value)))
            in
            env
                |> insertEnv body.var0 typedTerminalValue0
                |> insertEnv body.var0 typedTerminalValue1
                |> typeCheckComputation body.computation

        _ ->
            Debug.todo ""



-- ===Small Step Evaluator===


do : Computation -> State -> State
do computation state =
    { state | currentComputation = computation }


setEnvironment : Env -> State -> State
setEnvironment env state =
    { state | env = env }


setStack : Stack -> State -> State
setStack stack state =
    { state | stack = stack }


step : State -> ComputationStep
step ({ currentComputation, env, stack } as state) =
    case currentComputation of
        Calculus.MatchTensorProduct value body ->
            let
                ( typedTerminalValue0, typedTerminalValue1 ) =
                    extractTensorFromValue (env |> valueToTerminalValue value)
            in
            Active
                (state
                    |> do body.computation
                    |> setEnvironment
                        (env
                            |> insertEnv body.var0 typedTerminalValue0
                            |> insertEnv body.var1 typedTerminalValue1
                        )
                )

        Calculus.MatchSum value bodyLeft bodyRight ->
            let
                terminalValue =
                    env |> valueToTerminalValue value
            in
            case extractSumFromValue terminalValue of
                Left typedTerminalValueLeft ->
                    Active
                        (state
                            |> do bodyLeft.computation
                            |> setEnvironment
                                (env
                                    |> insertEnv bodyLeft.var typedTerminalValueLeft
                                )
                        )

                Right typedTerminalValueRight ->
                    Active
                        (state
                            |> do bodyRight.computation
                            |> setEnvironment
                                (env
                                    |> insertEnv bodyRight.var typedTerminalValueRight
                                )
                        )

        Calculus.MatchZero body ->
            -- TODO
            Debug.todo ""

        Calculus.Force value ->
            let
                terminalValue =
                    env |> valueToTerminalValue value

                ( oldEnv, computation ) =
                    extractFreeze terminalValue
            in
            Active
                (state
                    |> do computation
                    |> setEnvironment oldEnv
                )

        Calculus.MatchBool value bodyLeft bodyRight ->
            let
                terminalValue =
                    env |> valueToTerminalValue value
            in
            case env |> extractBoolFromValue terminalValue of
                Left () ->
                    Active
                        (state
                            |> do bodyLeft.computation
                        )

                Right () ->
                    Active
                        (state
                            |> do bodyRight.computation
                        )

        Calculus.Pop body ->
            case stack of
                Push value oldStack ->
                    Active
                        (state
                            |> do body.computation
                            -- TODO: The environment thingy is shady here.
                            |> setEnvironment (env |> insertEnv body.var.name (typedTerminalValue value body.var.type_))
                            |> setStack oldStack
                        )

                _ ->
                    Debug.todo "Evaluation Error: You are trying to pop from a stack that wasn't pushed to"

        Calculus.CartesianProductPair computation0 computation1 ->
            -- This is fascinating... You don't just first evaluate `computation0` then `computation1`.
            -- The stack has to contain the information that tells you which branch of the pair to evaluate
            case stack of
                First oldStack ->
                    Active
                        (state
                            |> do computation0
                            |> setStack oldStack
                        )

                Second oldStack ->
                    Active
                        (state
                            |> do computation1
                            |> setStack oldStack
                        )

                _ ->
                    Debug.todo "Evaluation Error: You are trying to project from a stack that wasn't pushed to"

        Calculus.UnitComputation ->
            TerminalComputation Calculus.UnitComputation state

        Calculus.Return value ->
            let
                terminalValue =
                    env |> valueToTerminalValue value
            in
            case stack of
                Sequence oldEnv body oldStack ->
                    Active
                        (state
                            |> do body.computation
                            |> setEnvironment (oldEnv |> insertEnv body.var (typedTerminalValue terminalValue (typeCheckTerminalValue terminalValue)))
                            |> setStack oldStack
                        )

                Nil ->
                    TerminatedWith terminalValue state

                _ ->
                    Debug.todo "Evaluation Error: You are trying to return a value in a stack that wasn't sequenced"

        Calculus.Push value nextComputation ->
            let
                terminalValue =
                    env |> valueToTerminalValue value
            in
            Active
                (state
                    |> do nextComputation
                    |> setStack (Push terminalValue stack)
                )

        Calculus.First nextComputation ->
            Active
                (state
                    |> do nextComputation
                    |> setStack (First stack)
                )

        Calculus.Second nextComputation ->
            Active
                (state
                    |> do nextComputation
                    |> setStack (Second stack)
                )

        Calculus.Sequence computation0 body ->
            Active
                (state
                    |> do computation0
                    |> setStack (Sequence env body stack)
                )
