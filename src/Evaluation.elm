module Evaluation exposing (..)

import Calculus exposing (Computation, ComputationType, Value, ValueName, ValueNameIntro, ValueType)
import Context exposing (Context)
import Dict exposing (Dict)
import Environment as Env exposing (Env, TerminalValue(..), TypedTerminalValue)
import TypeExtraction


type Either a b
    = Left a
    | Right b



-- ===Extraction===


extractTensorFromTerminalValue : TerminalValue -> ( TypedTerminalValue, TypedTerminalValue )
extractTensorFromTerminalValue value =
    case value of
        TerminalTensorProductPair value0 value1 ->
            ( Env.typedTerminalValue value0 (TypeExtraction.terminalValueToType value0)
            , Env.typedTerminalValue value1 (TypeExtraction.terminalValueToType value1)
            )

        _ ->
            Debug.todo "Extraction Error: you are trying to extract from a value of type `TensorProduct`"


extractSumFromTerminalValue : TerminalValue -> Either TypedTerminalValue TypedTerminalValue
extractSumFromTerminalValue value =
    case value of
        TerminalLeft valueLeft _ ->
            Left (Env.typedTerminalValue valueLeft (TypeExtraction.terminalValueToType valueLeft))

        TerminalRight _ valueRight ->
            Right (Env.typedTerminalValue valueRight (TypeExtraction.terminalValueToType valueRight))

        _ ->
            Debug.todo "Extraction Error: you are trying to extract from a value of type `Sum`"


extractBoolFromTerminalValue : TerminalValue -> Either () ()
extractBoolFromTerminalValue value =
    case value of
        TerminalTrueConstant ->
            Left ()

        TerminalFalseConstant ->
            Right ()

        _ ->
            Debug.todo "Extraction Error: you are trying to extract from a value of type `Bool`"


extractFreezeFromTerminalValue : TerminalValue -> ( Env, Computation )
extractFreezeFromTerminalValue value =
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
    | Head Stack
    | Tail Stack
    | Stream Env { var : ValueName, headComputation : Computation, tailComputation : Computation } Stack


type alias State =
    { env : Env
    , stack : Stack
    , currentComputation : Computation
    }


type ComputationStep
    = Active State
    | TerminatedWith TerminalValue State
    | TerminalComputation Computation State



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
                    extractTensorFromTerminalValue (env |> Env.valueToTerminalValue value)
            in
            Active
                (state
                    |> do body.computation
                    |> setEnvironment
                        (env
                            |> Env.insert body.var0 typedTerminalValue0
                            |> Env.insert body.var1 typedTerminalValue1
                        )
                )

        Calculus.MatchSum value bodyLeft bodyRight ->
            let
                terminalValue =
                    env |> Env.valueToTerminalValue value
            in
            case extractSumFromTerminalValue terminalValue of
                Left typedTerminalValueLeft ->
                    Active
                        (state
                            |> do bodyLeft.computation
                            |> setEnvironment
                                (env
                                    |> Env.insert bodyLeft.var typedTerminalValueLeft
                                )
                        )

                Right typedTerminalValueRight ->
                    Active
                        (state
                            |> do bodyRight.computation
                            |> setEnvironment
                                (env
                                    |> Env.insert bodyRight.var typedTerminalValueRight
                                )
                        )

        Calculus.MatchZero body ->
            -- TODO
            Debug.todo ""

        Calculus.Force value ->
            let
                terminalValue =
                    env |> Env.valueToTerminalValue value

                ( oldEnv, computation ) =
                    extractFreezeFromTerminalValue terminalValue
            in
            Active
                (state
                    |> do computation
                    |> setEnvironment oldEnv
                )

        Calculus.MatchBool value bodyLeft bodyRight ->
            let
                terminalValue =
                    env |> Env.valueToTerminalValue value
            in
            case extractBoolFromTerminalValue terminalValue of
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
                            |> setEnvironment (env |> Env.insert body.var.name (Env.typedTerminalValue value body.var.type_))
                            |> setStack oldStack
                        )

                Nil ->
                    TerminalComputation currentComputation state

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

                Nil ->
                    TerminalComputation currentComputation state

                _ ->
                    Debug.todo "Evaluation Error: You are trying to project from a stack that wasn't pushed to"

        Calculus.UnitComputation ->
            TerminalComputation currentComputation state

        Calculus.StreamGenerator value body ->
            let
                terminalValue =
                    env |> Env.valueToTerminalValue value

                typedTerminalValue =
                    Env.typedTerminalValue terminalValue (TypeExtraction.terminalValueToType terminalValue)
            in
            case stack of
                Head oldStack ->
                    Active
                        (state
                            |> do body.headComputation
                            |> setEnvironment (env |> Env.insert body.var typedTerminalValue)
                            |> setStack oldStack
                        )

                Tail oldStack ->
                    Active
                        (state
                            |> do body.tailComputation
                            |> setEnvironment (env |> Env.insert body.var typedTerminalValue)
                            |> setStack (Stream env body oldStack)
                        )

                Nil ->
                    TerminalComputation currentComputation state

                _ ->
                    Debug.todo "Evaluation Error: You are trying to access a stream from a stack that wasn't pushed to"

        Calculus.Return value ->
            let
                terminalValue =
                    env |> Env.valueToTerminalValue value
            in
            case stack of
                Sequence oldEnv body oldStack ->
                    Active
                        (state
                            |> do body.computation
                            |> setEnvironment (oldEnv |> Env.insert body.var (Env.typedTerminalValue terminalValue (TypeExtraction.terminalValueToType terminalValue)))
                            |> setStack oldStack
                        )

                -- TODO: This is really weird
                --       We're in a return branch, and yet we're dealing with streams.
                --       Would be nice to have a definition of streams that doesn't depend on Return type
                Stream oldEnv body oldStack ->
                    Active
                        (state
                            |> do (Calculus.StreamGenerator value body)
                            |> setEnvironment oldEnv
                            |> setStack oldStack
                        )

                Nil ->
                    TerminatedWith terminalValue state

                _ ->
                    Debug.todo "Evaluation Error: You are trying to return a value in a stack that wasn't sequenced"

        Calculus.Push value nextComputation ->
            let
                terminalValue =
                    env |> Env.valueToTerminalValue value
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

        Calculus.Head nextComputation ->
            Active
                (state
                    |> do nextComputation
                    |> setStack (Head stack)
                )

        Calculus.Tail nextComputation ->
            Active
                (state
                    |> do nextComputation
                    |> setStack (Tail stack)
                )
