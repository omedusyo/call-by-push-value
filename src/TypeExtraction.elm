module TypeExtraction exposing (..)

import Calculus exposing (Computation, ComputationType, Value, ValueName, ValueNameIntro, ValueType)
import Context exposing (Context)
import Environment as Env exposing (Env, TerminalValue(..), TypedTerminalValue)



-- ===Type Extraction===


terminalValueToType : TerminalValue -> ValueType
terminalValueToType value =
    case value of
        TerminalTensorProductPair value0 value1 ->
            Calculus.TensorProduct (terminalValueToType value0) (terminalValueToType value1)

        TerminalLeft valueLeft typeRight ->
            Calculus.ValueSum (terminalValueToType valueLeft) typeRight

        TerminalRight typeLeft valueRight ->
            Calculus.ValueSum typeLeft (terminalValueToType valueRight)

        TerminalFreeze env computation ->
            Calculus.Frozen (Env.toContext env |> computationToType computation)

        TerminalTrueConstant ->
            Calculus.BoolType

        TerminalFalseConstant ->
            Calculus.BoolType


valueToType : Value -> Context -> ValueType
valueToType value ctx =
    case value of
        Calculus.ValueNameUse name ->
            ctx |> Context.get name

        Calculus.TensorProductPair value0 value1 ->
            Calculus.TensorProduct (ctx |> valueToType value0) (ctx |> valueToType value1)

        Calculus.Left valueLeft typeRight ->
            Calculus.ValueSum (ctx |> valueToType valueLeft) typeRight

        Calculus.Right typeLeft valueRight ->
            Calculus.ValueSum typeLeft (ctx |> valueToType valueRight)

        Calculus.Freeze computation ->
            Calculus.Frozen (ctx |> computationToType computation)

        Calculus.TrueConstant ->
            Calculus.BoolType

        Calculus.FalseConstant ->
            Calculus.BoolType


computationToType : Computation -> Context -> ComputationType
computationToType computation ctx =
    case computation of
        Calculus.MatchTensorProduct value body ->
            case ctx |> valueToType value of
                Calculus.TensorProduct type0 type1 ->
                    ctx
                        |> Context.insert body.var0 type0
                        |> Context.insert body.var1 type1
                        |> computationToType body.computation

                _ ->
                    Debug.todo "Type Extraction Error: Expected a value of tensor product type"

        Calculus.MatchSum value leftBody _ ->
            case ctx |> valueToType value of
                Calculus.ValueSum type0 type1 ->
                    -- TODO: Arbitrarily choose the first branch
                    ctx
                        |> Context.insert leftBody.var type0
                        |> computationToType leftBody.computation

                _ ->
                    Debug.todo "Type Extraction Error: Expected a value of sum type"

        Calculus.MatchZero body ->
            -- TODO
            Debug.todo ""

        Calculus.Force value ->
            case ctx |> valueToType value of
                Calculus.Frozen type_ ->
                    type_

                _ ->
                    Debug.todo "Type Extraction Error: Expected a value of frozen type"

        Calculus.MatchBool _ leftBody _ ->
            -- TODO: Don't even check that the value is of type bool
            -- TODO: Arbitrarily choose the first branch
            ctx
                |> computationToType leftBody.computation

        Calculus.Pop body ->
            ctx
                |> Context.insert body.var.name body.var.type_
                |> computationToType body.computation

        _ ->
            Debug.todo ""
