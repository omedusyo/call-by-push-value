module Main exposing (..)

import Browser
import Calculus
import Evaluation
import Example
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Lib.Html as H
import Return exposing (Return)



-- ===MODEL===


type alias Model =
    {}


initModel : Model
initModel =
    {}



-- 3 * (4 + 5)


type Msg
    = Step


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        Step ->
            model
                |> Return.singleton



-- ===VIEW===


view : Model -> Html Msg
view model =
    H.column []
        [ H.div []
            [ H.text "hello, world" ]
        , H.button [ HE.onClick Step, HA.style "width" "60px" ] [ H.text "step" ]
        ]



-- ===MAIN===


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> Return.singleton initModel
        , update = update
        , subscriptions = subscriptions
        , view = \model -> view model |> H.toUnstyled
        }



-- ===SUBSCRIPTIONS===


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
