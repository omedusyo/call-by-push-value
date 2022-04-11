module Lib.Html exposing (..)

import Css
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA


row attrs html =
    H.div ([ HA.style "display" "flex", HA.style "flex-direction" "row" ] ++ attrs)
        html


alignedRow attrs html =
    H.div ([ HA.style "display" "flex", HA.style "flex-direction" "row", HA.style "align-items" "center" ] ++ attrs)
        html


column attrs html =
    H.div ([ HA.style "display" "flex", HA.style "flex-direction" "column" ] ++ attrs)
        html


gapX w =
    H.div [ HA.css [ Css.marginLeft (Css.px w) ] ] []


gapY h =
    H.div [ HA.css [ Css.marginTop (Css.px h) ] ] []
