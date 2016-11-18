module Main exposing (..)

import BSTSet exposing (..)
import Html exposing (Html, div, span, form, input, text, beginnerProgram)
import Html.Attributes exposing (class, type_, value, style)
import Html.Events exposing (onSubmit, onClick, onInput)


type alias Model =
    { set : Set String
    , input : String
    }


init : Model
init =
    { set = empty
    , input = ""
    }


type Msg
    = SetInput String
    | Insert
    | Reset
    | Balance


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetInput x ->
            { model | input = x }

        Insert ->
            { model
                | input = ""
                , set = insert model.input model.set
            }

        Reset ->
            { model | set = empty }

        Balance ->
            { model | set = balance model.set }


setView : Set String -> Html Msg
setView set =
    case set of
        Empty ->
            div
                [ style
                    [ ( "background-color", "#CC00CC" )
                    , ( "width", "25px" )
                    , ( "height", "25px" )
                    , ( "border-radius", "15px" )
                    , ( "padding", "10px" )
                    , ( "margin", "10px" )
                    , ( "border", "1px solid black" )
                    ]
                ]
                []

        Tree balance head left right ->
            div
                [ style
                    [ ( "border", "1px solid black" )
                    , ( "border-radius", "15px" )
                    , ( "padding", "10px" )
                    , ( "margin", "10px" )
                    , ( "flex-basis", "0" )
                    , ( "flex-grow", size set |> toString )
                    ]
                ]
                [ span [ style [ ( "text-align", "center" ) ] ]
                    [ "Value: \"" ++ head ++ "\" Height Balance: " ++ (height right - height left |> toString) |> text ]
                , div
                    [ style [ ( "display", "flex" ) ] ]
                    [ setView left
                    , setView right
                    ]
                ]


buttonStyle : Html.Attribute Msg
buttonStyle =
    style
        [ ( "appearance", "none" )
        , ( "-webkit-appearance", "none" )
        , ( "-safari-appearance", "none" )
        , ( "background-color", "rgba(0,0,0,0)" )
        , ( "border", "1px solid #CCC" )
        , ( "padding", "10px" )
        , ( "margin", "10px" )
        ]


view : Model -> Html Msg
view model =
    div []
        [ form
            [ style [ ( "padding", "10px" ) ]
            , onSubmit Insert
            ]
            [ input
                [ style
                    [ ( "padding", "10px" )
                    , ( "margin", "10px" )
                    ]
                , type_ "text"
                , onInput SetInput
                , value model.input
                ]
                []
            , input
                [ buttonStyle
                , type_ "submit"
                , value "Insert as String"
                ]
                []
            , input
                [ buttonStyle
                , type_ "button"
                , value "Reset"
                , onClick Reset
                ]
                []
            , input
                [ buttonStyle
                , type_ "button"
                , value "Balance"
                , onClick Balance
                ]
                []
            ]
        , setView model.set
        ]


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init
        , update = update
        , view = view
        }
