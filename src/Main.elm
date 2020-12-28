module Main exposing (main)

import Browser
import Html exposing (a, button, div, footer, input, label, li, section, span, strong, text, ul)
import Html.Attributes exposing (attribute, class, for, href, id, type_, value)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- MODEL


type alias Model =
    List String


init : Model
init =
    []



-- VIEW


view : Model -> Html.Html msg
view model =
    if List.isEmpty model then
        text ""

    else
        div []
            [ section
                [ class "main" ]
                [ input [ class "toggle-all", id "toggle-all", type_ "checkbox" ]
                    []
                , label [ for "toggle-all" ]
                    [ text "Mark all as complete" ]
                , ul [ class "todo-list" ]
                    (List.map
                        todo
                        model
                    )
                ]
            , footerHtml
            ]


todo : String -> Html.Html msg
todo todoText =
    li [ class "completed" ]
        [ div [ class "view" ]
            [ input [ attribute "checked" "", class "toggle", type_ "checkbox" ]
                []
            , label []
                [ text todoText ]
            , button [ class "destroy" ]
                []
            ]
        , input [ class "edit", value "Create a TodoMVC template" ]
            []
        ]


footerHtml =
    footer [ class "footer" ]
        [ span [ class "todo-count" ]
            [ strong []
                [ text "0" ]
            , text "item left"
            ]
        , ul [ class "filters" ]
            [ li []
                [ a [ class "selected", href "#/" ]
                    [ text "All" ]
                ]
            , li []
                [ a [ href "#/active" ]
                    [ text "Active" ]
                ]
            , li []
                [ a [ href "#/completed" ]
                    [ text "Completed" ]
                ]
            ]
        , button [ class "clear-completed" ]
            [ text "Clear completed" ]
        ]
