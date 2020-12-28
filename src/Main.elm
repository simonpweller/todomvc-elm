module Main exposing (main)

import Html exposing (button, div, input, label, li, section, text, ul)
import Html.Attributes exposing (attribute, class, for, id, type_, value)


main : Html.Html msg
main =
    section [ class "main" ]
        [ input [ class "toggle-all", id "toggle-all", type_ "checkbox" ]
            []
        , label [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , ul [ class "todo-list" ]
            [ completedTodo
            , openTodo
            ]
        ]


completedTodo : Html.Html msg
completedTodo =
    li [ class "completed" ]
        [ div [ class "view" ]
            [ input [ attribute "checked" "", class "toggle", type_ "checkbox" ]
                []
            , label []
                [ text "Taste JavaScript" ]
            , button [ class "destroy" ]
                []
            ]
        , input [ class "edit", value "Create a TodoMVC template" ]
            []
        ]


openTodo : Html.Html msg
openTodo =
    li []
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox" ]
                []
            , label []
                [ text "Buy a unicorn" ]
            , button [ class "destroy" ]
                []
            ]
        , input [ class "edit", value "Rule the web" ]
            []
        ]
