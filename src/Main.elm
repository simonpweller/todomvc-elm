module Main exposing (main)

import Browser
import Html exposing (Attribute, a, button, div, footer, h1, header, input, label, li, main_, section, span, strong, text, ul)
import Html.Attributes exposing (autofocus, class, for, href, id, placeholder, type_, value)
import Html.Events exposing (keyCode, on, onBlur, onInput)
import Json.Decode as Json



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- UPDATE


type Msg
    = NoOp
    | UpdateNewTodoText String
    | SubmitNewTodo


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        UpdateNewTodoText text ->
            { model | newTodoText = text }

        SubmitNewTodo ->
            { model | todos = model.todos ++ [ String.trim model.newTodoText ], newTodoText = "" }



-- MODEL


type alias Model =
    { todos : List String
    , newTodoText : String
    }


init : Model
init =
    { todos = []
    , newTodoText = ""
    }



-- VIEW


view : Model -> Html.Html Msg
view model =
    main_ []
        (if List.isEmpty model.todos then
            [ newTodo model.newTodoText ]

         else
            [ newTodo model.newTodoText
            , section
                [ class "main" ]
                [ input [ class "toggle-all", id "toggle-all", type_ "checkbox" ]
                    []
                , label [ for "toggle-all" ]
                    [ text "Mark all as complete" ]
                , ul [ class "todo-list" ]
                    (List.map
                        todo
                        model.todos
                    )
                ]
            , footerHtml
            ]
        )


newTodo : String -> Html.Html Msg
newTodo newTodoText =
    header [ class "header" ]
        [ h1 []
            [ text "todos" ]
        , input [ autofocus True, class "new-todo", placeholder "What needs to be done?", value newTodoText, onBlur SubmitNewTodo, onInput UpdateNewTodoText, handleEnter ]
            []
        ]


handleEnter : Attribute Msg
handleEnter =
    let
        isEnter code =
            if code == 13 then
                Json.succeed SubmitNewTodo

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


todo : String -> Html.Html msg
todo todoText =
    li []
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox" ]
                []
            , label []
                [ text todoText ]
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
