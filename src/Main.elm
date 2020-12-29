module Main exposing (main)

import Browser
import Html exposing (Attribute, a, button, div, footer, h1, header, input, label, li, main_, section, span, strong, text, ul)
import Html.Attributes exposing (autofocus, class, for, href, id, placeholder, type_, value)
import Html.Events exposing (keyCode, on, onBlur, onClick, onInput)
import Json.Decode as Json



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Todo =
    { text : String
    , id : Int
    , completed : Bool
    }


type alias Model =
    { todos : List Todo
    , newTodoText : String
    , nextId : Int
    }


init : Model
init =
    { todos = []
    , newTodoText = ""
    , nextId = 0
    }



-- UPDATE


type Msg
    = UpdateNewTodoText String
    | SubmitNewTodo
    | ToggleTodo Todo


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateNewTodoText text ->
            { model | newTodoText = text }

        SubmitNewTodo ->
            if String.isEmpty (String.trim model.newTodoText) then
                model

            else
                { model | todos = model.todos ++ [ Todo (String.trim model.newTodoText) model.nextId False ], newTodoText = "", nextId = model.nextId + 1 }

        ToggleTodo todo ->
            { model | todos = toggleTodo todo model.todos }


toggleTodo : Todo -> List Todo -> List Todo
toggleTodo todoToToggle list =
    let
        toggle todo =
            if todo.id == todoToToggle.id then
                { todo | completed = not todo.completed }

            else
                todo
    in
    List.map toggle list



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
                        renderTodo
                        model.todos
                    )
                ]
            , renderFooter
            ]
        )


newTodo : String -> Html.Html Msg
newTodo newTodoText =
    header [ class "header" ]
        [ h1 []
            [ text "todos" ]
        , input [ autofocus True, class "new-todo", placeholder "What needs to be done?", value newTodoText, onBlur SubmitNewTodo, onInput UpdateNewTodoText, on "keydown" (Json.andThen handleKeyDown keyCode) ]
            []
        ]


handleKeyDown : number -> Json.Decoder Msg
handleKeyDown code =
    if code == 13 then
        Json.succeed SubmitNewTodo

    else
        Json.fail "not ENTER"


renderTodo : Todo -> Html.Html Msg
renderTodo todo =
    li
        (if todo.completed then
            [ class "completed" ]

         else
            []
        )
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", onClick (ToggleTodo todo) ]
                []
            , label []
                [ text todo.text ]
            ]
        , input [ class "edit", value "Create a TodoMVC template" ]
            []
        ]


renderFooter =
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
