module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Attribute, a, button, div, footer, h1, header, input, label, li, main_, section, span, strong, text, ul)
import Html.Attributes exposing (autofocus, checked, class, classList, for, href, id, placeholder, type_, value)
import Html.Events exposing (keyCode, on, onBlur, onClick, onDoubleClick, onInput)
import Json.Decode as Json
import Task



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



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
    , editing : Maybe Todo
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { todos = []
      , newTodoText = ""
      , nextId = 0
      , editing = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Noop
    | UpdateNewTodoText String
    | SubmitNewTodo
    | Toggle Todo
    | ToggleAll
    | Remove Todo
    | RemoveCompleted
    | StartEditing Todo
    | CompleteEditing Todo
    | CancelEditing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        UpdateNewTodoText text ->
            ( { model | newTodoText = text }, Cmd.none )

        SubmitNewTodo ->
            ( if String.isEmpty (String.trim model.newTodoText) then
                model

              else
                { model | todos = model.todos ++ [ Todo (String.trim model.newTodoText) model.nextId False ], newTodoText = "", nextId = model.nextId + 1 }
            , Cmd.none
            )

        Toggle todo ->
            ( { model | todos = toggleTodo todo model.todos }, Cmd.none )

        ToggleAll ->
            ( { model
                | todos =
                    List.map
                        (toggleTo (anyOpen model.todos))
                        model.todos
              }
            , Cmd.none
            )

        Remove todo ->
            ( { model | todos = removeTodo todo model.todos }, Cmd.none )

        RemoveCompleted ->
            ( { model | todos = removeCompleted model.todos }, Cmd.none )

        StartEditing todo ->
            ( { model | editing = Just todo }, Task.attempt (\_ -> Noop) (Dom.focus ("todo-" ++ String.fromInt todo.id)) )

        CompleteEditing todo ->
            ( { model | todos = updateTodo todo model.todos, editing = Nothing }, Cmd.none )

        CancelEditing ->
            ( { model | editing = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


toggleTodo : Todo -> List Todo -> List Todo
toggleTodo todoToToggle list =
    let
        toggle todo =
            if todo.id == todoToToggle.id then
                { todo | completed = isOpen todo }

            else
                todo
    in
    List.map toggle list


toggleTo : Bool -> Todo -> Todo
toggleTo to todo =
    { todo | completed = to }


removeTodo : Todo -> List Todo -> List Todo
removeTodo todoToRemove list =
    let
        filter todo =
            not (todo.id == todoToRemove.id)
    in
    List.filter filter list


updateTodo : Todo -> List Todo -> List Todo
updateTodo todoToUpdate list =
    let
        replace todo =
            if todo.id == todoToUpdate.id then
                todoToUpdate

            else
                todo
    in
    List.map replace list


removeCompleted : List Todo -> List Todo
removeCompleted todos =
    List.filter isOpen todos


isDone : Todo -> Bool
isDone todo =
    todo.completed


isOpen : Todo -> Bool
isOpen todo =
    not (isDone todo)


anyOpen : List Todo -> Bool
anyOpen todos =
    List.any isOpen todos


anyDone : List Todo -> Bool
anyDone todos =
    List.any isDone todos


allDone : List Todo -> Bool
allDone todos =
    not (anyOpen todos)


openCount : List Todo -> Int
openCount todos =
    List.length (List.filter isOpen todos)


isEditing : Maybe Todo -> Todo -> Bool
isEditing editing todoToCheck =
    case editing of
        Just a ->
            a.id == todoToCheck.id

        Nothing ->
            False



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
                [ input [ class "toggle-all", id "toggle-all", type_ "checkbox", onClick ToggleAll, checked (allDone model.todos) ]
                    []
                , label [ for "toggle-all" ]
                    [ text "Mark all as complete" ]
                , ul [ class "todo-list" ]
                    (List.map
                        (renderTodo model.editing)
                        model.todos
                    )
                ]
            , renderFooter model.todos
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


renderTodo : Maybe Todo -> Todo -> Html.Html Msg
renderTodo editing todo =
    li
        [ classList
            [ ( "completed", todo.completed )
            , ( "editing", isEditing editing todo )
            ]
        ]
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", onClick (Toggle todo), checked (isDone todo) ]
                []
            , label [ onDoubleClick (StartEditing todo) ]
                [ text todo.text ]
            , button [ class "destroy", onClick (Remove todo) ]
                []
            ]
        , input [ class "edit", value todo.text, id ("todo-" ++ String.fromInt todo.id) ]
            []
        ]


renderFooter : List Todo -> Html.Html Msg
renderFooter todos =
    footer [ class "footer" ]
        [ span [ class "todo-count" ]
            [ strong []
                [ text (String.fromInt (openCount todos)) ]
            , text
                (if openCount todos == 1 then
                    " item left"

                 else
                    " items left"
                )
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
        , if anyDone todos then
            button [ class "clear-completed", onClick RemoveCompleted ]
                [ text "Clear completed" ]

          else
            text ""
        ]
