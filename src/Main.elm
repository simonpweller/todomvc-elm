module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Debug exposing (todo)
import Html exposing (Attribute, a, button, div, footer, h1, header, input, label, li, main_, section, span, strong, text, ul)
import Html.Attributes exposing (autofocus, checked, class, classList, for, href, id, placeholder, type_, value)
import Html.Events exposing (keyCode, on, onBlur, onClick, onDoubleClick, onInput)
import Json.Decode as Decode exposing (Decoder, bool, decodeString, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Ports
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


type Filter
    = All
    | Active
    | Completed


type alias Model =
    { todos : List Todo
    , newTodoText : String
    , nextId : Int
    , editing : Maybe Todo
    , filter : Filter
    }


init : Maybe String -> ( Model, Cmd Msg )
init storedTodos =
    let
        todos =
            case storedTodos of
                Just todosJson ->
                    decodeSavedTodos todosJson

                Nothing ->
                    []
    in
    ( { todos = todos
      , newTodoText = ""
      , nextId = 0
      , editing = Nothing
      , filter = All
      }
    , Task.attempt (\_ -> Noop) (Dom.focus "new-todo")
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
    | UpdateEditingText String
    | CompleteEditing
    | CancelEditing
    | SetFilter Filter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        UpdateNewTodoText text ->
            ( { model | newTodoText = text }, Cmd.none )

        SubmitNewTodo ->
            let
                todos =
                    model.todos ++ [ Todo (String.trim model.newTodoText) model.nextId False ]
            in
            if String.isEmpty (String.trim model.newTodoText) then
                ( model, Cmd.none )

            else
                ( { model | todos = todos, newTodoText = "", nextId = model.nextId + 1 }, saveTodos todos )

        Toggle todo ->
            let
                todos =
                    toggleTodo todo model.todos
            in
            ( { model | todos = todos }, saveTodos todos )

        ToggleAll ->
            let
                todos =
                    List.map
                        (toggleTo (anyOpen model.todos))
                        model.todos
            in
            ( { model
                | todos = todos
              }
            , saveTodos todos
            )

        Remove todo ->
            let
                todos =
                    removeTodo todo model.todos
            in
            ( { model | todos = todos }, saveTodos todos )

        RemoveCompleted ->
            let
                todos =
                    removeCompleted model.todos
            in
            ( { model | todos = todos }, saveTodos todos )

        StartEditing todo ->
            ( { model | editing = Just todo }, Task.attempt (\_ -> Noop) (Dom.focus ("todo-" ++ String.fromInt todo.id)) )

        UpdateEditingText text ->
            ( { model
                | editing =
                    case model.editing of
                        Nothing ->
                            Nothing

                        Just t ->
                            Just { t | text = text }
              }
            , Cmd.none
            )

        CompleteEditing ->
            let
                todos =
                    case model.editing of
                        Nothing ->
                            model.todos

                        Just t ->
                            if String.isEmpty (String.trim t.text) then
                                removeTodo t model.todos

                            else
                                updateTodo t model.todos
            in
            ( { model
                | todos = todos
                , editing = Nothing
              }
            , saveTodos todos
            )

        CancelEditing ->
            ( { model | editing = Nothing }, Cmd.none )

        SetFilter filter ->
            ( { model | filter = filter }, Cmd.none )



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
updateTodo editedTodo list =
    let
        replace todo =
            if todo.id == editedTodo.id then
                { editedTodo | text = String.trim editedTodo.text }

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


saveTodos : List Todo -> Cmd msg
saveTodos todos =
    Encode.list todoEncoder todos
        |> Encode.encode 0
        |> Ports.storeTodos


decodeSavedTodos : String -> List Todo
decodeSavedTodos todosJson =
    case decodeString todosDecoder todosJson of
        Ok todos ->
            todos

        Err _ ->
            []


todosDecoder : Decoder (List Todo)
todosDecoder =
    list todoDecoder


todoDecoder : Decoder Todo
todoDecoder =
    Decode.succeed Todo
        |> required "text" string
        |> required "id" int
        |> required "completed" bool


todoEncoder : Todo -> Encode.Value
todoEncoder todo =
    Encode.object
        [ ( "id", Encode.int todo.id )
        , ( "text", Encode.string todo.text )
        , ( "completed", Encode.bool todo.completed )
        ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        filteredTodos =
            List.filter
                (\todo ->
                    case model.filter of
                        All ->
                            True

                        Active ->
                            not todo.completed

                        Completed ->
                            todo.completed
                )
                model.todos
    in
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
                        filteredTodos
                    )
                ]
            , renderFooter model
            ]
        )


newTodo : String -> Html.Html Msg
newTodo newTodoText =
    header [ class "header" ]
        [ h1 []
            [ text "todos" ]
        , input [ autofocus True, class "new-todo", id "new-todo", placeholder "What needs to be done?", value newTodoText, onBlur SubmitNewTodo, onInput UpdateNewTodoText, on "keydown" (Decode.andThen handleSubmit keyCode) ]
            []
        ]


handleSubmit : number -> Decode.Decoder Msg
handleSubmit code =
    if code == 13 then
        Decode.succeed SubmitNewTodo

    else
        Decode.fail "not ENTER"


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
        , input
            [ class "edit"
            , value
                (case editing of
                    Nothing ->
                        ""

                    Just t ->
                        t.text
                )
            , id ("todo-" ++ String.fromInt todo.id)
            , onInput UpdateEditingText
            , onBlur CompleteEditing
            , on "keydown" (Decode.andThen handleKeyDown keyCode)
            ]
            []
        ]


handleKeyDown : number -> Decode.Decoder Msg
handleKeyDown code =
    if code == 13 then
        Decode.succeed CompleteEditing

    else if code == 27 then
        Decode.succeed CancelEditing

    else
        Decode.fail "not ENTER"


renderFooter : Model -> Html.Html Msg
renderFooter model =
    footer [ class "footer" ]
        [ span [ class "todo-count" ]
            [ strong []
                [ text (String.fromInt (openCount model.todos)) ]
            , text
                (if openCount model.todos == 1 then
                    " item left"

                 else
                    " items left"
                )
            ]
        , ul [ class "filters" ]
            [ li []
                [ a [ onClick (SetFilter All), classList [ ( "selected", model.filter == All ) ], href "#/" ]
                    [ text "All" ]
                ]
            , li []
                [ a [ onClick (SetFilter Active), classList [ ( "selected", model.filter == Active ) ], href "#/active" ]
                    [ text "Active" ]
                ]
            , li []
                [ a [ onClick (SetFilter Completed), classList [ ( "selected", model.filter == Completed ) ], href "#/completed" ]
                    [ text "Completed" ]
                ]
            ]
        , if anyDone model.todos then
            button [ class "clear-completed", onClick RemoveCompleted ]
                [ text "Clear completed" ]

          else
            text ""
        ]
