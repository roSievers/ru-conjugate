module Main exposing (..)

{-|

https://en.wiktionary.org/wiki/Appendix:Russian_verbs
-}

import Return
import Html exposing (Html, div, text, input, button, br, img)
import Html.Attributes exposing (value, style, src, alt)
import Html.Events exposing (onInput, onClick)
import Random exposing (Generator)
import Random.Extra as Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { userInput : String
    , word : Word
    , state : State
    }


type State
    = Correct
    | Wrong
    | StillTyping


type Msg
    = SetUserInput String
    | AppendLetter Char
    | DeleteLetter
    | SetTarget Word
    | GenerateTargetWord
    | CheckInput


type Stem
    = Class1 String
    | Class4b String


type Use
    = Me
    | You
    | She
    | We
    | YouGroup
    | They


type alias Word =
    { stem : Stem
    , use : Use
    }


init : ( Model, Cmd Msg )
init =
    ( { userInput = ""
      , word = defaultWord
      , state = StillTyping
      }
    , Random.generate SetTarget randomWord
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUserInput newText ->
            Return.singleton
                { model
                    | userInput = newText
                    , state = StillTyping
                }

        AppendLetter char ->
            { model
                | userInput = model.userInput ++ String.fromChar char
                , state = StillTyping
            }
                |> Return.singleton

        DeleteLetter ->
            { model
                | userInput = String.dropRight 1 model.userInput
                , state = StillTyping
            }
                |> Return.singleton

        SetTarget word ->
            { model
                | word = word
                , userInput = questionPhrase word
                , state = StillTyping
            }
                |> Return.singleton

        GenerateTargetWord ->
            ( model, Random.generate SetTarget randomWord )

        CheckInput ->
            let
                newState =
                    if model.userInput == targetPhrase model.word then
                        Correct
                    else
                        Wrong
            in
                { model | state = newState }
                    |> Return.singleton


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [ style [ ( "text-align", "center" ) ] ]
        [ textInput model
        , br [] []
        , colorblindCorrect model
        , br [] []
        , text (infinitive model.word)
        , text " "
        , button [ onClick CheckInput ] [ text "Prüfen" ]
        , button [ onClick GenerateTargetWord ] [ text "Neues Wort" ]
        , keyboard
        , br [] [        ]
        , img [ src "qrcode.png", alt "QR-Code for the Website" ] []
        , br [] []
        , text "http://rosievers.github.io/ru-conjugate/"
        ]


colorblindCorrect : Model -> Html Msg
colorblindCorrect model =
    case model.state of
        Correct ->
            text "Richtig"
        Wrong ->
            text "Falsch"
        StillTyping ->
            text ""


textInput : Model -> Html Msg
textInput model =
    let
        color =
            case model.state of
                Correct ->
                    Just "#009900"

                Wrong ->
                    Just "#990000"

                StillTyping ->
                    Nothing

        inputStyles =
            [ Just ( "width", "30em" )
            , Just ( "margin", "1em" )
            , Maybe.map (\c -> ("color", c)) color ]
              |> List.filterMap identity
              |> style
    in
        input
            [ value model.userInput
            , onInput SetUserInput
            , inputStyles
            ]
            []



-- Russian keyboard


keyboard : Html Msg
keyboard =
    let
        row1 =
            div [] (List.map charButton (String.toList "йцукеёнгшщзхъ") ++ [ backspaceButton ])

        row2 =
            div [] (List.map charButton (String.toList "фывапролджэ"))

        row3 =
            div [] (List.map charButton (String.toList "ячсмитьбю"))
    in
        div [ style [ ("margin-top", "0.5em")]]
            [ row1
            , br [] []
            , row2
            , br [] []
            , row3
            , br [] []
            , spacebar
            ]


charButton : Char -> Html Msg
charButton char =
    button [ onClick (AppendLetter char) ]
        [ text (String.fromChar char) ]


backspaceButton : Html Msg
backspaceButton =
    button [ onClick (DeleteLetter) ]
        [ text "бекспейс" ]


spacebar : Html Msg
spacebar =
    button
        [ onClick (AppendLetter ' ')
        , style [ ( "width", "30em" ) ]
        ]
        [ text "Пробел" ]



-- Random Word Generation


randomUse : Generator Use
randomUse =
    Random.sample
        [ Me, You, She, We, YouGroup, They ]
        |> Random.map (Maybe.withDefault Me)


defaultStem : Stem
defaultStem =
    Class1 "де́ла"


randomClass1 : Generator (Maybe Stem)
randomClass1 =
    Random.sample
        [ "де́ла", "работа", "зна", "понима", "чита"]
        |> Random.map (Maybe.map Class1)


randomClass4b : Generator (Maybe Stem)
randomClass4b =
    Random.sample
        [ "говор" ]
        |> Random.map (Maybe.map Class4b)


randomStem : Generator Stem
randomStem =
    Random.choices
        [ randomClass1, randomClass4b ]
        |> Random.map (Maybe.withDefault defaultStem)


randomWord : Generator Word
randomWord =
    Random.map2 Word
        randomStem
        randomUse


defaultWord : Word
defaultWord =
    Word defaultStem Me



-- Conjugation


infinitive : Word -> String
infinitive word =
    case word.stem of
        Class1 stem ->
            stem ++ "ть"

        Class4b stem ->
            stem ++ "ить"


targetPhrase : Word -> String
targetPhrase word =
    person word.use
        ++ " "
        ++ conjugate word


questionPhrase : Word -> String
questionPhrase word =
    person word.use
        ++ " "
        ++ stem word


person : Use -> String
person use =
    case use of
        Me ->
            "я"

        You ->
            "ты"

        She ->
            "она"

        We ->
            "мы"

        YouGroup ->
            "вы"

        They ->
            "они"


conjugate : Word -> String
conjugate word =
    case word.stem of
        Class1 stem ->
            conjugateClass1 stem word.use

        Class4b stem ->
            conjugateClass4b stem word.use


stem : Word -> String
stem word =
    case word.stem of
        Class1 stem ->
            stem

        Class4b stem -> stem


conjugateClass1 : String -> Use -> String
conjugateClass1 stem use =
    case use of
        Me ->
            stem ++ "ю"

        You ->
            stem ++ "ешь"

        She ->
            stem ++ "ет"

        We ->
            stem ++ "ем"

        YouGroup ->
            stem ++ "ете"

        They ->
            stem ++ "ют"


conjugateClass4b : String -> Use -> String
conjugateClass4b stem use =
    case use of
        Me ->
            stem ++ "ю"

        You ->
            stem ++ "ишь"

        She ->
            stem ++ "ит"

        We ->
            stem ++ "им"

        YouGroup ->
            stem ++ "ите"

        They ->
            stem ++ "ят"
