module Main exposing (Model, Msg(..), init, main, update)

import Browser
import Html exposing (Html, div, input, text)
import Html.Events exposing (onInput)
import Parser exposing ((|.), (|=), Parser, end, int, map, run, succeed, symbol, variable)
import Pratt exposing (Config, infixLeft, literal, prefix, subExpression)
import Set


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    String


type Msg
    = NewText String


type AST
    = Conj AST AST
    | Disj AST AST
    | Impl AST AST
    | Var String
    | Eq AST AST
    | Neg AST
    | Yes
    | No


init : Model
init =
    "(X + Y) -> (!Z * W)"


update : Msg -> Model -> Model
update msg _ =
    case msg of
        NewText txt ->
            txt


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Use '+' as disjunction" ]
        , div [] [ text "'*' as conjunction" ]
        , div [] [ text "'->' as implication" ]
        , div [] [ text "'!' as negation" ]
        , div [] [ text "'1' as true" ]
        , div [] [ text "'0' as false" ]
        , div [] [ text "'=' as equivalent" ]
        , div [] [ input [ onInput NewText ] [] ]
        , div [] [ text <| astToHuman <| toAst model ]
        , div [] [ text <| astToHuman <| rules <| toAst model ]
        ]


toAst : String -> AST
toAst exp =
    Result.withDefault No <| run parser exp


astToHuman : AST -> String
astToHuman ast =
    case ast of
        Var v ->
            v

        No ->
            "0"

        Yes ->
            "1"

        Conj l r ->
            "(" ++ astToHuman l ++ " * " ++ astToHuman r ++ ")"

        Disj l r ->
            "(" ++ astToHuman l ++ " + " ++ astToHuman r ++ ")"

        Impl l r ->
            "(" ++ astToHuman l ++ " -> " ++ astToHuman r ++ ")"

        Neg exp ->
            "!" ++ astToHuman exp

        Eq l r ->
            "(" ++ astToHuman l ++ " = " ++ astToHuman r ++ ")"


rules : AST -> AST
rules ast =
    case ast of
        Impl l r ->
            Disj (rules (Neg l)) (rules r)

        Neg (Disj l r) ->
            Conj (rules (Neg l)) (rules (Neg r))

        Neg (Conj l r) ->
            Disj (rules (Neg l)) (rules (Neg r))

        Neg (Neg v) ->
            rules v

        Neg No ->
            Yes

        Neg Yes ->
            No

        Conj No _ ->
            No

        Conj Yes v ->
            rules v

        Disj Yes _ ->
            Yes

        Disj No v ->
            rules v

        _ ->
            ast


logicExpression : Parser AST
logicExpression =
    Pratt.expression
        { oneOf =
            [ literal <|
                map (\v -> Var v) <|
                    variable
                        { start = Char.isAlpha
                        , inner = Char.isAlpha
                        , reserved = Set.fromList [ "0", "1" ]
                        }
            , literal <|
                map
                    (\i ->
                        if i == 0 then
                            No

                        else
                            Yes
                    )
                <|
                    int
            , prefix 3 (symbol "!") Neg
            , parenthesizedExpression
            ]
        , andThenOneOf =
            [ infixLeft 1 (symbol "+") Disj
            , infixLeft 2 (symbol "*") Conj
            , infixLeft 3 (symbol "->") Impl
            , infixLeft 3 (symbol "=") Eq
            ]
        , spaces = Parser.spaces
        }


parenthesizedExpression : Config AST -> Parser AST
parenthesizedExpression config =
    succeed identity
        |. symbol "("
        |= subExpression 0 config
        |. symbol ")"


parser : Parser AST
parser =
    succeed identity
        |= logicExpression
        |. end
