module Main exposing (..)

import Browser
import GraphQL.Client.Http as GraphQLClient
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Response
import Html exposing (Html, div, text)
import Http
import Task exposing (Task)


{-| Responses to `starWarsRequest` are decoded into this type.
-}
type alias FilmSummary =
    { title : Maybe String
    , someCharacterNames : List (Maybe String)
    , somePlanetNames : Maybe (List (Maybe String))
    }


{-| The definition of `starWarsRequest` builds up a query request value that
will later be encoded into the following GraphQL query document:
fragment filmPlanetsFragment on Film {
  planetConnection(first: $pageSize) {
    edges {
      node {
        name
      }
    }
  }
}
query ($filmID: ID!, $pageSize: Int = 3) {
  film(filmID: $filmID) {
    title
    characterConnection(first: $pageSize) {
      edges {
        node {
          name
        }
      }
    }
    ...filmPlanetsFragment
  }
}
This query is sent along with variable values extracted from the record passed
to `request`, and the response is decoded into a `FilmSummary`.
-}
starWarsRequest : Request Query FilmSummary
starWarsRequest =
    let
        filmID =
            Var.required "filmID" .filmID Var.id

        pageSize =
            Var.optional "pageSize" .pageSize (Var.nullable Var.int) (Just 3)

        planetsFragment =
            fragment "filmPlanetsFragment"
                (onType "Film")
                (extract
                    (field "planetConnection"
                        [ ( "first", Arg.variable pageSize ) ]
                        (connectionNodes (extract (field "name" [] (nullable string))))
                    )
                )
    in
    extract
        (field "film"
            [ ( "filmID", Arg.variable filmID ) ]
            (object FilmSummary
                |> with (field "title" [] (nullable string))
                |> with
                    (field "characterConnection"
                        [ ( "first", Arg.variable pageSize ) ]
                        (connectionNodes (extract (field "name" [] (nullable string))))
                    )
                |> with (fragmentSpread planetsFragment)
            )
        )
        |> queryDocument
        |> request
            { filmID = "1"
            , pageSize = Nothing
            }


{-| A function that helps you extract node objects from paginated Relay connections.
-}
connectionNodes :
    ValueSpec NonNull ObjectType result vars
    -> ValueSpec NonNull ObjectType (List result) vars
connectionNodes spec =
    extract
        (field "edges"
            []
            (list
                (extract
                    (field "node" [] spec)
                )
            )
        )


type Model
    = Resp FilmSummary
    | Errors String
    | Loading


type Msg
    = QueryResponse FilmSummary
    | GraphQLErrors (List GraphQL.Response.RequestError)
    | HttpError Http.Error


graphQLToMsg : GraphQLClient.Result FilmSummary -> Msg
graphQLToMsg result =
    case result of
        GraphQLClient.Success data ->
            QueryResponse data

        GraphQLClient.SuccessWithErrors _ data ->
            QueryResponse data

        GraphQLClient.DecoderError err _ ->
            GraphQLErrors err

        GraphQLClient.HttpError err ->
            HttpError err


sendQueryRequest : Request Query FilmSummary -> Cmd Msg
sendQueryRequest request =
    GraphQLClient.sendQuery "/" graphQLToMsg request


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading, sendQueryRequest starWarsRequest )


view : Model -> Browser.Document Msg
view model =
    { title = "Example"
    , body =
        [ viewModel model ]
    }


viewModel : Model -> Html Msg
viewModel model =
    case model of
        Loading ->
            Html.text "Loading..."

        Errors e ->
            Html.text ("Oh no! I got this error " ++ e)

        Resp r ->
            viewFilmSummary r


viewFilmSummary : FilmSummary -> Html Msg
viewFilmSummary summary =
    Html.div []
        [ Html.text ("Title: " ++ Maybe.withDefault "Unknown" summary.title)
        , viewCharacterNames summary.someCharacterNames
        , viewPlanetNames <| Maybe.withDefault [] summary.somePlanetNames
        ]


viewCharacterNames : List (Maybe String) -> Html Msg
viewCharacterNames names =
    Html.div []
        [ Html.text "Character names: "
        , viewNameList names
        ]


viewPlanetNames : List (Maybe String) -> Html Msg
viewPlanetNames names =
    Html.div []
        [ Html.text "Planet names: "
        , viewNameList names
        ]


viewNameList : List (Maybe String) -> Html Msg
viewNameList names =
    names
        |> List.map (Maybe.withDefault " -- ")
        |> String.join ", "
        |> Html.text


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl err ->
            "Bad url: " ++ err

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
            "Bad status code: " ++ String.fromInt code

        Http.BadBody err ->
            "Bad body: " ++ err


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryResponse data ->
            ( Resp data, Cmd.none )

        GraphQLErrors gqlErrors ->
            ( gqlErrors |> List.map .message |> String.join ", " |> Errors, Cmd.none )

        HttpError err ->
            ( err |> httpErrorToString |> Errors, Cmd.none )
