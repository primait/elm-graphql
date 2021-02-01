module GraphQL.Client.Http exposing
    ( RequestError, DocumentLocation, sendQuery, sendMutation, RequestOptions, customSendQuery, customSendMutation
    , Result(..), graphQLValue
    )

{-| The functions in this module let you perform HTTP requests to conventional GraphQL server endpoints.

@docs Error, RequestError, DocumentLocation, sendQuery, sendMutation, RequestOptions, customSendQuery, customSendMutation, customSendQueryRaw, customSendMutationRaw

-}

import GraphQL.Client.Http.Util as Util
import GraphQL.Request.Builder as Builder
import GraphQL.Response
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


graphQLBodyWith : List ( String, Encode.Value ) -> Builder.Request operationType result -> Http.Body
graphQLBodyWith extraFields request =
    let
        documentString =
            Builder.requestBody request

        variableValues =
            Builder.jsonVariableValues request

        postBody =
            Util.postBodyJsonWith extraFields documentString variableValues
    in
    Http.stringBody "application/json" <| Encode.encode 0 postBody


graphQLBody : Builder.Request operationType result -> Http.Body
graphQLBody =
    graphQLBodyWith []


graphQLValue : Builder.Request operationType result -> { query : String, variables : Maybe Encode.Value }
graphQLValue request =
    { query = Builder.requestBody request, variables = Builder.jsonVariableValues request }


parseBody : Decoder data -> String -> Result data
parseBody dataDecoder body =
    let
        dataRes =
            Decode.decodeString (Decode.field "data" dataDecoder) body

        errors =
            body
                |> Decode.decodeString (Decode.field "errors" GraphQL.Response.errorsDecoder)
                |> Result.toMaybe
                |> Maybe.withDefault []
    in
    case ( dataRes, errors ) of
        ( Ok data, [] ) ->
            Success data

        ( Ok data, gqlErrors ) ->
            SuccessWithErrors gqlErrors data

        ( Err decoderError, [] ) ->
            HttpError (Http.BadBody <| Decode.errorToString decoderError)

        ( Err decoderError, gqlErrors ) ->
            DecoderError gqlErrors decoderError


graphQLExpect : (Result data -> msg) -> Builder.Request operationType data -> Http.Expect msg
graphQLExpect tagger request =
    let
        dataDecoder : Decoder data
        dataDecoder =
            Builder.responseDataDecoder request

        parser : Result.Result Http.Error String -> Result data
        parser result =
            case result of
                Ok body ->
                    parseBody dataDecoder body

                Err err ->
                    HttpError err
    in
    Http.expectString (parser >> tagger)


{-| An error returned by the GraphQL server that indicates there was something wrong with the request.
-}
type alias RequestError =
    { message : String
    , locations : List DocumentLocation
    }


{-| A location in a GraphQL request document.
-}
type alias DocumentLocation =
    { line : Int
    , column : Int
    }


{-| Represents errors that can occur when sending a GraphQL request over HTTP.
-}
type Result data
    = Success data
    | SuccessWithErrors (List RequestError) data
    | DecoderError (List RequestError) Decode.Error
    | HttpError Http.Error


{-| Takes a URL and a `Query` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendQuery :
    String
    -> (Result data -> msg)
    -> Builder.Request Builder.Query data
    -> Cmd msg
sendQuery =
    Util.defaultRequestOptions >> send


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendMutation :
    String
    -> (Result data -> msg)
    -> Builder.Request Builder.Mutation data
    -> Cmd msg
sendMutation =
    Util.defaultRequestOptions >> send


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint and return raw `Http.Response` in Task.
-}
sendMutationRaw :
    String
    -> (Result data -> msg)
    -> Builder.Request Builder.Mutation data
    -> Cmd msg
sendMutationRaw url tagger request =
    send (Util.defaultRequestOptions url) tagger request


{-| Options available for customizing GraphQL HTTP requests. `method` should be either `"GET"` or `"POST"`. For `GET` requests, the `url` is modified to include extra parameters in the query string for the GraphQL document and variables. Otherwise, the document and variables are included in the HTTP request body.
-}
type alias RequestOptions =
    { method : String
    , headers : List Http.Header
    , url : String
    , timeout : Maybe Float
    , tracker : Maybe String
    , withCredentials : Bool
    }


{-| Like `sendQuery`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request.
-}
customSendQuery :
    RequestOptions
    -> (Result data -> msg)
    -> Builder.Request Builder.Query data
    -> Cmd msg
customSendQuery =
    send


{-| Like `sendMutation`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request.
-}
customSendMutation :
    RequestOptions
    -> (Result data -> msg)
    -> Builder.Request Builder.Mutation data
    -> Cmd msg
customSendMutation =
    send


send :
    RequestOptions
    -> (Result data -> msg)
    -> Builder.Request operationType data
    -> Cmd msg
send options tagger request =
    let
        requestArgs =
            { method = options.method
            , headers = options.headers
            , url = options.url
            , body = graphQLBody request
            , expect = graphQLExpect tagger request
            , timeout = options.timeout
            , tracker = options.tracker
            }
    in
    if options.withCredentials then
        Http.riskyRequest requestArgs

    else
        Http.request requestArgs
