module GraphQL.Client.Http exposing (Error(..), RequestError, DocumentLocation, sendQuery, sendMutation, RequestOptions, customSendQuery, customSendMutation)

{-| The functions in this module let you perform HTTP requests to conventional GraphQL server endpoints.

@docs Error, RequestError, DocumentLocation, sendQuery, sendMutation, RequestOptions, customSendQuery, customSendMutation, customSendQueryRaw, customSendMutationRaw

-}

import GraphQL.Client.Http.Util as Util
import GraphQL.Request.Builder as Builder
import GraphQL.Response as Response
import Http
import Json.Decode
import Json.Encode
import Task exposing (Task)


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
type Error
    = HttpError Http.Error
    | GraphQLError (List RequestError)


{-| Options available for customizing GraphQL HTTP requests. `method` should be either `"GET"` or `"POST"`. For `GET` requests, the `url` is modified to include extra parameters in the query string for the GraphQL document and variables. Otherwise, the document and variables are included in the HTTP request body.
-}
type alias RequestOptions =
    { method : String
    , headers : List Http.Header
    , url : String
    , timeout : Maybe Float
    , tracker : Maybe String
    , risky : Bool
    }


defaultRequestOptions : String -> RequestOptions
defaultRequestOptions url =
    { method = "POST"
    , headers = []
    , url = url
    , timeout = Nothing
    , tracker = Nothing
    , risky = False
    }


type alias RequestConfig msg =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }


requestConfig :
    RequestOptions
    -> String
    -> Http.Expect a
    -> Maybe Json.Encode.Value
    -> RequestConfig a
requestConfig requestOptions documentString expect variableValues =
    let
        ( url, body ) =
            if requestOptions.method == "GET" then
                ( Util.parameterizedUrl requestOptions.url documentString variableValues, Http.emptyBody )

            else
                ( requestOptions.url, Util.postBody documentString variableValues )
    in
    { method = requestOptions.method
    , headers = requestOptions.headers
    , url = url
    , body = body
    , expect = expect
    , timeout = requestOptions.timeout
    , tracker = requestOptions.tracker
    }


{-| Takes a URL and a `Query` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendQuery :
    String
    -> (Result Error result -> msg)
    -> Builder.Request Builder.Query result
    -> Cmd msg
sendQuery =
    defaultRequestOptions >> send


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendMutation :
    String
    -> (Result Error result -> msg)
    -> Builder.Request Builder.Mutation result
    -> Cmd msg
sendMutation =
    defaultRequestOptions >> send


{-| Like `sendQuery`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request.
-}
customSendQuery :
    RequestOptions
    -> (Result Error result -> msg)
    -> Builder.Request Builder.Query result
    -> Cmd msg
customSendQuery =
    send


{-| Like `sendMutation`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request.
-}
customSendMutation :
    RequestOptions
    -> (Result Error result -> msg)
    -> Builder.Request Builder.Mutation result
    -> Cmd msg
customSendMutation =
    send


send :
    RequestOptions
    -> (Result Error result -> msg)
    -> Builder.Request operationType result
    -> Cmd msg
send requestOptions toMsg request =
    let
        expect =
            expectGraphQL toMsg request

        documentString =
            Builder.requestBody request

        variableValues =
            Builder.jsonVariableValues request

        httpRequest =
            if requestOptions.risky then
                Http.riskyRequest

            else
                Http.request
    in
    requestConfig requestOptions documentString expect variableValues
        |> httpRequest


expectGraphQL :
    (Result Error result -> msg)
    -> Builder.Request operationType result
    -> Http.Expect msg
expectGraphQL toMsg request =
    let
        decoder =
            Json.Decode.map2 Tuple.pair
                (Json.Decode.maybe (Json.Decode.field "errors" Response.errorsDecoder))
                (Json.Decode.field "data" (Builder.responseDataDecoder request))
    in
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (HttpError (Http.BadUrl url))

                Http.Timeout_ ->
                    Err (HttpError Http.Timeout)

                Http.NetworkError_ ->
                    Err (HttpError Http.NetworkError)

                Http.BadStatus_ metadata _ ->
                    Err (HttpError (Http.BadStatus metadata.statusCode))

                Http.GoodStatus_ metadata body ->
                    case Json.Decode.decodeString decoder body of
                        Ok ( Just errors, _ ) ->
                            Err (GraphQLError errors)

                        Ok ( Nothing, data ) ->
                            Ok data

                        Err err ->
                            Err (HttpError (Http.BadBody (Json.Decode.errorToString err)))
