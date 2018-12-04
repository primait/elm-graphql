module GraphQL.Client.Http exposing
    ( Error(..), RequestError, DocumentLocation, sendQuery, sendMutation, RequestOptions, customSendQuery, customSendMutation
    , customSendMutationTask, customSendMutationWithBodyParts, customSendQueryTask, sendMutationTask, sendQueryTask
    )

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


{-| Takes a URL and a `Query` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendQuery :
    String
    -> (Result Error result -> msg)
    -> Builder.Request Builder.Query result
    -> Cmd msg
sendQuery =
    defaultRequestOptions >> send Nothing


{-| Takes a URL and a `Mutation` `Request` and returns a `Task` that you can perform with `Task.attempt` which will send a `POST` request to a GraphQL server at the given endpoint.
-}
sendMutation :
    String
    -> (Result Error result -> msg)
    -> Builder.Request Builder.Mutation result
    -> Cmd msg
sendMutation =
    defaultRequestOptions >> send Nothing


{-| Like `sendQuery`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request.
-}
customSendQuery :
    RequestOptions
    -> (Result Error result -> msg)
    -> Builder.Request Builder.Query result
    -> Cmd msg
customSendQuery =
    send Nothing


{-| Like `sendMutation`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request.
-}
customSendMutation :
    RequestOptions
    -> (Result Error result -> msg)
    -> Builder.Request Builder.Mutation result
    -> Cmd msg
customSendMutation =
    send Nothing


{-| Like `sendMutation`, but takes an `RequestOptions` value instead of a URL to let you further customize the HTTP request.
-}
customSendMutationWithBodyParts :
    String
    -> List Http.Part
    -> RequestOptions
    -> (Result Error result -> msg)
    -> Builder.Request Builder.Mutation result
    -> Cmd msg
customSendMutationWithBodyParts jsonPartName parts =
    send
        (Just
            { jsonPartName = jsonPartName
            , additionalParts = parts
            }
        )


type alias BodyPartsConfig =
    { jsonPartName : String
    , additionalParts : List Http.Part
    }


send :
    Maybe BodyPartsConfig
    -> RequestOptions
    -> (Result Error result -> msg)
    -> Builder.Request operationType result
    -> Cmd msg
send maybeBodyPartsConfig requestOptions toMsg request =
    let
        httpRequest =
            if requestOptions.risky then
                Http.riskyRequest

            else
                Http.request
    in
    requestConfig maybeBodyPartsConfig requestOptions toMsg request
        |> httpRequest


{-| -}
sendQueryTask :
    String
    -> Builder.Request Builder.Query result
    -> Task Error result
sendQueryTask =
    defaultRequestOptions >> task Nothing


{-| -}
sendMutationTask :
    String
    -> Builder.Request Builder.Mutation result
    -> Task Error result
sendMutationTask =
    defaultRequestOptions >> task Nothing


{-| -}
customSendQueryTask :
    RequestOptions
    -> Builder.Request Builder.Query result
    -> Task Error result
customSendQueryTask =
    task Nothing


{-| -}
customSendMutationTask :
    RequestOptions
    -> Builder.Request Builder.Mutation result
    -> Task Error result
customSendMutationTask =
    task Nothing


task :
    Maybe BodyPartsConfig
    -> RequestOptions
    -> Builder.Request operationType result
    -> Task Error result
task maybeBodyPartsConfig requestOptions request =
    let
        httpTask =
            if requestOptions.risky then
                Http.riskyTask

            else
                Http.task
    in
    taskConfig maybeBodyPartsConfig requestOptions request
        |> httpTask


resolveGraphQL :
    Builder.Request operationType result
    -> Http.Resolver Error result
resolveGraphQL request =
    Http.stringResolver (responseToResult request)


expectGraphQL :
    (Result Error result -> msg)
    -> Builder.Request operationType result
    -> Http.Expect msg
expectGraphQL toMsg request =
    Http.expectStringResponse toMsg (responseToResult request)


responseToResult : Builder.Request operationType result -> Http.Response String -> Result Error result
responseToResult request response =
    let
        decoder =
            Json.Decode.map2 Tuple.pair
                (Json.Decode.maybe <| Json.Decode.field "errors" Response.errorsDecoder)
                (Json.Decode.maybe <| Json.Decode.field "data" (Builder.responseDataDecoder request))
    in
    case response of
        Http.BadUrl_ url ->
            Err (HttpError (Http.BadUrl url))

        Http.Timeout_ ->
            Err (HttpError Http.Timeout)

        Http.NetworkError_ ->
            Err (HttpError Http.NetworkError)

        Http.BadStatus_ metadata _ ->
            Err (HttpError (Http.BadStatus metadata.statusCode))

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Ok ( Just errors, _ ) ->
                    Err (GraphQLError errors)

                Ok ( Nothing, Just data ) ->
                    Ok data

                Ok ( Nothing, Nothing ) ->
                    Err (HttpError (Http.BadBody "`data` and `errors` fields are missing"))

                Err err ->
                    Err (HttpError (Http.BadBody (Json.Decode.errorToString err)))


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
    Maybe BodyPartsConfig
    -> RequestOptions
    -> (Result Error result -> msg)
    -> Builder.Request operationType result
    -> RequestConfig msg
requestConfig maybeBodyPartsConfig requestOptions toMsg request =
    let
        expect =
            expectGraphQL toMsg request

        { url, body } =
            httpPayload maybeBodyPartsConfig requestOptions request
    in
    { method = requestOptions.method
    , headers = requestOptions.headers
    , url = url
    , body = body
    , expect = expect
    , timeout = requestOptions.timeout
    , tracker = requestOptions.tracker
    }


type alias TaskConfig result =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , resolver : Http.Resolver Error result
    , timeout : Maybe Float
    }


taskConfig :
    Maybe BodyPartsConfig
    -> RequestOptions
    -> Builder.Request operationType result
    -> TaskConfig result
taskConfig maybeBodyPartsConfig requestOptions request =
    let
        resolver =
            resolveGraphQL request

        { url, body } =
            httpPayload maybeBodyPartsConfig requestOptions request
    in
    { method = requestOptions.method
    , headers = requestOptions.headers
    , url = url
    , body = body
    , resolver = resolver
    , timeout = requestOptions.timeout
    }


httpPayload :
    Maybe BodyPartsConfig
    -> RequestOptions
    -> Builder.Request operationType result
    -> { url : String, body : Http.Body }
httpPayload maybeBodyPartsConfig requestOptions request =
    let
        documentString =
            Builder.requestBody request

        variableValues =
            Builder.jsonVariableValues request
    in
    if requestOptions.method == "GET" then
        { url = Util.parameterizedUrl requestOptions.url documentString variableValues
        , body = Http.emptyBody
        }

    else
        case maybeBodyPartsConfig of
            Just filesConfig ->
                { url = requestOptions.url
                , body =
                    Http.multipartBody <|
                        Http.stringPart filesConfig.jsonPartName (Util.postBodyJson documentString variableValues |> Json.Encode.encode 0)
                            :: filesConfig.additionalParts
                }

            Nothing ->
                { url = requestOptions.url, body = Util.postBody documentString variableValues }
