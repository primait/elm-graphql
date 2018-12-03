module GraphQL.Client.Http.Util exposing (parameterizedUrl, postBody, postBodyJson)

import Http
import Json.Encode
import Url


postBodyJson : String -> Maybe Json.Encode.Value -> Json.Encode.Value
postBodyJson documentString variableValues =
    let
        documentValue =
            Json.Encode.string documentString

        extraParams =
            variableValues
                |> Maybe.map (\obj -> [ ( "variables", obj ) ])
                |> Maybe.withDefault []
    in
    Json.Encode.object ([ ( "query", documentValue ) ] ++ extraParams)


postBody : String -> Maybe Json.Encode.Value -> Http.Body
postBody documentString variableValues =
    Http.jsonBody (postBodyJson documentString variableValues)


parameterizedUrl : String -> String -> Maybe Json.Encode.Value -> String
parameterizedUrl url documentString variableValues =
    let
        firstParamPrefix =
            if String.contains "?" url then
                "&"

            else
                "?"

        queryParam =
            firstParamPrefix ++ "query=" ++ Url.percentEncode documentString

        variablesParam =
            variableValues
                |> Maybe.map
                    (\obj ->
                        "&variables=" ++ Url.percentEncode (Json.Encode.encode 0 obj)
                    )
                |> Maybe.withDefault ""
    in
    url ++ queryParam ++ variablesParam
