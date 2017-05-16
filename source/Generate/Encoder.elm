module Generate.Encoder exposing (..)

import Generate.Utils exposing (typeName, encoderName)
import Swagger.Definition as Def exposing (Definition, getType, getFullName)
import Codegen.Function exposing (function, arg, pipeline, letin, caseof, lazy)
import Codegen.List exposing (list)
import Codegen.Literal exposing (string)
import Codegen.Tuple exposing (tuple)
import Swagger.Type
    exposing
        ( Type(Object_, Array_, Dict_, String_, Enum_, Int_, Float_, Bool_, Ref_)
        , Properties(Properties)
        , Property(Required, Optional, Default)
        , getItemsType
        )


renderEncoder : Definition -> String
renderEncoder definition =
    let
        name =
            getFullName definition
    in
        function (encoderName <| name)
            [ arg (typeName name) "value" ]
            ("Json.Encode.Value")
            (renderEncoderBody definition)


renderEncoderBody : Definition -> String
renderEncoderBody definition =
    case getType definition of
        Object_ properties ->
            renderObjectBody (getFullName definition) properties

        String_ _ ->
            renderPrimitiveBody "string"

        Int_ _ ->
            renderPrimitiveBody "int"

        Float_ _ ->
            renderPrimitiveBody "float"

        Bool_ _ ->
            renderPrimitiveBody "bool"

        _ ->
            "mjau"


renderPrimitiveBody : String -> String
renderPrimitiveBody typeName =
    "Json.Encode." ++ typeName ++ " value"


renderObjectBody : String -> Properties -> String
renderObjectBody name (Properties properties) =
    properties
        |> List.map (renderObjectProperty name)
        |> list
        |> (++) "Json.Encode.object "


renderObjectProperty : String -> Property -> String
renderObjectProperty parentName property =
    case property of
        Required name type_ ->
            tuple (string name) "mu"

        Optional name type_ ->
            tuple (string name) "mu"

        Default name type_ _ ->
            tuple (string name) "mu"
