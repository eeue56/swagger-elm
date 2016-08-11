module Codegen.Function exposing (Arg, function, pipeline)

import String


type alias Args =
    List Arg


type Arg
    = Arg Type Name


type alias Type =
    String


type alias Name =
    String


type alias Body =
    String


function : Name -> Args -> Type -> Body -> String
function name args type' body =
    (functionType name args type' body)
        ++ "\n"
        ++ (functionDeclaration name args type' body)
        ++ "\n"


functionType : Name -> Args -> Type -> Body -> String
functionType name args type' body =
    name
        ++ " : "
        ++ (String.concat <| List.map ((flip (++) " -> ") << argType) args)
        ++ type'


functionDeclaration : Name -> Args -> Type -> Body -> String
functionDeclaration name args type' body =
    name ++ (String.join " " <| List.map argName args) ++ " = \n" ++ body


argType : Arg -> Type
argType (Arg type' name) =
    type'


argName : Arg -> Name
argName (Arg type' name) =
    name


type alias Initial =
    String


type alias Piped =
    String


pipeline : Initial -> List Piped -> String
pipeline init items =
    items
        |> List.map ((++) "\n  |> ")
        |> String.concat
        |> (++) init