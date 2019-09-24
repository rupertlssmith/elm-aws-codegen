module Templates.Api exposing (GenModel, coreServiceMod, docs, example, globalService, module_, regionalService, service, serviceFile)

import Dict exposing (Dict)
import Elm.CodeGen exposing (..)
import LevelOne exposing (..)
import Templates.L1 exposing (..)


type alias GenModel =
    { name : List String
    , isRegional : Bool
    , endpointPrefix : String
    , apiVersion : String
    , protocol : String
    , signer : String
    , docs : String
    , declarations : Declarations
    , imports : List ()
    , operations : List ()
    }


example : GenModel
example =
    { name = [ "Some", "Module" ]
    , isRegional = True
    , endpointPrefix = "dynamodb"
    , apiVersion = "2012-08-10"
    , protocol = "json"
    , signer = "signV4"
    , docs = ""
    , declarations =
        Dict.fromList
            [ ( "record", exampleRecord )
            , ( "custom", exampleCustom )
            ]
    , imports = []
    , operations = []
    }


exampleRecord : Declarable
exampleRecord =
    TProduct
        [ ( "a", TBasic BInt )
        , ( "b", TBasic BBool )
        , ( "c", TBasic BReal )
        , ( "d", TBasic BString )
        , ( "e", TBasic BString |> CList |> TContainer )
        , ( "f", TBasic BString |> CSet |> TContainer )
        , ( "g", CDict (TBasic BString) (TBasic BString) |> TContainer )
        , ( "h", TBasic BString |> COptional |> TContainer )
        ]
        |> DAlias


exampleCustom : Declarable
exampleCustom =
    DSum
        [ ( "a", [ ( "val", TBasic BInt ) ] )
        , ( "b", [ ( "val", TBasic BBool ) ] )
        , ( "c", [ ( "val", TBasic BReal ) ] )
        , ( "d", [ ( "val", TBasic BString ) ] )
        , ( "e", [ ( "val", TBasic BString |> CList |> TContainer ) ] )
        , ( "f", [ ( "val", TBasic BString |> CSet |> TContainer ) ] )
        , ( "g", [ ( "vals", CDict (TBasic BString) (TBasic BString) |> TContainer ) ] )
        , ( "h", [ ( "maybeVal", TBasic BString |> COptional |> TContainer ) ] )
        ]


serviceFile : GenModel -> File
serviceFile model =
    let
        ( serviceFn, linkage ) =
            service model

        ( types, linkage2 ) =
            typeDeclarations model

        ( codecs, linkage3 ) =
            jsonCodecs model

        declarations =
            codecs
                |> List.append types
                |> (::) serviceFn

        linkages =
            linkage3
                |> List.append linkage2
                |> (::) linkage

        ( imports, exposings ) =
            combineLinkage linkages

        moduleSpec =
            module_ model exposings
    in
    file moduleSpec imports declarations []


coreServiceMod : List String
coreServiceMod =
    [ "AWS", "Core", "Service" ]



--== Module Specification (with exposing).


module_ : GenModel -> List TopLevelExpose -> Module
module_ model exposings =
    normalModule model.name exposings



--== Module Documentation
-- {-| {{= it.documentation }}
--
-- @docs service
--
-- ## Table of Contents
--
-- * [Operations](#operations){{~ it.categories :c }}
-- * [{{= c.name }}](#{{= c.name.toLowerCase() }}){{~}}
--
-- ## Operations
--
-- {{~ it.operationNames :name }}* [{{= name }}](#{{= name }})
-- {{~}}
--
-- @docs {{= it.operationNames.join(',') }}
-- {{~ it.categories :c }}
-- ## {{= c.name }}
--
-- {{~ c.types.filter(t => t.exposeAs).map(t => t.type) :t }}* [{{= t }}](#{{= t }})
-- {{~}}
--
-- @docs {{= c.types.filter(t => t.exposeAs).map(t => t.type).join(',') }}
-- {{~}}
-- -}


docs =
    ""



--== Service Definition


service : GenModel -> ( Declaration, Linkage )
service model =
    if model.isRegional then
        regionalService model

    else
        globalService model


regionalService : GenModel -> ( Declaration, Linkage )
regionalService model =
    let
        sig =
            signature "service"
                (funAnn
                    (fqTyped coreServiceMod "Region" [])
                    (fqTyped coreServiceMod "Service" [])
                )

        impl =
            apply
                [ fqFun coreServiceMod "defineRegional"
                , string model.endpointPrefix
                , string model.apiVersion
                , fqFun coreServiceMod model.protocol
                , fqFun coreServiceMod model.signer
                ]
    in
    ( funDecl
        (Just "{-| Configuration for this service. -}")
        (Just sig)
        "service"
        []
        impl
    , emptyLinkage
        |> addImport (importStmt coreServiceMod Nothing Nothing)
        |> addExposing (funExpose "service")
    )


globalService : GenModel -> ( Declaration, Linkage )
globalService model =
    let
        sig =
            signature "service"
                (fqTyped coreServiceMod "Service" [])

        impl =
            apply
                [ fqFun coreServiceMod "defineGlobal"
                , string model.endpointPrefix
                , string model.apiVersion
                , fqFun coreServiceMod model.protocol
                , fqFun coreServiceMod model.signer
                ]
    in
    ( funDecl
        (Just "{-| Configuration for this service. -}")
        (Just sig)
        "service"
        []
        impl
    , emptyLinkage
        |> addImport (importStmt coreServiceMod Nothing Nothing)
        |> addExposing (funExpose "service")
    )



--== Types and Codecs
-- {{~ it.types.filter(t => t.exposeAs) :t }}
-- {{= t.typeDef }}
--
-- {{? t.decoderDef }}
-- {{= t.decoderDef }}
-- {{?}}
--
-- {{? t.toStringDef }}
-- {{= t.toStringDef }}
-- {{?}}
-- {{~}}
--
-- {{~ it.types.filter(t => t.category === 'request') :t }}
-- {{= t.typeDef }}
-- {{~}}


typeDeclarations : GenModel -> ( List Declaration, List Linkage )
typeDeclarations model =
    Dict.foldl
        (\name decl accum -> Templates.L1.typeDecl name decl :: accum)
        []
        model.declarations
        |> List.unzip


jsonCodecs : GenModel -> ( List Declaration, List Linkage )
jsonCodecs model =
    Dict.foldl
        (\name decl accum -> Templates.L1.codec name decl :: accum)
        []
        model.declarations
        |> List.unzip



--== Operations
--
-- {{~ it.types.filter(t => t.exposeAs || t.category === 'request') :t }}
-- {{? it.metadata.protocol === 'json' && t.jsonEncoderDef }}
-- {{= t.jsonEncoderDef }}
-- {{?}}
--
-- {{? it.metadata.protocol === 'query' && t.queryEncoderDef }}
-- {{= t.queryEncoderDef }}
-- {{?}}
-- {{~}}
