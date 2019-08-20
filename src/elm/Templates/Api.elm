module Templates.Api exposing (GenModel, coreServiceMod, docs, example, globalService, module_, regionalService, service, serviceFile)

import Dict exposing (Dict)
import ElmDSL exposing (..)
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
        [ ( "a", TBasic BInt )
        , ( "b", TBasic BBool )
        , ( "c", TBasic BReal )
        , ( "d", TBasic BString )
        , ( "e", TBasic BString |> CList |> TContainer )
        , ( "f", TBasic BString |> CSet |> TContainer )
        , ( "g", CDict (TBasic BString) (TBasic BString) |> TContainer )
        , ( "h", TBasic BString |> COptional |> TContainer )
        ]


serviceFile : GenModel -> File
serviceFile model =
    let
        ( serviceFn, linkage ) =
            service model

        ( typeDecls, linkage2 ) =
            Dict.foldl
                (\name decl accum -> Templates.L1.typeDecl name decl :: accum)
                []
                model.declarations
                |> List.unzip

        declarations =
            serviceFn :: typeDecls

        linkages =
            linkage :: linkage2

        ( imports, exposings ) =
            deDupeImportsAndExposing linkages

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


service : GenModel -> ( Declaration, ImportsAndExposing )
service model =
    if model.isRegional then
        regionalService model

    else
        globalService model


regionalService : GenModel -> ( Declaration, ImportsAndExposing )
regionalService model =
    let
        sig =
            signature "service"
                (functionTypeAnnotation
                    (typed coreServiceMod "Region" [])
                    (typed coreServiceMod "Service" [])
                )

        impl =
            application
                [ functionOrValue coreServiceMod "defineRegional"
                , literal model.endpointPrefix
                , literal model.apiVersion
                , functionOrValue coreServiceMod model.protocol
                , functionOrValue coreServiceMod model.signer
                ]
    in
    ( functionDeclaration
        (Just "{-| Configuration for this service. -}")
        (Just sig)
        "service"
        []
        impl
    , emptyImportsAndExposing
        |> addImport (import_ coreServiceMod Nothing Nothing)
        |> addExposing (functionExpose "service")
    )


globalService : GenModel -> ( Declaration, ImportsAndExposing )
globalService model =
    let
        sig =
            signature "service"
                (typed coreServiceMod "Service" [])

        impl =
            application
                [ functionOrValue coreServiceMod "defineGlobal"
                , literal model.endpointPrefix
                , literal model.apiVersion
                , functionOrValue coreServiceMod model.protocol
                , functionOrValue coreServiceMod model.signer
                ]
    in
    ( functionDeclaration
        (Just "{-| Configuration for this service. -}")
        (Just sig)
        "service"
        []
        impl
    , emptyImportsAndExposing
        |> addImport (import_ coreServiceMod Nothing Nothing)
        |> addExposing (functionExpose "service")
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


t =
    ()



-- {-| {{= it.doc }}
-- -}
-- type {{= it.type }}
--     = {{= it.type }}_{{= it.enum.join(`\n    | ${it.type}_`) }}


unionType =
    ()



-- {-| {{= it.doc }}
-- -}
-- type alias {{= it.type }} =
--     { {{= it.members.map(it.memberType).join('\n    , ') }}
--     }


recordType =
    ()



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
