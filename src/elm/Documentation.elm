module Documentation exposing (htmlToDocComment, htmlToFileComment)

import Elm.CodeGen as CG exposing (Comment, DocComment, FileComment)
import Html.Parser as HP


htmlToFileComment : String -> Comment FileComment
htmlToFileComment val =
    let
        parsedHtmlResult =
            HP.run val

        empty =
            CG.emptyFileComment
    in
    case parsedHtmlResult of
        Err _ ->
            empty
                |> CG.markdown val

        Ok nodes ->
            htmlToComment nodes True [] empty
                |> Tuple.second


htmlToDocComment : String -> Comment DocComment
htmlToDocComment val =
    let
        parsedHtmlResult =
            HP.run val

        empty =
            CG.emptyDocComment
    in
    case parsedHtmlResult of
        Err _ ->
            empty
                |> CG.markdown val

        Ok nodes ->
            htmlToComment nodes True [] empty
                |> Tuple.second


htmlToComment : List HP.Node -> Bool -> List String -> Comment a -> ( List String, Comment a )
htmlToComment nodes isTop accum comment =
    case nodes of
        [] ->
            ( accum, comment )

        node :: ns ->
            let
                ( innerAccum, innerComment ) =
                    nodeToComment node isTop accum comment
            in
            htmlToComment ns isTop innerAccum innerComment


nodeToComment : HP.Node -> Bool -> List String -> Comment a -> ( List String, Comment a )
nodeToComment node isTop accum comment =
    case node of
        HP.Text text ->
            ( text :: accum, comment )

        HP.Element el attr children ->
            let
                ( innerAccum, innerComment ) =
                    htmlToComment children False accum comment

                ( taggedAccum, taggedInnerComment ) =
                    case ( el, innerAccum ) of
                        -- ( "p", _ ) ->
                        --     ( [], CG.markdown (List.reverse innerAccum |> String.join "") innerComment )
                        ( "fullname", hd :: tl ) ->
                            ( ("## " ++ hd) :: tl, innerComment )

                        -- ( "ul", _ ) ->
                        --     ( [], CG.markdown (List.reverse innerAccum |> String.join "") innerComment )
                        ( "li", _ ) ->
                            let
                                _ =
                                    Debug.log "innerAccum" innerAccum
                            in
                            ( [ "\n - " ++ (List.reverse innerAccum |> String.join "") ]
                            , innerComment
                            )

                        ( "code", hd :: tl ) ->
                            ( ("`" ++ hd ++ "`") :: tl, innerComment )

                        ( "a", hd :: tl ) ->
                            ( ("`" ++ hd ++ "`") :: tl, innerComment )

                        _ ->
                            ( innerAccum, innerComment )
            in
            if isTop then
                ( [], CG.markdown (List.reverse taggedAccum |> String.join "") taggedInnerComment )

            else
                ( taggedAccum, taggedInnerComment )

        HP.Comment _ ->
            ( accum, comment )
