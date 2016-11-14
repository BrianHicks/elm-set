module BSTSet exposing (..)


type Set comparable
    = Tree comparable (Set comparable) (Set comparable)
    | Empty



-- build


empty : Set comparable
empty =
    Empty


singleton : comparable -> Set comparable
singleton item =
    Tree item empty empty


insert : comparable -> Set comparable -> Set comparable
insert item set =
    case set of
        Empty ->
            singleton item

        Tree head left right ->
            if item < head then
                Tree head (insert item left) right
            else if item > head then
                Tree head left (insert item right)
            else
                set



-- combine


union : Set comparable -> Set comparable -> Set comparable
union a b =
    a
        |> toList
        |> List.foldl insert b



-- querying


member : comparable -> Set comparable -> Bool
member item set =
    case set of
        Empty ->
            False

        Tree head left right ->
            if item == head then
                True
            else if item < head then
                member item left
            else
                member item right


size : Set comparable -> Int
size set =
    case set of
        Empty ->
            0

        Tree head left right ->
            1 + size left + size right



-- size : Set comparable -> Int
-- size =
--     let
--         counter : Set comparable -> Int -> Int
--         counter set current =
--             case set of
--                 Empty ->
--                     current
--                 Tree _ _ _ ->
--                     current + 1
--     in
--         foldr counter 0
-- lists


fromList : List comparable -> Set comparable
fromList items =
    List.foldl insert empty items


toList : Set comparable -> List comparable
toList =
    let
        lister : Set comparable -> List comparable -> List comparable
        lister set list =
            case set of
                Empty ->
                    list

                Tree val _ _ ->
                    val :: list
    in
        foldr lister []



-- transform


foldr : (Set comparable -> a -> a) -> a -> Set comparable -> a
foldr fn dest set =
    case set of
        Empty ->
            fn set dest

        Tree _ left right ->
            let
                tmpr =
                    foldr fn dest right

                tmps =
                    fn set tmpr

                tmpl =
                    foldr fn tmps left
            in
                tmpl



-- delete : comparable -> BST comparable -> BST comparable
-- delete item tree =
--     case tree of
--         Leaf ->
--             tree
--         Tree cmp left right ->
--             if item < cmp then
--                 Tree cmp (delete item left) right
--             else if item > cmp then
--                 Tree cmp left (delete item right)
--             else
--                 combine left right
