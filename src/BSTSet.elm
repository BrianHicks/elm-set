module BSTSet exposing (..)


type Set comparable
    = Branch comparable (Set comparable) (Set comparable)
    | Empty



-- build


empty : Set comparable
empty =
    Empty


singleton : comparable -> Set comparable
singleton item =
    Branch item empty empty


insert : comparable -> Set comparable -> Set comparable
insert item tree =
    case tree of
        Empty ->
            Branch item empty empty

        Branch cmp left right ->
            if item < cmp then
                Branch cmp (insert item left) right
            else if item > cmp then
                Branch cmp left (insert item right)
            else
                tree



-- combine


union : Set comparable -> Set comparable -> Set comparable
union a b =
    a
        |> toList
        |> List.foldl insert b



-- querying


contains : comparable -> Set comparable -> Bool
contains item set =
    case set of
        Empty ->
            False

        Branch cmp left right ->
            if item < cmp then
                contains item left
            else if item > cmp then
                contains item right
            else
                True


size : Set comparable -> Int
size =
    let
        counter : Set comparable -> Int -> Int
        counter set current =
            case set of
                Empty ->
                    current

                Branch _ _ _ ->
                    current + 1
    in
        foldr counter 0



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

                Branch val _ _ ->
                    val :: list
    in
        foldr lister []



-- transform


foldr : (Set comparable -> a -> a) -> a -> Set comparable -> a
foldr fn dest set =
    case set of
        Empty ->
            fn set dest

        Branch _ left right ->
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
--         Branch cmp left right ->
--             if item < cmp then
--                 Branch cmp (delete item left) right
--             else if item > cmp then
--                 Branch cmp left (delete item right)
--             else
--                 combine left right
-- size : BST comparable -> Int
-- size tree =
--     case tree of
--         Leaf ->
--             0
--         Branch _ left right ->
--             1 + size left + size right
-- contains : comparable -> BST comparable -> Bool
-- contains item tree =
--     case tree of
--         Leaf ->
--             False
--         Branch cmp left right ->
--             if item < cmp then
--                 contains item left
--             else if item > cmp then
--                 contains item right
--             else
--                 True
