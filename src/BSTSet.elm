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



-- size : Set comparable -> Int
-- size set =
--     case set of
--         Empty ->
--             0
--         Tree head left right ->
--             1 + size left + size right


size : Set comparable -> Int
size =
    foldr (\_ count -> count + 1) 0


fromList : List comparable -> Set comparable
fromList items =
    List.foldl insert empty items


toList : Set comparable -> List comparable
toList =
    foldr (::) []



-- transform


foldr : (comparable -> a -> a) -> a -> Set comparable -> a
foldr fn acc set =
    case set of
        Empty ->
            acc

        Tree head left right ->
            let
                tmpr =
                    foldr fn acc right

                tmph =
                    fn head tmpr

                tmpl =
                    foldr fn tmph left
            in
                tmpl


foldl : (comparable -> a -> a) -> a -> Set comparable -> a
foldl fn acc set =
    case set of
        Empty ->
            acc

        Tree head left right ->
            let
                tmpl =
                    foldl fn acc left

                tmph =
                    fn head tmpl

                tmpr =
                    foldl fn tmph right
            in
                tmpr



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
