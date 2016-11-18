module BSTSet exposing (..)


type Set comparable
    = Tree Int comparable (Set comparable) (Set comparable)
    | Empty



-- build


empty : Set comparable
empty =
    Empty


singleton : comparable -> Set comparable
singleton item =
    Tree 1 item empty empty


tree : comparable -> Set comparable -> Set comparable -> Set comparable
tree head left right =
    Tree (max (height left) (height right) |> (+) 1) head left right


insert : comparable -> Set comparable -> Set comparable
insert item set =
    case set of
        Empty ->
            singleton item

        Tree _ head left right ->
            if item < head then
                tree head (insert item left) right |> balance
            else if item > head then
                tree head left (insert item right) |> balance
            else
                set


naiveInsert : comparable -> Set comparable -> Set comparable
naiveInsert item set =
    case set of
        Empty ->
            singleton item

        Tree _ head left right ->
            if item < head then
                tree head (naiveInsert item left) right
            else if item > head then
                tree head left (naiveInsert item right)
            else
                set



-- remove : comparable -> Set comparable -> Set comparable
-- remove item set =
--     case set of
--         Empty ->
--             set
--         Tree head left right ->
--             if item < head then
--                 Tree head (remove item left) right
--             else if item > head then
--                 Tree head left (remove item right)
--             else
--                 union left right
-- -- combine
-- union : Set comparable -> Set comparable -> Set comparable
-- union =
--     foldl insert
-- intersect : Set comparable -> Set comparable -> Set comparable
-- intersect a b =
--     filter ((flip member) a) b
-- diff : Set comparable -> Set comparable -> Set comparable
-- diff a b =
--     filter (not << (flip member) a) b
-- querying
-- member : comparable -> Set comparable -> Bool
-- member item =
--     foldr (\candidate acc -> acc || (candidate == item)) False


size : Set comparable -> Int
size set =
    case set of
        Empty ->
            0

        Tree _ _ left right ->
            1 + size left + size right


height : Set comparable -> Int
height set =
    case set of
        Empty ->
            0

        Tree height _ _ _ ->
            height



-- fromList : List comparable -> Set comparable
-- fromList items =
--     List.foldl insert empty items
-- toList : Set comparable -> List comparable
-- toList =
--     foldr (::) []
-- -- transform
-- foldr : (comparable -> a -> a) -> a -> Set comparable -> a
-- foldr fn acc set =
--     case set of
--         Empty ->
--             acc
--         Tree head left right ->
--             let
--                 accRight =
--                     foldr fn acc right
--                 accHead =
--                     fn head accRight
--                 accLeft =
--                     foldr fn accHead left
--             in
--                 accLeft
-- foldl : (comparable -> a -> a) -> a -> Set comparable -> a
-- foldl fn acc set =
--     case set of
--         Empty ->
--             acc
--         Tree head left right ->
--             let
--                 accLeft =
--                     foldl fn acc left
--                 accHead =
--                     fn head accLeft
--                 accRight =
--                     foldl fn accHead right
--             in
--                 accRight
-- filter : (comparable -> Bool) -> Set comparable -> Set comparable
-- filter cmp set =
--     foldl
--         (\head acc ->
--             if cmp head then
--                 acc
--             else
--                 remove head acc
--         )
--         set
--         set
-- rebalancing


balance : Set comparable -> Set comparable
balance set =
    case set of
        Empty ->
            set

        Tree _ head left right ->
            let
                diff =
                    height right - height left
            in
                if diff < -2 then
                    rotr set |> balanceSubtrees
                else if diff == -2 then
                    tree head (rotl left) (balance right) |> rotr |> balanceSubtrees
                else if diff > 2 then
                    rotl set |> balanceSubtrees
                else if diff == 2 then
                    tree head (balance left) (rotr right) |> rotl |> balanceSubtrees
                else
                    set


balanceSubtrees : Set comparable -> Set comparable
balanceSubtrees set =
    case set of
        Empty ->
            set

        Tree _ head left right ->
            tree head (balance left) (balance right)


rotl : Set comparable -> Set comparable
rotl set =
    case set of
        Tree _ head lessThans (Tree _ subHead betweens greaterThans) ->
            tree subHead (tree head lessThans betweens) greaterThans

        _ ->
            set


rotr : Set comparable -> Set comparable
rotr set =
    case set of
        Tree _ head (Tree _ subHead lessThans betweens) greaterThans ->
            tree subHead lessThans (tree head betweens greaterThans)

        _ ->
            set
