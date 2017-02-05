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
    Tree
        (max (height left) (height right) |> (+) 1)
        head
        left
        right


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


remove : comparable -> Set comparable -> Set comparable
remove item set =
    case set of
        Empty ->
            set

        Tree _ head left right ->
            if item < head then
                tree head (remove item left) right |> balance
            else if item > head then
                tree head left (remove item right) |> balance
            else
                union left right



-- combine


union : Set comparable -> Set comparable -> Set comparable
union =
    foldl insert


intersect : Set comparable -> Set comparable -> Set comparable
intersect a b =
    filter (\item -> member item a) b


diff : Set comparable -> Set comparable -> Set comparable
diff a b =
    filter (\item -> not <| member item b) a



-- querying


member : comparable -> Set comparable -> Bool
member item set =
    case set of
        Empty ->
            False

        Tree _ head left right ->
            if item < head then
                member item left
            else if item > head then
                member item right
            else
                True


size : Set comparable -> Int
size =
    foldl (\_ acc -> acc + 1) 0


height : Set comparable -> Int
height set =
    case set of
        Empty ->
            0

        Tree height _ _ _ ->
            height


fromList : List comparable -> Set comparable
fromList items =
    List.foldl insert empty items


toList : Set comparable -> List comparable
toList set =
    case set of
        Empty ->
            []

        Tree _ head left right ->
            toList left ++ [ head ] ++ toList right



-- transform


map : (comparable -> comparable2) -> Set comparable -> Set comparable2
map fn set =
    foldl
        (\item acc -> insert (fn item) acc)
        empty
        set


foldr : (comparable -> a -> a) -> a -> Set comparable -> a
foldr fn acc set =
    case set of
        Empty ->
            acc

        Tree _ head left right ->
            let
                accRight =
                    foldr fn acc right

                accHead =
                    fn head accRight

                accLeft =
                    foldr fn accHead left
            in
                accLeft


foldl : (comparable -> a -> a) -> a -> Set comparable -> a
foldl fn acc set =
    case set of
        Empty ->
            acc

        Tree _ head left right ->
            let
                accLeft =
                    foldl fn acc left

                accHead =
                    fn head accLeft

                accRight =
                    foldl fn accHead right
            in
                accRight


filter : (comparable -> Bool) -> Set comparable -> Set comparable
filter cmp set =
    foldl
        (\item acc ->
            if cmp item then
                insert item acc
            else
                acc
        )
        empty
        set


partition : (comparable -> Bool) -> Set comparable -> ( Set comparable, Set comparable )
partition cmp set =
    foldl
        (\item ( yes, no ) ->
            if cmp item then
                ( insert item yes, no )
            else
                ( yes, insert item no )
        )
        ( empty, empty )
        set



-- rebalancing


balance : Set comparable -> Set comparable
balance set =
    case set of
        Empty ->
            set

        Tree _ head left right ->
            if heightDiff set == -2 && heightDiff left == 1 then
                -- left leaning tree with right-leaning left subtree. Rotate left, then right.
                tree head (rotl left) right |> rotr
            else if heightDiff set < -1 then
                -- left leaning tree, generally. Rotate right.
                rotr set
            else if heightDiff set == 2 && heightDiff right == -1 then
                -- right leaning tree with left-leaning right subtree. Rotate right, then left.
                tree head left (rotr right) |> rotl
            else if heightDiff set > 1 then
                -- right leaning tree, generally. Rotate left.
                rotl set
            else
                -- diff is -1, 0, or 1. Already balanced, no operation required.
                set


balanceOnly2 : Set comparable -> Set comparable
balanceOnly2 set =
    case set of
        Empty ->
            set

        Tree _ head left right ->
            if heightDiff set < -1 then
                rotr set
            else if heightDiff set > 1 then
                rotl set
            else
                set


heightDiff : Set comparable -> Int
heightDiff set =
    case set of
        Empty ->
            0

        Tree _ _ left right ->
            height right - height left


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
