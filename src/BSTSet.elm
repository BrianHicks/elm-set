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


remove : comparable -> Set comparable -> Set comparable
remove item set =
    case set of
        Empty ->
            set

        Tree head left right ->
            if item < head then
                Tree head (remove item left) right
            else if item > head then
                Tree head left (remove item right)
            else
                union left right



-- combine


union : Set comparable -> Set comparable -> Set comparable
union =
    foldl insert


intersect : Set comparable -> Set comparable -> Set comparable
intersect a b =
    filter ((flip member) a) b


diff : Set comparable -> Set comparable -> Set comparable
diff a b =
    filter (not << (flip member) a) b



-- querying


member : comparable -> Set comparable -> Bool
member item =
    foldr (\candidate acc -> acc || (candidate == item)) False


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

        Tree head left right ->
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
        (\head acc ->
            if cmp head then
                acc
            else
                remove head acc
        )
        set
        set
