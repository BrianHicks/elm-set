module BSTSetTests exposing (..)

import BSTSet as Set
import Expect
import Fuzz
import List
import Set as NSet
import Test exposing (..)


-- build


empty : Test
empty =
    test "empty" <|
        \() ->
            Expect.equal Set.empty Set.Empty


singleton : Test
singleton =
    fuzz Fuzz.int "singleton" <|
        \i ->
            Expect.equal (Set.singleton i) (Set.Tree 1 i Set.Empty Set.Empty)


insert : Test
insert =
    describe "insert"
        [ fuzz Fuzz.int "into empty" <|
            \i ->
                Set.empty
                    |> Set.insert i
                    |> Expect.equal (Set.tree i Set.Empty Set.Empty)
        , describe "into nonempty"
            [ fuzz Fuzz.int "when inserted is greater than head" <|
                \i ->
                    Set.singleton (i - 1)
                        |> Set.insert i
                        |> Expect.equal (Set.tree (i - 1) Set.Empty (Set.singleton i))
            , fuzz Fuzz.int "when inserted is less than head" <|
                \i ->
                    Set.singleton (i + 1)
                        |> Set.insert i
                        |> Expect.equal (Set.tree (i + 1) (Set.singleton i) Set.Empty)
            , fuzz Fuzz.int "when inserted is equal to head" <|
                \i ->
                    Set.singleton i
                        |> Set.insert i
                        |> Expect.equal (Set.singleton i)
            ]
        ]



-- querying


size : Test
size =
    fuzz (Fuzz.list Fuzz.int) "size" <|
        \xs ->
            List.foldl Set.insert Set.empty xs
                |> Set.size
                |> Expect.equal (NSet.size <| NSet.fromList xs)


member : Test
member =
    describe "member"
        [ fuzz Fuzz.int "true" <|
            \i ->
                Set.singleton i
                    |> Set.member i
                    |> Expect.equal True
        , fuzz Fuzz.int "false" <|
            \i ->
                Set.singleton (i + 1)
                    |> Set.member i
                    |> Expect.equal False
        ]


remove : Test
remove =
    describe "remove"
        [ test "empty" <|
            \_ ->
                Set.empty
                    |> Set.remove 1
                    |> Expect.equal Set.empty
        , test "remove stays balanced" <|
            \_ ->
                List.range 1 10
                    |> Set.fromList
                    |> Set.remove 3
                    |> Set.remove 2
                    |> Set.remove 1
                    |> Set.heightDiff
                    |> Expect.atMost 1
        , fuzz Fuzz.int "singleton" <|
            \i ->
                Set.singleton i
                    |> Set.remove i
                    |> Expect.equal Set.empty
        , fuzz (Fuzz.list Fuzz.int) "same as core" <|
            \is ->
                let
                    expected =
                        NSet.fromList is
                            |> NSet.remove (List.head is |> Maybe.withDefault 0)
                            |> NSet.toList

                    actual =
                        Set.fromList is
                            |> Set.remove (List.head is |> Maybe.withDefault 0)
                            |> Set.toList
                in
                    Expect.equal expected actual
        ]


union : Test
union =
    describe "union"
        [ fuzz Fuzz.int "same" <|
            \i ->
                Set.singleton i
                    |> Set.union (Set.singleton i)
                    |> Expect.equal (Set.singleton i)
        , fuzz (Fuzz.tuple ( Fuzz.int, Fuzz.int )) "different" <|
            \( i, j ) ->
                let
                    expectation =
                        if i == j then
                            [ i ]
                        else
                            [ i, j ] |> List.sort
                in
                    Set.singleton i
                        |> Set.union (Set.singleton j)
                        |> Set.toList
                        |> Expect.equal expectation
        ]


intersect : Test
intersect =
    fuzz (Fuzz.tuple ( Fuzz.int, Fuzz.int )) "intersect" <|
        \( i, j ) ->
            let
                intersection =
                    Set.intersect (Set.singleton i) (Set.singleton j)
            in
                if i == j then
                    Expect.equal (Set.singleton i) intersection
                else
                    Expect.equal Set.empty intersection


diff : Test
diff =
    describe "diff"
        [ fuzz (Fuzz.tuple ( Fuzz.int, Fuzz.int )) "diff" <|
            \( i, j ) ->
                let
                    difference =
                        Set.diff (Set.singleton i) (Set.singleton j)
                in
                    if i == j then
                        Expect.equal Set.empty difference
                    else
                        Expect.equal (Set.singleton i) difference
        , fuzz (Fuzz.tuple ( Fuzz.int, Fuzz.int )) "same as core" <|
            \( i, j ) ->
                let
                    ours =
                        Set.diff (Set.singleton i) (Set.singleton j)

                    stdlib =
                        NSet.diff (NSet.singleton i) (NSet.singleton j)
                in
                    Expect.equal (Set.toList ours) (NSet.toList stdlib)
        ]


listOps : Test
listOps =
    describe "fromList and toList"
        [ fuzz (Fuzz.list Fuzz.int |> Fuzz.map (NSet.fromList >> NSet.toList)) "round-trip conversion" <|
            \xs ->
                xs
                    |> Set.fromList
                    |> Set.toList
                    |> Expect.equal xs
        ]


filter : Test
filter =
    describe "filter"
        [ test "empty" <|
            \_ ->
                Set.empty
                    |> Set.filter (always False)
                    |> Expect.equal Set.empty
        , fuzz Fuzz.int "matches" <|
            \i ->
                Set.singleton i
                    |> Set.filter ((==) i)
                    |> Expect.equal (Set.singleton i)
        , fuzz Fuzz.int "does not match" <|
            \i ->
                Set.singleton i
                    |> Set.filter ((==) (i + 1))
                    |> Expect.equal Set.empty
        ]


partition : Test
partition =
    describe "partition"
        [ test "empty" <|
            \_ ->
                Set.empty
                    |> Set.partition (always False)
                    |> Expect.equal ( Set.empty, Set.empty )
        , fuzz Fuzz.int "matches" <|
            \i ->
                Set.singleton i
                    |> Set.partition ((==) i)
                    |> Expect.equal ( Set.singleton i, Set.empty )
        , fuzz Fuzz.int "does not match" <|
            \i ->
                Set.singleton i
                    |> Set.partition (always False)
                    |> Expect.equal ( Set.empty, Set.singleton i )
        ]



-- transform


map : Test
map =
    describe "map"
        [ test "increments" <|
            \() ->
                Set.singleton 1
                    |> Set.map ((+) 1)
                    |> Expect.equal (Set.singleton 2)
        , fuzz (Fuzz.list Fuzz.int) "matches core" <|
            \xs ->
                let
                    ours =
                        Set.fromList xs
                            |> Set.map ((+) 1)
                            |> Set.toList

                    stdlib =
                        NSet.fromList xs
                            |> NSet.map ((+) 1)
                            |> NSet.toList
                in
                    Expect.equal stdlib ours
        ]


foldr : Test
foldr =
    describe "foldr"
        [ describe "test by counting" <|
            [ test "empty" <|
                \() ->
                    Set.foldr (+) 0 Set.Empty
                        |> Expect.equal 0
            , test "branch" <|
                \() ->
                    Set.foldr (+) 0 (Set.tree 1 Set.Empty Set.Empty)
                        |> Expect.equal 1
            ]
        ]


foldl : Test
foldl =
    describe "foldl"
        [ describe "test by counting" <|
            [ test "empty" <|
                \() ->
                    Set.foldl (+) 0 Set.Empty
                        |> Expect.equal 0
            , test "branch" <|
                \() ->
                    Set.foldl (+) 0 (Set.tree 1 Set.Empty Set.Empty)
                        |> Expect.equal 1
            ]
        ]


balance : Test
balance =
    describe "balance"
        [ test "empty" <|
            \_ ->
                Expect.equal
                    Set.empty
                    (Set.balance Set.empty)
        , test "singleton" <|
            \_ ->
                Expect.equal
                    (Set.singleton 1)
                    (Set.balance <| Set.singleton 1)
        , test "lopsided right-facing" <|
            \_ ->
                let
                    from =
                        Set.tree 1 Set.Empty <| Set.tree 2 Set.empty <| Set.singleton 3

                    to =
                        Set.tree 2 (Set.singleton 1) (Set.singleton 3)
                in
                    from
                        |> Set.balance
                        |> Expect.equal to
        , test "lopsided left-facing" <|
            \_ ->
                let
                    from =
                        Set.tree 3 (Set.tree 2 (Set.singleton 1) Set.empty) Set.empty

                    to =
                        Set.tree 2 (Set.singleton 1) (Set.singleton 3)
                in
                    from
                        |> Set.balance
                        |> Expect.equal to
        , test "left, leaning right" <|
            \_ ->
                let
                    from =
                        Set.tree 3 (Set.tree 1 Set.empty (Set.singleton 2)) Set.empty

                    to =
                        Set.tree 2 (Set.singleton 1) (Set.singleton 3)
                in
                    from
                        |> Set.balance
                        |> Expect.equal to
        , test "right, leaning left" <|
            \_ ->
                let
                    from =
                        Set.tree 1 Set.empty (Set.tree 3 (Set.singleton 2) Set.empty)

                    to =
                        Set.tree 2 (Set.singleton 1) (Set.singleton 3)
                in
                    from
                        |> Set.balance
                        |> Expect.equal to
        ]


all : Test
all =
    concat
        [ empty
        , singleton
        , insert
        , size
        , member
        , listOps
        , foldl
        , foldr
        , balance
        , remove
        , union
        , map
        , filter
        , partition
        , intersect
        , diff
        ]
