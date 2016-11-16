module BSTSetTests exposing (..)

import BSTSet as Set
import Expect
import Fuzz
import List
import Set as NSet
import String
import Test exposing (..)


empty : Test
empty =
    test "empty" <|
        \() ->
            Expect.equal Set.empty Set.Empty


singleton : Test
singleton =
    fuzz Fuzz.int "singleton" <|
        \i ->
            Expect.equal (Set.singleton i) (Set.Tree i Set.Empty Set.Empty)


remove : Test
remove =
    describe "remove"
        [ test "empty" <|
            \_ ->
                Set.empty
                    |> Set.remove 1
                    |> Expect.equal Set.empty
        , fuzz Fuzz.int "singleton" <|
            \i ->
                Set.singleton i
                    |> Set.remove i
                    |> Expect.equal Set.empty
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
                    Set.foldr (+) 0 (Set.Tree 1 Set.Empty Set.Empty)
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
                    Set.foldl (+) 0 (Set.Tree 1 Set.Empty Set.Empty)
                        |> Expect.equal 1
            ]
        ]


insert : Test
insert =
    describe "insert"
        [ fuzz Fuzz.int "into empty" <|
            \i ->
                Set.Empty
                    |> Set.insert i
                    |> Expect.equal (Set.Tree i Set.Empty Set.Empty)
        , describe "into nonempty"
            [ fuzz Fuzz.int "when inserted is greater than head" <|
                \i ->
                    Set.singleton (i - 1)
                        |> Set.insert i
                        |> Expect.equal (Set.Tree (i - 1) Set.Empty (Set.Tree i Set.Empty Set.Empty))
            , fuzz Fuzz.int "when inserted is less than head" <|
                \i ->
                    Set.singleton (i + 1)
                        |> Set.insert i
                        |> Expect.equal (Set.Tree (i + 1) (Set.Tree i Set.Empty Set.Empty) Set.Empty)
            , fuzz Fuzz.int "when inserted is equal to head" <|
                \i ->
                    Set.singleton i
                        |> Set.insert i
                        |> Expect.equal (Set.singleton i)
            ]
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
                    |> Expect.equal Set.empty
        , fuzz Fuzz.int "does not match" <|
            \i ->
                Set.singleton i
                    |> Set.filter ((==) (i + 1))
                    |> Expect.equal (Set.singleton i)
        ]


all : Test
all =
    concat
        [ empty
        , singleton
        , remove
        , foldr
        , foldl
        , insert
        , union
        , size
        , member
        , listOps
        , filter
        ]
