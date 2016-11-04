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
            Expect.equal (Set.singleton i) (Set.Branch i Set.Empty Set.Empty)


foldr : Test
foldr =
    describe "foldr"
        [ describe "test by counting" <|
            let
                sum : Set.Set comparable -> Int -> Int
                sum set total =
                    case set of
                        Set.Empty ->
                            total

                        Set.Branch _ _ _ ->
                            1 + total

                base =
                    Set.Empty
            in
                [ test "empty" <|
                    \() ->
                        Set.foldr sum 0 Set.Empty
                            |> Expect.equal 0
                , test "branch" <|
                    \() ->
                        Set.foldr sum 0 (Set.Branch 1 Set.Empty Set.Empty)
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
                    |> Expect.equal (Set.Branch i Set.Empty Set.Empty)
        , describe "into nonempty"
            [ fuzz Fuzz.int "when inserted is greater than head" <|
                \i ->
                    Set.singleton (i - 1)
                        |> Set.insert i
                        |> Expect.equal (Set.Branch (i - 1) Set.Empty (Set.Branch i Set.Empty Set.Empty))
            , fuzz Fuzz.int "when inserted is less than head" <|
                \i ->
                    Set.singleton (i + 1)
                        |> Set.insert i
                        |> Expect.equal (Set.Branch (i + 1) (Set.Branch i Set.Empty Set.Empty) Set.Empty)
            , fuzz Fuzz.int "when inserted is equal to head" <|
                \i ->
                    Set.singleton i
                        |> Set.insert i
                        |> Expect.equal (Set.singleton i)
            ]
        ]


size : Test
size =
    fuzz (Fuzz.list Fuzz.int) "size" <|
        \xs ->
            List.foldl Set.insert Set.empty xs
                |> Set.size
                |> Expect.equal (NSet.size <| NSet.fromList xs)


contains : Test
contains =
    describe "contains"
        [ fuzz Fuzz.int "true" <|
            \i ->
                Set.singleton i
                    |> Set.contains i
                    |> Expect.equal True
        , fuzz Fuzz.int "false" <|
            \i ->
                Set.singleton (i + 1)
                    |> Set.contains i
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


all : Test
all =
    concat
        [ empty
        , singleton
        , foldr
        , insert
        , size
        , contains
        , listOps
        ]
