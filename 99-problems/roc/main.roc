app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    imports [pf.Stdout]
    provides [main, lastA, lastB, lastC, lastD, penultimate, nth, length] to pf

# echo ./main.roc | entr -c roc test --dev

# Roc doesn't have maybe by default so we define it.
Maybe a : [Just a, Nothing]

###############################################################################

# https://ocaml.org/exercises#1
# Write a function last : 'a list -> 'a option that returns the last element of a list

lastA : List a -> Maybe a
lastA = \lst ->
    when lst is
        [] -> Nothing
        [x] -> Just x
        [_, .. as xs] -> lastA xs

expect lastA [1, 2, 3] == Just 3
expect lastA [1, 2] == Just 2
expect lastA [1] == Just 1
expect lastA [] == Nothing

# Use this syntax to inspect a failure
# expect
#     actual = last [1, 2, 3]
#     actual == Just 4

#####

# Alternatively, we can return unit (`{}`)
lastB : List a -> Result a {}
lastB = \lst ->
    when lst is
        [] -> Err {}
        [x] -> Ok x
        [_, .. as xs] -> lastB xs

expect lastB [1, 2, 3] == Ok 3
expect lastB [1, 2] == Ok 2
expect lastB [1] == Ok 1
expect lastB [] == Err {}

#####

# Alternatively, I could use some "loose" types on the fly.
# This isn't very type-safe since it doesn't constraint what the function can output

lastC : List a -> [None, Some a]
lastC = \lst ->
    when lst is
        [] -> None
        [x] -> Some x
        [_, .. as xs] -> lastC xs

expect lastC [1, 2, 3] == Some 3
expect lastC [1, 2] == Some 2
expect lastC [1] == Some 1
# expect lastC [] == Err "empty" # This compiles!! (but generates a runtime failed assertion)
expect lastC [] == None

# "Roc does not use linked list but arrays/vectors. So you don't need a recursion."
# Mmm so that means using recursion doesn't make much sens + it seems like we can't construct a cons list like 1 :: 2 :: 3 []
# Which limits what we can do with recursive algorithms.

expect lastD [1, 2, 3] == Ok 3
expect lastD [1, 2] == Ok 2
expect lastD [1] == Ok 1
# expect lastD [] == Err "empty" # This compiles!! (but generates a runtime failed assertion)
expect
    actual = Err OutOfBounds
    actual == lastD []

lastD : List a -> Result a [OutOfBounds]
lastD = \lst ->
    List.get lst (Num.max 1 (List.len lst) - 1) # That's not very nice. Removing `Num.max` compiles and generates a runtime error get (-1)

###############################################################################

# https://ocaml.org/exercises#2
# Find the last but one (last and penultimate) elements of a list.

expect penultimate [1, 2, 3] == Just 2
expect penultimate [1, 2] == Just 1
expect penultimate [1] == Nothing
expect penultimate [] == Nothing

penultimate = \lst ->
    when lst is
        [] -> Nothing
        [x, _] -> Just x
        [_, .. as xs] -> penultimate xs

###############################################################################

# https://ocaml.org/exercises#3
# Find the N'th element of a list.

expect [] |> nth 0 == Err {}
expect [1, 2, 3] |> nth 0 == Ok 1
expect [1, 2, 3] |> nth 1 == Ok 2
expect [1, 2, 3] |> nth 2 == Ok 3
expect [1, 2, 3] |> nth 3 == Err {}

# nth : Int, List a -> Result Int {}
nth : List a, Int * -> Result a {}
nth = \lst, n ->
    when lst is
        [] -> Err {}
        [x, .. as xs] ->
            when n is
                0 -> Ok x
                _ -> nth xs (n - 1)

###############################################################################

# https://ocaml.org/exercises#4
# Find the number of elements of a list.

expect 0 == length []
expect 3 == length [1, 2, 3]

length : List * -> Num *
length = \lst ->
    aux = \n, lst2 ->
        when lst2 is
            [] -> n
            [_, .. as xs] -> aux (n + 1) xs
    aux 0 lst

###############################################################################

# https://ocaml.org/exercises#4
# Reverse a list.

# expect [] == rev []
# expect [1] == rev [1]
# expect
#     actual = [1, 2]
#     actual == rev [2, 1]
# expect [3, 2, 1] == rev [1, 2, 3]

# rev : List a -> List a
# rev = \lst ->
#     when lst is
#         [] -> []
#         [x, .. as xs] ->
#             [x, .. (rev xs)] <-- we can't build a list like so!

expect [3, 2, 1] == rev2 [1, 2, 3]
rev2 = \lst -> lst |> List.walk [] List.prepend

###############################################################################

# Conclusion: I feel not supporting linked lists by default is a big downside and quite a departure
# from traditional FP.
# I could define my own linked list like below, but without custom operators it'd be pretty clunky.
# So, with this in mind, I still fell Roc isn't for me.
# cons = \x, lst -> (Cons x, lst)
# myList = cons 1 (cons 2 (cons 3 Nil))

###############################################################################

main =
    Stdout.line "Hello, World!"
