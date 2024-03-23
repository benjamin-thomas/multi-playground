Playing with Gren, stopped at part 1.

Conclusions:

- Lack of a List type is a bummer
    - Array-only encourages imperative thinking rather than functional thinking
    - Access via indexes
    - No cons operator
    - Can't deconstruct and consume an array with cons (as we would with a list)
    - So must rely only on higher-order functions, no custom recursion
- Removal of tuples is quite annoying
    - makes certain constructs pretty verbose
- Not too sure about using an update/model combo for a generic app's architecture