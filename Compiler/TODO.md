
- Lab6:
    - Testing:
        - [x] Function name scoping (shadowing function names/variables)
        - [ ] Int map function, basic hofs
        - [x] If you use a function, it is defined
        - [ ] Pass function names around a lot
        - [ ] Curried functions and hofs --> return a partial application of a function
        - [x] Currently NO CLOSURES = no external variables in lambda's scope

        ¿Qué queremos con este código?
        ```
        (int) -> int x;
        x = fn (int y) -> int {
            return x (1);
        };
        ```

        - [x] Bad fn application
          - [x] Incorrect argument type
          - [x] Incorrect syntax
        - [ ] Lambdas with spilled arguments (6+ args)
        - [ ] Later: how does this affect tail call optimization?
        - [x] Make sure we can't return structs
        - [ ] Make use of the full range of C0 constructs


