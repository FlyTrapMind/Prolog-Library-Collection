TODO
====

  [ ] `opt_arguments/3` is not steadfast.
      The following throws an exception:
      ```prolog
      opt_arguments(
        [[default(''),opt(debug),longflags([debug]),type(atom)]],
        _,
        [Dir]
      ),
     ```

