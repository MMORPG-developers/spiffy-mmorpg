            --- Erlang ports example ---

Provides an Erlang module that can talk to a C program to call either of two
functions, foo and bar. The calls through the port are hidden so that it looks
like the caller is simply calling two Erlang functions.

This example is taken (with few changes) from
    http://www.erlang.org/doc/tutorial/c_port.html

To use (assuming you're on a UNIX system):
    At the shell:
        $ gcc -o library library.c
        $ erlc caller.erl
    Then run Erlang (`erl`) and in Erlang type:
        > caller:start("./library").
        > caller:foo(7).
        > caller:bar(7).
        > caller:stop().
    (If you have '.' on your path, you can replace the first line with simply
        > caller:start("library").
     but "./library" is more explicit and avoids the risk of name collisions.)

