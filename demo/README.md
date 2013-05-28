AMP Demo
========

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

To send questions in the erlang shell:

Client = demo:connect(1234).

demo:sum(Client, 1, 2).
demo:divide(Client, 6, 3).
