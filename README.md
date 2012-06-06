## etest

ETest is a small Erlang testing framework, work in progress.



## Installation

Install etest by adding it as a [rebar](https://github.com/basho/rebar)
dependency:

```erlang
% In your rebar.config:
{deps, [
    {etest, ".*", {git, "git://github.com/wooga/etest.git"}}
]}.
```

Then run `rebar get-deps` to sync your dependencies.
