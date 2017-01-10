# GenFRP

GenFRP is a library that allows for programming in elixir using the 
Functional Reactive Programming method.

This is a way to store and dispatch information that is very readable and extensible.

The idea is as follows:

1. Your FRP module contains `use GenFRP, initial_state: some_useful_starting_state`. (`some_useful_starting_state` can be anything you wish, of course)
2. It implements the `update(state, event) :: state` function.
3. It implements the `render(state, last_rendered_state) :: any` function.

`update` is called whenever an event is sent to the GenFRP process by using `GenFRP.send_event(frp_pid, whatever_you_want_to_send)`.

`render` is called whenever you want to know its current state (in some representation format that is useful for the outside world, i.e. rendered).
GenFRP uses a very simple caching mechanism to ensure that `render` is only invoked when the internal state has changed since its last invocation.


Here is a very simple example (which can be found as `GenFRP.Example.Map` as well)


```elixir
defmodule GenFRP.Example.Map do
  use GenFRP, initial_state: %{}


  def update(state, [{key, val}]) do
    Map.put(state, key, val)
  end

  def update(state, event) do
    IO.puts "Unrecognized event passed to `#{inspect(__MODULE__)}.update/2`: #{event}"
  end
  
  @doc """
  Returns the key-value pairs as strings, separated over multiple lines, in alphabetical order.
  """
  def render(state, last_rendered_state) do
  Enum.map(state, fn {key, val} ->
      "`#{key}`: `#{val}`"
    end)
  end
  Enum.join("\n")
end

```

This can be used as follows:

```
{:ok, pid} = GenFRP.start_link(GenFRP.Example.Map)
# ... maybe some other code here...
GenFRP.send_event(pid, {:foo, :bar})
# ... maybe some more code here...
GenFRP.send_event(pid, {:baz, 42})
# ... maybe yet some other code here...
GenFRP.render(pid)
"`foo`: `bar`
``baz`: `42`
"
```

## Installation

The package can be installed
by adding `gen_frp` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:gen_frp, "~> 0.1.0"}]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/gen_frp](https://hexdocs.pm/gen_frp).

