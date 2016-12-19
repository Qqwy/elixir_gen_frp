defmodule GenFRP.Example.Map do
  use GenFRP, initial_state: %{}


  def update(state, [{key, val}]) do
    Map.put(state, key, val)
  end

  def render(state) do
    rendered_state = "The current map: #{inspect(state)}"
    IO.puts(rendered_state)
    rendered_state
  end
end

