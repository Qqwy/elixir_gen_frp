defmodule GenFRP.Example.Map do
  use GenFRP, initial_state: %{}


  def update(state, [{key, val}]) do
    Map.put(state, key, val)
  end

  def update(state, event) do
    IO.puts "Unrecognized event passed to `#{inspect(__MODULE__)}.update/2`: #{event}"
    state
  end

  def render(state, last_rendered_state) do
    rendered_state = "The current map: #{inspect(state)}"
    IO.puts(rendered_state)
    IO.puts("The diff since last time:#{inspect MapDiff.diff(last_rendered_state, state)}")
    # rendered_state
    state
  end
end
