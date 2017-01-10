defmodule GenFRP.Behaviour do

  @type state :: any
  @type event :: any
  @type view :: any

  @callback update(state, event) :: state
  @callback render(state, last_rendered_state) :: view

  defmacro __using__(opts) do
    quote location: :keep do
      @behaviour GenFRP.Behaviour


      @doc """
      The initial state that the GenFRP-module will have
      when it is started.
      """
      def initial_state(), do: unquote(opts[:initial_state])

      @doc """
      Takes the current state and 'some' event
      and returns a new state.
      """
      def update(state, _event), do: state

      @doc """
      Takes the current state and returns something useful.
      The second argument passed to `render` is the state
      that was the output of the last time `render` was called.

      This is done so implementations of `render` can, if they want,
      use sophisticated algorithms that work on the _patch_ or _diff_
      of changes between the two states,

      instead of re-creating a whole new output each time.
      """
      def render(state, last_rendered_state), do: state

      defoverridable update: 2, render: 2
    end
  end
end
