defmodule GenFRP.Behaviour do

  @type state :: any
  @type event :: any
  @type view :: any

  @callback update(state, event) :: state
  @callback render(state) :: view

  defmacro __using__(opts) do
    quote location: :keep do


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
      """
      def render(state), do: state

      defoverridable update: 2, render: 1
    end
  end
end
