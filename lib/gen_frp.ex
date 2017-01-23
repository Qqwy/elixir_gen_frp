defmodule GenFRP do
  @moduledoc """
  The main GenFRP module.

  GenFRP wraps GenServer, which means
  that a separate process is spawned using `start` or start_link`.
  After this, the other functions in this module
  can be used to send messages to this process.

  The idea is to:

  - start a GenFRP process with your desired GenFRP behaviour.
  - Send events using `send_event` to this process, which will update its state.
  - At some point, `render` the state the process has during that time.

  The results of `render` are cached, to restrict the amount of work that is done.

  Instead of sending events manually, `register_callback` can be used to
  register _callbacks_ that might send an event whenever they are triggered.
  A callback is basically a wrapper with some setup and some tear-down code.
  """


  alias GenFRP.Callback

  defstruct [:module, :state, callbacks: MapSet.new, last_rendered_state: nil, last_render: nil]

  defmacro __using__(opts) do
    quote do
      use GenFRP.Behaviour, unquote(opts)
    end
  end

  use GenServer

  # EXTERNAL

  @doc """
  Starts the given `frp_module` in a as a GenFRP process.

  It returns the PID of the started process, which is to be used
  for most of the other methods in this module.
  """
  def start(frp_module) do
    start(frp_module, frp_module.initial_state())
  end

  def start(frp_module, initial_state) do
    GenServer.start(__MODULE__, %__MODULE__{module: frp_module, state: initial_state, callbacks: %{}})
  end

  @doc """
  Atomically starts the given `frp_module` as a GenFRP process
  and sets up a link to it; if it crashes, the process that started it will also crash.
  """
  def start_link(frp_module) do
    start(frp_module, frp_module.initial_state())
  end

  def start_link(frp_module, initial_state) do
    GenServer.start_link(__MODULE__, %__MODULE__{module: frp_module, state: initial_state, callbacks: %{}})
  end

  @doc """
  Adds the given `callback` to the given FRP process `pid`.

  See the GenFRP.Callback module for more information.
  """
  def register_callback(pid, callback = %Callback{}) do
    GenServer.call(pid, {:register_callback, callback})
  end

  @doc """
  Deregisters a previously-registered callback.

  To deregister a previously-registered callback, simply specify the
  exact same callback to this function (or reconstruct one from the same original parameters).
  """
  def deregister_callback(pid, callback = %Callback{}) do
    GenServer.call(pid, {:deregister_callback, callback})
  end

  @doc """
  Sends the given `event`
  (which might be anything that your FRP implementation
  expects in its implementation of the `GenFRP.Behaviour.update/2` function)
  to the FRP process `pid`.
  """
  def send_event(pid, event) do
    GenServer.cast(pid, {:send_event, event})
  end

  @doc """
  Requests to render the internal state of `pid`.

  Internally, the GenFRP.Behaviour's `render/1` function will be called
  on your FRP implementation.

  But this will only happen if the state has changed since the last call to `render`.
  If it has not, then the older, cached state will be returned.
  """
  def render(pid) do
    GenServer.call(pid, :render)
  end

  @doc """
  A simple function that just dumps the full internal state of the GenFRP process.
  Should only be used for debugging/testing purposes of the GenFRP process itself;
  might be removed in future releases.

  (Debugging your GenFRP behaviour can be done by checking the outcome of calling
  the `update/2` function directly)
  """
  def debug(pid) do
    GenServer.call(pid, :debug)
  end

  # INTERNAL
  @doc false
  def handle_call({:register_callback, callback = %Callback{}}, _from, gen_server_state = %__MODULE__{}) do
    callback_starting_state = callback.start_fun.(self())
    new_gen_server_state = %{gen_server_state | callbacks: Map.put(gen_server_state.callbacks, callback, callback_starting_state)}
    {:reply, :ok, new_gen_server_state}
  end

  def handle_call({:deregister_callback, callback}, _from, gen_server_state = %__MODULE__{}) do
    callback.stop_fun.(self(), gen_server_state.callbacks[callback])
    new_gen_server_state = Map.put(gen_server_state, :callbacks, Map.delete(gen_server_state.callbacks, callback))
    {:reply, :ok, new_gen_server_state}
  end

  # Prevent unneccesary work.
  def handle_call(:render, _from, gen_server_state = %__MODULE__{state: state, last_rendered_state: state}) do
    {:reply, gen_server_state.last_render, gen_server_state}
  end

  def handle_call(:render, _from, gen_server_state = %__MODULE__{module: module, state: state, last_rendered_state: last_rendered_state}) do
    render = module.render(state, last_rendered_state)
    new_gen_server_state = %__MODULE__{gen_server_state | last_render: render, last_rendered_state: state}
    {:reply, render, new_gen_server_state}
  end

  def handle_call(:debug, _from, gen_server_state) do
    {:reply, gen_server_state, gen_server_state}
  end

  @doc false
  def handle_cast({:send_event, event}, gen_server_state = %__MODULE__{module: module, state: state}) do
    state = module.update(state, event)
    new_gen_server_state = %__MODULE__{gen_server_state | state: state}
    {:noreply, new_gen_server_state}
  end
end
