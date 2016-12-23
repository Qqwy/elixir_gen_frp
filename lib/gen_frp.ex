defmodule GenFRP do
  alias GenFRP.Callback


  defstruct [:module, :state, callbacks: MapSet.new, last_rendered_state: nil, last_render: nil]

  defmacro __using__(opts) do
    quote do
      use GenFRP.Behaviour, unquote(opts)
    end
  end


  use GenServer

  # EXTERNAL

  def start(frp_module) do
    start(frp_module, frp_module.initial_state())
  end

  def start(frp_module, initial_state) do
    GenServer.start(__MODULE__, %__MODULE__{module: frp_module, state: initial_state, callbacks: %{}})
  end


  def start_link(frp_module) do
    start(frp_module, frp_module.initial_state())
  end

  def start_link(frp_module, initial_state) do
    GenServer.start_link(__MODULE__, %__MODULE__{module: frp_module, state: initial_state, callbacks: %{}})
  end


  def register_callback(pid, callback = %Callback{}) do
    GenServer.call(pid, {:register_callback, callback})
  end

  def deregister_callback(pid, callback = %Callback{}) do
    GenServer.call(pid, {:deregister_callback, callback})
  end

  def send_event(pid, event) do
    GenServer.cast(pid, {:send_event, event})
  end

  def render(pid) do
    GenServer.call(pid, :render)
  end

  def debug(pid) do
    GenServer.call(pid, :debug)
  end

  # INTERNAL

  def handle_call({:register_callback, callback = %Callback{}}, _from, gen_server_state = %__MODULE__{}) do
    callback_starting_state = callback.start_fun.(self())
    IO.inspect(callback_starting_state)
    new_gen_server_state = %{gen_server_state | callbacks: Map.put(gen_server_state.callbacks, callback, callback_starting_state)}
    {:reply, :ok, new_gen_server_state}
  end

  def handle_call({:deregister_callback, callback}, _from, gen_server_state = %__MODULE__{}) do
    callback.stop_fun.(self(), gen_server_state.callbacks[callback])
    new_gen_server_state = Map.put(gen_server_state, :callbacks, Map.delete(gen_server_state.callbacks, callback))
    IO.inspect(new_gen_server_state)
    {:reply, :ok, new_gen_server_state}
  end

  # Prevent unneccesary work.
  def handle_call(:render, _from, gen_server_state = %__MODULE__{state: state, last_rendered_state: state}) do
    {:reply, gen_server_state.last_render, gen_server_state}
  end

  def handle_call(:render, _from, gen_server_state = %__MODULE__{module: module, state: state, last_rendered_state: last_rendered_state}) do
    IO.puts "Rerendering..."
    render = module.render(state, last_rendered_state)
    new_gen_server_state = %__MODULE__{gen_server_state | last_render: render, last_rendered_state: state}
    {:reply, render, new_gen_server_state}
  end

  def handle_call(:debug, _from, gen_server_state) do
    {:reply, gen_server_state, gen_server_state}
  end

  def handle_cast({:send_event, event}, gen_server_state = %__MODULE__{module: module, state: state}) do
    # IO.puts "Received event: #{inspect(event)}, #{inspect(gen_server_state)}"
    # IO.puts "Calling update"
    state = module.update(state, event)
    new_gen_server_state = %__MODULE__{gen_server_state | state: state}
    {:noreply, new_gen_server_state}
  end
end
