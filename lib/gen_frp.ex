defmodule GenFRP do
  alias GenFRP.Callback

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
    GenServer.start(__MODULE__, %{module: frp_module, state: initial_state, callbacks: MapSet.new})
  end


  def start_link(frp_module) do
    start(frp_module, frp_module.initial_state())
  end

  def start_link(frp_module, initial_state) do
    GenServer.start_link(__MODULE__, %{module: frp_module, state: initial_state, callbacks: MapSet.new})
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

  # INTERNAL

  def handle_call({:register_callback, callback = %Callback{}}, _from, gen_server_state) do
    new_gen_server_state = %{gen_server_state | callbacks: MapSet.put(gen_server_state.callbacks, callback)}
    callback.start_fun.()
    {:reply, :ok, new_gen_server_state}
  end

  def handle_call({:deregister_callback, callback}, _from, gen_server_state) do
    new_gen_server_state = Map.put(gen_server_state, :callbacks, MapSet.delete(gen_server_state.callbacks, callback))
    callback.stop_fun.()
    {:reply, :ok, new_gen_server_state}
  end

  def handle_call(:render, _from, gen_server_state = %{module: module, state: state}) do
    {:reply, module.render(state), gen_server_state}
  end

  def handle_cast({:send_event, event}, gen_server_state = %{module: module, state: state, callbacks: _callbacks}) do
    IO.puts "Received event: #{inspect(event)}, #{inspect(gen_server_state)}"
    IO.puts "Calling update"
    state = module.update(state, event)
    {:noreply, Map.put(gen_server_state, :state, state)}
  end
end
