defmodule GenFRP.Callback do
  @moduledoc """
  A GenFRP.Callback is a struct that contains a starting function `start_fun`
  and a stopping function `stop_fun`.

  `start_fun` should handle setting up some periodical event
  or other way of trigger; `stop_fun` should handle removing this
  trigger later.

  - `start_fun` is executed when the callback is registered to the FRP process,
  with a single argument: the PID of the FRP process.
  - `stop_fun` is executed when the callback gets deregistered later.
  It is called with two arguments:
    1) the PID of the FRP process,
    2) the value that `start_fun` returned earlier.

  """

  defstruct start_fun: &GenFRP.Helper.void/1, stop_fun: &GenFRP.Helper.void/2

  @doc """
  Sends the specified `event` every `milliseconds`
  to the GenFRP process.

  If `function_or_event` is a (zero-arity) function, it
  will be invoked and its return value sent as event to the
  GenFRP process.
  """
  def every_interval(milliseconds, function_or_event)
  def every_interval(milliseconds, function) when is_function(function) do
    start_fun =
      fn frp_pid ->
        {:ok, timer_ref} =
          Petick.start(interval: milliseconds, callback:
            fn _ ->
              GenFRP.send_event(frp_pid, function.())
            end
          )
        timer_ref
      end

    stop_fun =
      fn frp_pid, timer_ref->
        IO.puts("Stopping fun called!")
        Petick.terminate(timer_ref)
      end

    %__MODULE__{start_fun: start_fun, stop_fun: stop_fun}
  end

  def every_interval(milliseconds, event) do
    every_interval(milliseconds, fn -> event end)
  end

  @doc """
  Sends the current UTC timestamp as DateTime struct as
  event to the given process, once every `milliseconds`.
  (Default: 1000, i.e. once every 1 second)
  """
  def tick(milliseconds \\ 1000) do
    every_interval(milliseconds, fn -> DateTime.utc_now() end)
  end
end
