defmodule GenFRP.Callback.Time do
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

    %GenFRP.Callback{start_fun: start_fun, stop_fun: stop_fun}
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
