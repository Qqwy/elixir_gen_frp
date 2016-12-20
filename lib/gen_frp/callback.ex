defmodule GenFRP.Callback do
  # require BlockTimer

  defstruct start_fun: &GenFRP.Helper.void/1, stop_fun: &GenFRP.Helper.void/2


  def tick do
    %__MODULE__{
      start_fun:
      fn frp_pid ->
        # {:ok, timer_ref} = BlockTimer.apply_interval(BlockTimer.seconds(1), do: GenFRP.send_event(frp_pid, baz: DateTime.utc_now()))
        {:ok, timer_ref} = Petick.start(interval: 1000, callback: fn _ -> GenFRP.send_event(frp_pid, currentTime: DateTime.utc_now()) end)
        timer_ref
      end,
      stop_fun:
      fn frp_pid, timer_ref->
        IO.puts("Stopping fun called!")
        Petick.terminate(timer_ref)
      end
    }
  end
end
