defmodule GenFRP.Callback do
  require BlockTimer

  defstruct start_fun: &GenFRP.Helper.void/1, stop_fun: &GenFRP.Helper.void/2, starting_state: nil


  def tick do
    %__MODULE__{
      start_fun:
      fn frp_pid ->
        {:ok, timer_ref} = BlockTimer.apply_interval(BlockTimer.seconds(1), do: GenFRP.send_event(frp_pid, baz: DateTime.utc_now()))
        timer_ref
      end,
      stop_fun:
      fn frp_pid, timer_ref->
        :timer.cancel(timer_ref)
      end

    }
  end
end
