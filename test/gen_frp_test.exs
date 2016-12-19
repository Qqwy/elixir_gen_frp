defmodule GenFRPTest do
  use ExUnit.Case
  doctest GenFRP

  test "the truth" do
    assert 1 + 1 == 2
  end

  test "Simple example" do
    {:ok, pid} = GenFRP.start_link(GenFRP.Example.Map)
    GenFRP.send_event(pid, ok: "1")
    GenFRP.send_event(pid, fail: "2")
    assert GenFRP.render(pid) == "The current map: %{fail: \"2\", ok: \"1\"}"
  end
end
