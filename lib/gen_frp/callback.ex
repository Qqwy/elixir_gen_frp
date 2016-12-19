defmodule GenFRP.Callback do
  defstruct start_fun: &GenFRP.Helper.void/0, stop_fun: &GenFRP.Helper.void/0
end
