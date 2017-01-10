defmodule GenFRP.Mixfile do
  use Mix.Project

  def project do
    [app: :gen_frp,
     version: "0.1.0",
     # build_path: "../../_build",
     # config_path: "../../config/config.exs",
     # deps_path: "../../deps",
     # lockfile: "../../mix.lock",
     elixir: "~> 1.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    # Specify extra applications you'll use from Erlang/Elixir
    [extra_applications: [:logger]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:my_dep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:my_dep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
  #
  # To depend on another app inside the umbrella:
  #
  #   {:my_app, in_umbrella: true}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      {:petick, "~> 0.0.1"},
      {:map_diff, "~> 1.0", only: [:dev, :test]},
    ]
  end


  defp description do
    """
    A library that allows for doing Functional Reactive Programming in Elixir
    """
  end

  defp package do
    [# These are the default files included in the package
      name: :postgrex,
      files: ["lib", "priv", "mix.exs", "README*", "readme*", "LICENSE*", "license*"],
      maintainers: ["Eric Meadows-Jönsson", "José Valim"],
      licenses: ["Apache 2.0"],
      links: %{"GitHub" => "https://github.com/ericmj/postgrex",
               "Docs" => "http://ericmj.github.io/postgrex/"}]
  end
end
