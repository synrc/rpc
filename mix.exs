defmodule RPC.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :rpc,
      version: "0.9.0",
      elixir: "~> 1.7",
      description: "RPC Remote Procedure Call",
      package: package(),
      deps: deps()
    ]
  end

  def package do
    [
      files: ~w(src mix.exs LICENSE),
      licenses: ["ISC"],
      maintainers: ["Namdak Tonpa"],
      name: :rpc,
      links: %{"GitHub" => "https://github.com/synrc/rpc"}
    ]
  end

  def application() do
    [mod: {:bert, []}]
  end

  def deps() do
    [
      {:ex_doc, "~> 0.11", only: :dev}
    ]
  end
end
