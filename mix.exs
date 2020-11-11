defmodule RPC.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :rpc,
      version: "3.11.1",
      elixir: "~> 1.9",
      description: "RPC Remote Procedure Call",
      package: package(),
      deps: deps()
    ]
  end

  def package do
    [
      files: ~w(include src mix.exs LICENSE),
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
