defmodule SSEServer.Application do
  use Application

  def start(_type, _args) do
    children = [
      SSEServer.Server
    ]

    opts = [strategy: :one_for_one, name: SSEServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
