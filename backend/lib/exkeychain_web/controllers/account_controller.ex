defmodule ExkeychainWeb.AccountController do
  use ExkeychainWeb, :controller

  require Logger

  def entrypoint(conn, _params) do
    if :kc_server.is_loaded() do
      redirect(conn, to: "/accounts/")
    else
      redirect(conn, to: Routes.account_path(conn, :load) )
    end

  end

  def form(conn, _params) do
    render(conn, "load.html")
  end

  def load(conn, %{ "file" => file, "pwd" => pwd } = params) do
    Logger.info("params: #{ inspect params }")
    with :ok <- :kc_server.load( file, pwd),
    #do: redirect(conn, to: Routes.account_path(conn, :index) )
    do: render(conn, :authentication)
  end

  def index(conn, _params) do
    accounts = get_accounts([])
    render(conn, :index, accounts: accounts )
  end

  def show(conn, %{ "id" => id }) do
    {:account, a} = id |> String.to_integer() |> :kc_server.get()
    render(conn, :show, account: a)
  end

  def new(conn, _params) do
    # used to get a new, unsaved, account to edit and then
    # submit to creare
    {:account, a} = :kc_server.new_account()
    render(conn, :show, account: a)
  end

  def create(conn, params) do
    # used to submit a newly created account
    Logger.info("#{ __MODULE__ }#{  elem(__ENV__.function,0) }")
    params = %{params|"id" => String.to_integer(params["id"])}
    params = for { key, val} <- params, into: %{}, do: {String.to_atom(key), val}
    :kc_server.put({:account, params})
    json(conn, %{ id: params.id })
  end

  def update(conn, params) do
    Logger.info("#{ __MODULE__ }#{  elem(__ENV__.function,0) }")
    params = %{params|"id" => String.to_integer(params["id"])}
    params = for { key, val} <- params, into: %{}, do: {String.to_atom(key), val}
    :kc_server.put({:account, params})
    json(conn, %{ id: params.id })
  end

  def delete(conn, %{ "id" => id }) do
    id |> String.to_integer() |> :kc_server.delete()
    Logger.info("#{ __MODULE__ }#{  elem(__ENV__.function,0) }")
    json(conn, %{ id: id })
  end

  @type account :: [%{}]
  @spec get_accounts([account] | []) :: [account | []]
  defp get_accounts([]) do
    case :kc_server.first() do
      {:account, a} -> get_accounts([a])
      _ -> []
    end
  end

  defp get_accounts(acc) do
    case :kc_server.next() do
      {:account, a} -> get_accounts([a|acc])
      _ -> acc
    end
  end
end
