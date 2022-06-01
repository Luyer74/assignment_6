%% Assignment 6
%% Mauricio, Luis & David

-module(ass6).
-export([
    open_store/0,
    store/1,
    product/2,
    subscribe_partner/1,
    list_partners/0,
    delete_partner/1,
    register_product/2,
    remove_product/1,
    modify_stock/2,
    close_store/0,
    test/0
]).

% generic name "store"
getAlias() -> store.

% transforms an atom name to a short machine name
getNode(Name) ->
    list_to_atom(
        atom_to_list(Name) ++ "@Luiss-MacBook-Air-2"
    ).

% Creates and starts the master process of the store,
%  as well asregistering it with the alias name (an atom).
open_store() ->
    Alias = getAlias(),
    register(Alias, spawn(?MODULE, store, [Alias])),
    io:format("master ~p created~n", [Alias]).

close_store() ->
    {getAlias(), getNode(getAlias())} ! {close}.

subscribe_partner(Partner) ->
    {getAlias(), getNode(getAlias())} ! {subscribe_partner, Partner}.

delete_partner(Partner) ->
    {getAlias(), getNode(getAlias())} ! {delete_partner, Partner}.

list_partners() ->
    {getAlias(), getNode(getAlias())} ! {list_partners}.

register_product(Product, Quantity) ->
    {getAlias(), getNode(getAlias())} ! {register_product, Product, Quantity}.

remove_product(Product) ->
    {getAlias(), getNode(getAlias())} ! {remove_product, Product}.

modify_stock(Product, Quantity) ->
    {getAlias(), getNode(getAlias())} ! {modify_stock, Product, Quantity}.

store(Alias) -> store(Alias, [], []).
store(Alias, Partners, Products) ->
    receive
        {subscribe_partner, Partner} ->
            AddedPreviously = lists:any(fun(X) -> X == Partner end, Partners),
            if
                % already exists partner
                AddedPreviously ->
                    io:format("Partner ~p already exist~n", [Partner]),
                    store(Alias, Partners, Products);
                true ->
                    io:format("~p partner created ~n", [
                        Partner
                    ]),
                    store(Alias, Partners ++ [Partner], Products)
            end;
        {delete_partner, Partner} ->
            AddedPreviously = lists:any(fun(X) -> X == Partner end, Partners),
            if
                AddedPreviously ->
                    io:format("Deleted partner ~p ~n", [Partner]),
                    store(Alias, lists:delete(Partner, Partners), Products);
                true ->
                    io:format("Partner ~p does not exist~n", [Partner]),
                    store(Alias, Partners, Products)
            end;
        {register_product, Product, Quantity} ->
            AddedPreviously = lists:any(fun({P, _}) -> P == Product end, Products),
            Alive = net_adm:ping(getNode(Product)),
            if
                Alive == pang ->
                    io:format("node ~p is down~n", [getNode(Product)]),
                    store(Alias, Partners, Products);
                true ->
                    if
                        AddedPreviously ->
                            io:format("Product ~p already exists ~n", [Product]),
                            store(Alias, Partners, Products);
                        true ->
                            Pid = spawn(getNode(Product), ?MODULE, product, [Product, Quantity]),
                            case rpc:call(getNode(Product), erlang, is_process_alive, [Pid]) of
                                true ->
                                    io:format(
                                        "~p product slave with quantity ~p created in node ~p~n", [
                                            Product, Quantity, getNode(Product)
                                        ]
                                    ),
                                    store(Alias, Partners, Products ++ [{Product, Pid}]);
                                false ->
                                    io:format("node ~p does not exist~n", [getNode(Product)]),
                                    store(Alias, Partners, Products)
                            end
                    end
            end;
        {remove_product, Product} ->
            AddedPreviously = lists:any(fun({P, _}) -> P == Product end, Products),
            if
                AddedPreviously ->
                    io:format("Removed product ~p ~n", [Product]),
                    Pid = get_pid(Product, Products),
                    Pid ! {delete},
                    store(Alias, Partners, delete_product_list(Product, Products));
                true ->
                    io:format("Product ~p does not exist~n", [Product]),
                    store(Alias, Partners, Products)
            end;
        {modify_stock, Product, Quantity} ->
            AddedPreviously = lists:any(fun({P, _}) -> P == Product end, Products),
            if
                AddedPreviously ->
                    io:format("Modified stock of ~p ~n", [Product]),
                    Pid = get_pid(Product, Products),
                    Pid ! {modify, Quantity},
                    store(Alias, Partners, Products);
                true ->
                    io:format("Product ~p does not exist~n", [Product]),
                    store(Alias, Partners, Products)
            end;
        {list_partners} ->
            io:fwrite("~w~n", [Partners]),
            store(Alias, Partners, Products);
        {close} ->
            RemoveProduct = fun({_, Pid}) -> Pid ! {remove} end,
            lists:foreach(RemoveProduct, Products),
            io:format("Closing store ~n")
    end.

product(Product, Quantity) ->
    receive
        {remove} ->
            io:format(user, "Product ~p was removed  ~n", [Product]);
        {modify, Q} ->
            if
                Q + Quantity >= 0 ->
                    product(Product, Q + Quantity);
                true ->
                    product(Product, Quantity)
            end
    end.

delete_product_list(_, []) ->
    [];
delete_product_list(P, [{PD, QD} | R]) ->
    if
        P == PD ->
            delete_product_list(P, R);
        true ->
            [{PD, QD}] ++ delete_product_list(P, R)
    end.

get_pid(_, []) ->
    -1;
get_pid(P, [{PD, PID} | R]) ->
    if
        P == PD ->
            PID;
        true ->
            get_pid(P, R)
    end.

test()->
    open_store(),
    subscribe_partner(luis),
    subscribe_partner(luis),
    list_partners(),
    delete_partner(yerik),
    delete_partner(luis),
    list_partners(),
    subscribe_partner(luis),
    list_partners(),
    close_store().