%% Assignment 6
%% Mauricio, Luis & David

-module(ass6).
-export([
    open_store/0,
    store/1,
    partner/1,
    subscribe_partner/1,
    list_partners/0,
    delete_partner/1,
    register_product/2,
    remove_product/1,
    modify_stock/2,
    stock_list/0,
    stock_list/1,
    close_store/0
]).

% generic name "store"
getAlias() -> store.

% transforms an atom name to a short machine name
getNode(Name) ->
    list_to_atom(
        atom_to_list(Name) ++ "@MJI-LAPTOP"
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

% stock list for store
stock_list() ->
    {getAlias(), getNode(getAlias())} ! {stock_list}.

% stock list for partner
stock_list(Partner) ->
    {getAlias(), getNode(getAlias())} ! {stock_list, Partner}.

store(Alias) -> store(Alias, [], []).
store(Alias, Partners, Products) ->
    receive
        {subscribe_partner, Partner} ->
            Alive = net_adm:ping(getNode(Partner)),
            if
                Alive == pang ->
                    io:format("PartnerNode ~p is down~n", [getNode(Partner)]),
                    store(Alias, Partners, Products);
                true ->
                    AddedPreviously = lists:any(fun({X, _}) -> X == Partner end, Partners),
                    if
                        % already exists partner
                        AddedPreviously ->
                            io:format("Partner ~p already exist~n", [Partner]),
                            store(Alias, Partners, Products);
                        true ->
                            Pid = spawn(getNode(Partner), ?MODULE, partner, [Partner]),
                            case rpc:call(getNode(Partner), erlang, is_process_alive, [Pid]) of
                                true ->
                                    io:format("~p partner created in node ~p~n", [
                                        Partner, getNode(Partner)
                                    ]),
                                    store(Alias, Partners ++ [{Partner, Pid}], Products);
                                false ->
                                    io:format("node ~p does not exist~n", [getNode(Partner)]),
                                    store(Alias, Partners, Products)
                            end
                    end
            end;
        {delete_partner, Partner} ->
            AddedPreviously = lists:any(fun({X, _}) -> X == Partner end, Partners),
            if
                AddedPreviously ->
                    io:format("Deleted partner ~p ~n", [Partner]),
                    Pid = get_person_pid(Partner, Partners),
                    Pid ! {delete},
                    store(Alias, lists:delete(Partner, Partners), Products);
                true ->
                    io:format("Partner ~p does not exist~n", [Partner]),
                    store(Alias, Partners, Products)
            end;
        {register_product, Product, Quantity} ->
            AddedPreviously = lists:any(fun({P, _}) -> P == Product end, Products),
            if
                AddedPreviously ->
                    io:format("Product ~p already exists ~n", [Product]),
                    store(Alias, Partners, Products);
                true ->
                    io:format("Added product  ~p with quantity ~p ~n", [Product, Quantity]),
                    store(Alias, Partners, Products ++ [{Product, Quantity}])
            end;
        {remove_product, Product} ->
            AddedPreviously = lists:any(fun({P, _}) -> P == Product end, Products),
            if
                AddedPreviously ->
                    io:format("Removed product ~p ~n", [Product]),
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
                    store(Alias, Partners, modify_stock_list({Product, Quantity}, Products));
                true ->
                    io:format("Product ~p does not exist~n", [Product]),
                    store(Alias, Partners, Products)
            end;
        {stock_list, Partner} ->
            AddedPreviously = lists:any(fun({X, _}) -> X == Partner end, Partners),
            if
                AddedPreviously ->
                    Pid = get_person_pid(Partner, Partners),
                    Pid ! {stock_list, Products},
                    store(Alias, Partners, Products);
                true ->
                    io:format("Partner ~p does not exist~n", [Partner]),
                    store(Alias, Partners, Products)
            end;
        {stock_list} ->
            io:fwrite(user, "~w~n", [Products]),
            store(Alias, Partners, Products);
        {list_partners} ->
            io:fwrite("~w~n", [Partners]),
            store(Alias, Partners, Products);
        {close} ->
            io:format("Closing store ~n")
    end.

partner(Alias) ->
    receive
        {msg, Msg} ->
            io:format(user, "Partner ~p recieved ~p ~n", [Alias, Msg]),
            partner(Alias);
        {delete} ->
            io:format(user, "Partner ~p was deleted  ~n", [Alias]);
        {stock_list, Products} ->
            io:fwrite(user, "~w~n", [Products]),
            partner(Alias)
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

modify_stock_list(_, []) ->
    [];
modify_stock_list({P, Q}, [{PD, QD} | R]) ->
    if
        (P == PD) and (Q + QD >= 0) ->
            [{PD, QD + Q}] ++ modify_stock_list({P, Q}, R);
        true ->
            [{PD, QD}] ++ modify_stock_list({P, Q}, R)
    end.

get_person_pid(_, []) ->
    -1;
get_person_pid(P, [{PD, PID} | R]) ->
    if
        P == PD ->
            PID;
        true ->
            get_person_pid(P, R)
    end.
