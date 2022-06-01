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
    stock_list/0,
    create_order/2,
    test/0,
    testOrder/0,
    sold_products/0
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
    {getAlias(), getNode(getAlias())} ! {subscribe_partner, Partner, self()},
    receive
        starting ->
            io:format("Partner ~p requests subscription ~n", [Partner])
    end,
    receive
        ending ->
            io:format("Partner ~p is subscribed ~n", [Partner]);
        exists ->
            io:format("Partner ~p is taken. Choose another name~n", [Partner])
    end.

delete_partner(Partner) ->
    {getAlias(), getNode(getAlias())} ! {delete_partner, Partner, self()},
    receive
        starting ->
            io:format("Partner ~p requests unsubscribing ~n", [Partner])
    end,
    receive
        ending ->
            io:format("Partner ~p is unsubscribed ~n", [Partner]);
        dont_exist ->
            io:format("Partner ~p dont exists~n", [Partner])
    end.

list_partners() ->
    {getAlias(), getNode(getAlias())} ! {list_partners, self()},
    receive
        List -> io:fwrite("~w~n", [List])
    end.

register_product(Product, Quantity) ->
    {getAlias(), getNode(getAlias())} ! {register_product, Product, Quantity, node()}.

remove_product(Product) ->
    {getAlias(), getNode(getAlias())} ! {remove_product, Product}.

modify_stock(Product, Quantity) ->
    {getAlias(), getNode(getAlias())} ! {modify_stock, Product, Quantity}.

create_order(Partner, ProductList) ->
    {getAlias(), getNode(getAlias())} ! {create_order, Partner, ProductList, self()},
    receive
        starting -> io:format("Recieved order from partner ~p ... ~n", [Partner])
    end,
    receive
        partner_dont_exist ->
            io:format("Partner: ~p doesnt exist, try with valid one ~n", [Partner]);
        {order, OrderNumber, List} ->
            io:format("According to stock, here is what you got: ~n"),
            io:format("Order Number : ~p ~n", [OrderNumber]),
            io:format("~w~n", [List])
    end.
sold_products() ->
    {getAlias(), getNode(getAlias())} ! {sold_products, self()},
    receive
        Orders -> print_orders(Orders)
    end.

stock_list() ->
    {getAlias(), getNode(getAlias())} ! {stock_list, self()},
    receive
        List -> io:fwrite("~w~n", [List])
    end.

store(Alias) -> store(Alias, [], [], []).
store(Alias, Partners, Products, Orders) ->
    receive
        {subscribe_partner, Partner, Pid} ->
            Pid ! starting,
            AddedPreviously = lists:any(fun(X) -> X == Partner end, Partners),
            if
                % already exists partner
                AddedPreviously ->
                    io:format("Partner ~p already exist~n", [Partner]),
                    Pid ! exists,
                    store(Alias, Partners, Products, Orders);
                true ->
                    io:format("~p partner created ~n", [
                        Partner
                    ]),
                    Pid ! ending,
                    store(Alias, Partners ++ [Partner], Products, Orders)
            end;
        {delete_partner, Partner, Pid} ->
            Pid ! starting,
            AddedPreviously = lists:any(fun(X) -> X == Partner end, Partners),
            if
                AddedPreviously ->
                    io:format("Deleted partner ~p ~n", [Partner]),
                    Pid ! ending,
                    store(Alias, lists:delete(Partner, Partners), Products, Orders);
                true ->
                    io:format("Partner ~p does not exist~n", [Partner]),
                    Pid ! dont_exist,
                    store(Alias, Partners, Products, Orders)
            end;
        {register_product, Product, Quantity, Node} ->
            AddedPreviously = lists:any(fun({P, _}) -> P == Product end, Products),
            Alive = net_adm:ping(Node),
            if
                AddedPreviously ->
                    io:format("Product ~p already exists ~n", [Product]),
                    store(Alias, Partners, Products, Orders);
                true ->
                    if
                        Alive == pang ->
                            io:format("node ~p is down, please try again ~n", [Node]),
                            store(Alias, Partners, Products, Orders);
                        true ->
                            Pid = spawn(Node, ?MODULE, product, [Product, Quantity]),
                            case rpc:call(Node, erlang, is_process_alive, [Pid]) of
                                true ->
                                    io:format(
                                        "Product ~p with quantity ~p created in node ~p~n", [
                                            Product, Quantity, Node
                                        ]
                                    ),
                                    Pid ! {created},
                                    store(Alias, Partners, Products ++ [{Product, Pid}], Orders);
                                false ->
                                    io:format("node ~p does not exist~n", [Node]),
                                    store(Alias, Partners, Products, Orders)
                            end
                    end
            end;
        {remove_product, Product} ->
            AddedPreviously = lists:any(fun({P, _}) -> P == Product end, Products),
            if
                AddedPreviously ->
                    io:format("Removed product ~p ~n", [Product]),
                    Pid = get_pid(Product, Products),
                    Pid ! {remove},
                    store(Alias, Partners, delete_product_list(Product, Products), Orders);
                true ->
                    io:format("Product ~p does not exist~n", [Product]),
                    store(Alias, Partners, Products, Orders)
            end;
        {modify_stock, Product, Quantity} ->
            AddedPreviously = lists:any(fun({P, _}) -> P == Product end, Products),
            if
                AddedPreviously ->
                    io:format("Modifying stock of ~p ....~n", [Product]),
                    Pid = get_pid(Product, Products),
                    Pid ! {modify, Quantity},
                    store(Alias, Partners, Products, Orders);
                true ->
                    io:format("Product ~p does not exist~n", [Product]),
                    store(Alias, Partners, Products, Orders)
            end;
        {list_partners, Pid} ->
            io:fwrite("~w~n", [Partners]),
            Pid ! Partners,
            store(Alias, Partners, Products, Orders);
        {stock_list, Pid} ->
            % io:fwrite("~w~n", [Products]),
            MapFun = fun({Product, PidProduct}) ->
                PidProduct ! {query_stock, self()},
                receive
                    Quantity -> {Product, Quantity}
                end
            end,
            io:fwrite("~w~n", [lists:map(MapFun, Products)]),
            Pid ! lists:map(MapFun, Products),
            store(Alias, Partners, Products, Orders);
        {create_order, Partner, ProductList, Pid} ->
            Pid ! starting,
            PartnerExist = lists:any(fun(X) -> X == Partner end, Partners),
            if
                PartnerExist ->
                    % create order
                    MapFun = fun({Product, DesiredQuantity}) ->
                        ProductPid = get_pid(Product, Products),
                        if
                            (ProductPid == -1) ->
                                {Product, 0};
                            true ->
                                ProductPid ! {query_stock, self()},
                                receive
                                    Quantity ->
                                        if
                                            (Quantity - DesiredQuantity >= 0) ->
                                                ProductPid ! {modify, -DesiredQuantity},
                                                {Product, DesiredQuantity};
                                            true ->
                                                ProductPid ! {modify, -Quantity},
                                                {Product, Quantity}
                                        end
                                end
                        end
                    end,
                    Order = lists:map(MapFun, ProductList),
                    Pid ! {order, 1 + order_size(Orders), Order},
                    io:format("Order ~p created~n", [1 + order_size(Orders)]),
                    io:format("~p ~n", [Order]),
                    store(Alias, Partners, Products, Orders ++ [Order]);
                true ->
                    Pid ! partner_dont_exist,
                    io:format("Invalid partner ~p ~n", [Partner]),
                    store(Alias, Partners, Products, Orders)
            end;
        {sold_products, Pid} ->
            Pid ! Orders,
            print_orders(Orders),
            store(Alias, Partners, Products, Orders);
        {close} ->
            RemoveProduct = fun({_, Pid}) ->
                Pid ! {remove}
            end,
            lists:foreach(RemoveProduct, Products),
            io:format("Closing store ~n")
    end.

product(Product, Quantity) ->
    receive
        {created} ->
            io:format(user, "Product ~p was created with a stock of ~p ~n", [Product, Quantity]),
            product(Product, Quantity);
        {remove} ->
            io:format(user, "Product ~p was removed  ~n", [Product]);
        {modify, Q} ->
            if
                Q + Quantity >= 0 ->
                    io:format(user, "New Stock for ~p is ~p ~n", [Product, Quantity + Q]),
                    product(Product, Q + Quantity);
                true ->
                    io:format(user, "Can't modify ~p stock with ~p ~n", [Product, Quantity + Q]),
                    product(Product, Quantity)
            end;
        {query_stock, Pid} ->
            Pid ! Quantity,
            product(Product, Quantity)
    end.

print_orders(Orders) -> print_orders(Orders, 1).
print_orders([], _) ->
    done;
print_orders([X | R], N) ->
    io:format("Order #~p ~n ~p ~n", [N, X]),
    print_orders(R, N + 1).

order_size([]) -> 0;
order_size([_ | R]) -> 1 + order_size(R).

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

test() ->
    open_store(),
    subscribe_partner(luis),
    subscribe_partner(luis),
    list_partners(),
    delete_partner(yerik),
    delete_partner(luis),
    list_partners(),
    subscribe_partner(luis),
    list_partners(),
    register_product(cheese, 5),
    register_product(cheese, 12),
    modify_stock(cheese, 2),
    modify_stock(cheese, -10),
    modify_stock(eggs, 10),
    remove_product(cheese),
    remove_product(cheese),
    close_store().

testOrder() ->
    subscribe_partner(mau),
    register_product(cheese, 5),
    register_product(apple, 12),
    register_product(banana, 10),
    register_product(eggs, 20),
    stock_list(),
    create_order(mau, [{eggs, 10}, {banana, 40}]),
    create_order(mau, [{eggs, 4}, {apple, 1}, {banana, 4}, {coco, 5}]),
    sold_products().
