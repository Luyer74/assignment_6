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
    testSubscriber/0,
    testOrder/0,
    testStock/0,
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

% sends message to close the store to master process
close_store() ->
    {getAlias(), getNode(getAlias())} ! {close}.

% given a partner, add it to the subscribers
% partner name should be unique
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

% given a partner name, remove it from subscribers
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

% get the list of parters or subscribers (unique).
list_partners() ->
    {getAlias(), getNode(getAlias())} ! {list_partners, self()},
    receive
        List -> io:fwrite("~w~n", [List])
    end.

% register a new product
% product is unique
% Quantity must be positive integeer
register_product(Product, Quantity) ->
    {getAlias(), getNode(getAlias())} ! {register_product, Product, Quantity, node()}.

% remove a product from the list
% given its unique name
remove_product(Product) ->
    {getAlias(), getNode(getAlias())} ! {remove_product, Product}.

% Add or remove product quantity
% given the product name and the
% diff quantity.
% quantity could be neg / pos
modify_stock(Product, Quantity) ->
    {getAlias(), getNode(getAlias())} ! {modify_stock, Product, Quantity}.

% A subscribed partner requests an
% order as a product list of
% tupples of pruduct and quantity
% [{Product, Quantity}]
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

% get all orders made on this store
sold_products() ->
    {getAlias(), getNode(getAlias())} ! {sold_products, self()},
    receive
        Orders -> print_orders(Orders)
    end.

% get all products and their quanities on stock
stock_list() ->
    {getAlias(), getNode(getAlias())} ! {stock_list, self()},
    receive
        List -> io:fwrite("~w~n", [List])
    end.

% master process starts with empty lists and alias
% Alias is just an atom name
% Partners is a list of unique atoms
% Products is a list of tupples of the form {Product, Pid}
% Where Product is a unique atom and Pid is a process id
% related to the product
% Orders is a list of orders
% A single order is a list of tupples {Product, Quantity}
store(Alias) -> store(Alias, [], [], []).
store(Alias, Partners, Products, Orders) ->
    receive
        {subscribe_partner, Partner, Pid} ->
            % send node message that it's being processed
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
                    % send confirmation mesage back to node
                    Pid ! ending,
                    %% append new partner
                    store(Alias, Partners ++ [Partner], Products, Orders)
            end;
        {delete_partner, Partner, Pid} ->
            % send node message that it's being processed
            Pid ! starting,
            AddedPreviously = lists:any(fun(X) -> X == Partner end, Partners),
            if
                AddedPreviously ->
                    io:format("Deleted partner ~p ~n", [Partner]),
                    % send back that partner is deleted
                    Pid ! ending,
                    % delete from list
                    store(Alias, lists:delete(Partner, Partners), Products, Orders);
                true ->
                    io:format("Partner ~p does not exist~n", [Partner]),
                    Pid ! dont_exist,
                    % send back that partner didnt exist
                    store(Alias, Partners, Products, Orders)
            end;
        {register_product, Product, Quantity, Node} ->
            AddedPreviously = lists:any(fun({P, _}) -> P == Product end, Products),
            Alive = net_adm:ping(Node),
            if
                AddedPreviously ->
                    io:format("Product ~p already exists ~n", [Product]),
                    % product is already registred
                    store(Alias, Partners, Products, Orders);
                true ->
                    if
                        Alive == pang ->
                            % the give node where the funtion was called isnt up
                            io:format("node ~p is down, please try again ~n", [Node]),
                            store(Alias, Partners, Products, Orders);
                        true ->
                            % spawn slave process of product on given node
                            Pid = spawn(Node, ?MODULE, product, [Product, Quantity]),
                            case rpc:call(Node, erlang, is_process_alive, [Pid]) of
                                true ->
                                    io:format(
                                        "Product ~p with quantity ~p created in node ~p~n", [
                                            Product, Quantity, Node
                                        ]
                                    ),
                                    Pid ! {created},
                                    % append product with pid to list
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
                    % send kill message to pid
                    Pid ! {remove},
                    % remove product
                    store(Alias, Partners, delete_product_list(Product, Products), Orders);
                true ->
                    %invalid product
                    io:format("Product ~p does not exist~n", [Product]),
                    store(Alias, Partners, Products, Orders)
            end;
        {modify_stock, Product, Quantity} ->
            AddedPreviously = lists:any(fun({P, _}) -> P == Product end, Products),
            if
                AddedPreviously ->
                    io:format("Modifying stock of ~p ....~n", [Product]),
                    % get pid of product
                    Pid = get_pid(Product, Products),
                    % attempt to modify it
                    Pid ! {modify, Quantity},
                    store(Alias, Partners, Products, Orders);
                true ->
                    %invalid product
                    io:format("Product ~p does not exist~n", [Product]),
                    store(Alias, Partners, Products, Orders)
            end;
        {list_partners, Pid} ->
            io:fwrite("~w~n", [Partners]),
            Pid ! Partners,
            % send to client the list of partners
            store(Alias, Partners, Products, Orders);
        {stock_list, Pid} ->
            MapFun = fun({Product, PidProduct}) ->
                PidProduct ! {query_stock, self()},
                receive
                    Quantity -> {Product, Quantity}
                end
            end,
            % for every product, get the quantity
            io:fwrite("~w~n", [lists:map(MapFun, Products)]),
            Pid ! lists:map(MapFun, Products),
            % send back the products list with quantities
            store(Alias, Partners, Products, Orders);
        {create_order, Partner, ProductList, Pid} ->
            % send node message that it's being processed
            Pid ! starting,
            PartnerExist = lists:any(fun(X) -> X == Partner end, Partners),
            if
                PartnerExist ->
                    % create order
                    MapFun = fun({Product, DesiredQuantity}) ->
                        ProductPid = get_pid(Product, Products),
                        if
                            % product doesn't exist, gets none
                            (ProductPid == -1) ->
                                {Product, 0};
                            true ->
                                ProductPid ! {query_stock, self()},
                                receive
                                    Quantity ->
                                        if
                                            % could take all the client wants
                                            (Quantity - DesiredQuantity >= 0) ->
                                                ProductPid ! {modify, -DesiredQuantity},
                                                {Product, DesiredQuantity};
                                            % client wants more than stock, then take all
                                            true ->
                                                ProductPid ! {modify, -Quantity},
                                                {Product, Quantity}
                                        end
                                end
                        end
                    end,
                    % for every product wanted, map to a new quantity depending on stock
                    Order = lists:map(MapFun, ProductList),
                    % send node message confirmation order
                    Pid ! {order, 1 + order_size(Orders), Order},
                    io:format("Order ~p created~n", [1 + order_size(Orders)]),
                    io:format("~p ~n", [Order]),
                    % append order to orders list
                    store(Alias, Partners, Products, Orders ++ [Order]);
                true ->
                    % send node message that partner is invalid
                    Pid ! partner_dont_exist,
                    io:format("Invalid partner ~p ~n", [Partner]),
                    store(Alias, Partners, Products, Orders)
            end;
        {sold_products, Pid} ->
            % send orders to client node
            Pid ! Orders,
            print_orders(Orders),
            store(Alias, Partners, Products, Orders);
        {close} ->
            RemoveProduct = fun({_, Pid}) ->
                Pid ! {remove}
            end,
            % kill every product process
            lists:foreach(RemoveProduct, Products),
            % closing by not calling recursibely
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

% HELPER PRIVATE FUNCTIONS

% Function to print sold products by order
print_orders(Orders) -> print_orders(Orders, 1).
print_orders([], _) ->
    done;
print_orders([X | R], N) ->
    io:format("Order #~p ~n ~p ~n", [N, X]),
    print_orders(R, N + 1).

% Function to get Orders list size
order_size([]) -> 0;
order_size([_ | R]) -> 1 + order_size(R).

% Function to delete product from list
% Basically creates a copy list without
% the to be deleted product
delete_product_list(_, []) ->
    [];
delete_product_list(P, [{PD, QD} | R]) ->
    if
        P == PD ->
            delete_product_list(P, R);
        true ->
            [{PD, QD}] ++ delete_product_list(P, R)
    end.

% Given a product, return its Pid
get_pid(_, []) ->
    -1;
get_pid(P, [{PD, PID} | R]) ->
    if
        P == PD ->
            PID;
        true ->
            get_pid(P, R)
    end.

% Subscriber Test

% STEPS

% erl -sname store
% c(ass6).
% ass6:open_store().

% On other terminal...

% erl -sname client
% c(ass6).
% ass6:testSubscriber().

% Back on store terminal...

% ass6:close_store().

testSubscriber() ->
    subscribe_partner(luis),
    subscribe_partner(mau),
    subscribe_partner(miguel),
    subscribe_partner(miguel),
    subscribe_partner(alberto),
    subscribe_partner(hector),
    list_partners(),
    delete_partner(yerik),
    subscribe_partner(miguel),
    subscribe_partner(luis),
    delete_partner(alberto),
    delete_partner(mau),
    list_partners().

% STEPS

% erl -sname store
% c(ass6).
% ass6:open_store().

% On other terminal...

% erl -sname client
% c(ass6).
% ass6:testStock().

% Back on store terminal...

% ass6:close_store().

testStock() ->
    register_product(cheese, 5),
    register_product(apple, 12),
    register_product(banana, 10),
    register_product(eggs, 20),
    stock_list(),
    remove_product(eggs),
    modify_stock(banana, 5),
    modify_stock(cheese, -2),
    stock_list().

% All Test

% STEPS

% erl -sname store
% c(ass6).
% ass6:open_store().

% On other terminal...

% erl -sname client
% c(ass6).
% ass6:testOrder().

% Back on store terminal...

% ass6:close_store().

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
