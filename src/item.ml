open Lwt.Infix

module Item = struct
  type t = {
    id: int;
    name: string;
    price: int;
  }
  let t =
    let encode {id; name; price} = Ok (id, name, price) in
    let decode (id, name, price) = Ok {id; name; price} in
    let rep = Caqti_type.(tup3 int string int) in
    Caqti_type.custom ~encode ~decode rep
end

module Ingredient = struct
  type t = {
    id: int;
    item: int;
    ingredient: int;
  }
  let t =
    let encode {id; item; ingredient} = Ok (id, item, ingredient) in
    let decode (id, item, ingredient) = Ok {id; item; ingredient} in
    let rep = Caqti_type.(tup3 int int int) in
    Caqti_type.custom ~encode ~decode rep
end

let handleItems item acc =
  item :: acc

let allItemsQuery =
  Caqti_request.collect
    Caqti_type.unit
    Item.t
    "SELECT id, name, price FROM item"

let allItems pool =
  let query (module C: Caqti_lwt.CONNECTION) = C.rev_collect_list allItemsQuery () in
  Caqti_lwt.Pool.use query pool |> Database.or_error

let itemIngredientsQuery =
  Caqti_request.collect
    Caqti_type.int
    Caqti_type.int
    "SELECT id FROM ingredient WHERE item=?"

let itemIngredients pool itemId =
  let query (module C: Caqti_lwt.CONNECTION) = C.rev_collect_list itemIngredientsQuery itemId in
  Caqti_lwt.Pool.use query pool |> Database.or_error

let singleItemQuery =
  Caqti_request.collect
    Caqti_type.int
    Item.t
    "SELECT id, name, price FROM item WHERE id=?"

let singleItem pool id =
  let query (module C: Caqti_lwt.CONNECTION) = C.fold singleItemQuery handleItems id [] in
  Caqti_lwt.Pool.use query pool |> Database.or_error >|= function
    | Ok [] -> Ok None
    | Ok (item :: _) -> Ok (Some item)
    | Error _ as e -> e

let addItemQuery =
  Caqti_request.exec
    Caqti_type.(tup2 string int)
    "INSERT INTO item (name, price) VALUES (?, ?)"

let insertIdQuery =
  Caqti_request.find
    Caqti_type.unit
    Caqti_type.int
    "SELECT LAST_INSERT_ID() AS id"

let addItem pool (name, price) =
  let query (module C: Caqti_lwt.CONNECTION) =
    C.exec addItemQuery (name, price) >>= function
    | Ok () -> C.find insertIdQuery ()
    | Error _ as err -> Lwt.return err in
  Caqti_lwt.Pool.use query pool |> Database.or_error

let addIngredientQuery =
  Caqti_request.exec
    Caqti_type.(tup2 int int)
    "INSERT INTO ingredient (item, ingredient) VALUES (?, ?)"

let addIngredient pool ingredient =
  let query (module C: Caqti_lwt.CONNECTION) = C.exec addIngredientQuery ingredient in
  Caqti_lwt.Pool.use query pool |> Database.or_error

let item = Graphql_lwt.Schema.(obj "item"
  ~fields:(fun item -> [
    field "id"
      ~args:[]
      ~typ:(non_null int)
      ~resolve:(fun _ (p: Item.t) -> p.id);
    field "name"
      ~args:[]
      ~typ:(non_null string)
      ~resolve:(fun _ (p: Item.t) -> p.name);
    io_field "ingredients"
      ~args:[]
      ~typ:(non_null (list (non_null item)))
      ~resolve:(fun pool (p: Item.t) ->
        itemIngredients pool p.id >|= (function
          | Ok ingredients -> ingredients
          | Error (Database.Database_error e) -> failwith e
        ) >>= (
          Lwt_list.map_s (fun ingredient ->
            singleItem pool ingredient >|= function
              | Ok a -> a
              | Error (Database.Database_error e) -> failwith e
          )
        ) >|= (
          List.fold_left
            (fun acc maybeItem ->
              match maybeItem with
              | Some item -> item :: acc
              | None -> acc
            )
            []
        )
      );
    field "price"
      ~args:[]
      ~typ:(non_null int)
      ~resolve:(fun _ (p: Item.t) -> p.price);
  ])
)

let handleItemsResult itemsResult = match itemsResult with
| Ok items -> items
| Error (Database.Database_error e) -> failwith e

let query = Graphql_lwt.Schema.[
  io_field "items"
    ~args:[]
    ~typ:(non_null @@ list @@ non_null @@ item)
    ~resolve:(fun pool () ->
      allItems pool >|= handleItemsResult
    );
]

let mutation =
  Graphql_lwt.Schema.(
    let itemInput = Arg.(obj "addItemInput" ~coerce:(fun name price ingredients -> ((name, price), ingredients)) ~fields:[
      arg "name" ~typ:(non_null string);
      arg "price" ~typ:(non_null int);
      arg "ingredients" ~typ:(list (non_null int));
    ]) in
    [
      io_field "addItem"
        ~args:Arg.[
          arg "item" ~typ:(non_null itemInput)
        ]
        ~typ:int
        ~resolve:(fun pool () (item, ingredients) -> addItem pool item >>= (function
          | Ok id -> (match ingredients with
            | Some ingredients ->
                Lwt_list.iter_p (
                  fun ingredient -> addIngredient pool (id, ingredient) >|= function
                    | Ok () -> ()
                    | Error (Database_error err) -> failwith err
                ) ingredients >|= (fun () -> Some id)
            | None -> Lwt.return @@ Some id)
          | Error (Database.Database_error e) -> failwith e
        ))
    ]
  )
