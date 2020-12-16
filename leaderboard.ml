type data = Float of float | String of string

(** AF: [["col1";"col2"]; [row1_v1; row_1_v2]; [row2_v1; row2_v2]] is the table
    with column names, "col1" and "col2" where each column has two data entries.
    Data is stored by rows. 
    RI: There are no duplicate column names. All entires in a column must all
    be either of type float or string, so [data] is a variant of either float 
    or string. There are no empty cells. No data entry can have the same name 
    as a column label. *)
type t = data list list

let empty = []

(** [get_labels] is the list of column labels. *)
let get_labels = function 
  |[] -> [] 
  |h::t -> h 

(** [get_data] is the table without column labels. *)
let get_data = function 
  |[] -> [] 
  |h::t -> t 

(** [str_lst_to_data] is the data list representation of a string list. *)
let str_lst_to_data tbl : data list = 
  let rec str_list_helper acc = function 
    |[] -> List.rev acc 
    |h::t -> str_list_helper 
               ((try Float (float_of_string h) with 
                   |Failure _ -> String h)::acc) t 
  in 
  str_list_helper [] tbl

(** [table_from_csv] is the table representation of the csv file of the 
    format string list list. *)
let table_from_csv csv : t = 
  let rec tbl_helper (tbl : t) = function 
    |[] -> List.rev tbl
    |h::t -> tbl_helper ((str_lst_to_data h)::tbl) t
  in 
  tbl_helper [] csv

let read_leaderboard = 
  let filename = "leaderboard.csv" in 
  filename |> Csv.load |> table_from_csv 

(** [string_of_list] converts a row of the table to a string list. *)
let data_lst_to_string lst : string list = 
  let rec string_helper  acc = function
    |[] ->  List.rev acc
    |(Float f)::t->  string_helper ((string_of_float f)::acc) t
    |(String s)::t -> string_helper (s::acc) t 
  in 
  string_helper [] lst 

(** [csv_from_table] is the string list list reprsentation of a table. *)
let csv_from_table tbl : string list list= 
  let rec csv_helper  csv = function
    |[] -> List.rev csv 
    |h::t -> csv_helper ((data_lst_to_string h)::csv) t
  in csv_helper [] tbl

let write_leaderboard t = 
  let filename = "leaderboard.csv" in 
  t |> csv_from_table |> Csv.save filename

let display t = csv_from_table t

let add_player_score str pts t = 
  (get_labels t)::[String str; Float pts]::(get_data t)

let sort_leaderboard t = 
  failwith "unimplemented"

(** [list_of_array_rows] is a helper function that converts the table into a 
    string array list.  *)
let list_of_array_rows tbl : data array list=  
  let rec arr_helper arr_lst = function
    |[] -> List.rev arr_lst
    |h::t -> arr_helper ((Array.of_list (h))::arr_lst) t
  in 
  arr_helper [] tbl

(** [arr_lst_to_data_lst] is the representation of a string array list as a 
    Table.t. *)
let arr_lst_to_data_lst lst = 
  let rec data_helper acc = function
    |[] -> List.rev acc
    |h::t -> data_helper ((Array.to_list h)::acc) t
  in 
  data_helper [] lst

let sort t str = 
  let sorted_vals = List.sort 
      (fun a b -> match (a.(1), b.(1)) with 
         |(Float f1, Float f2) -> (-1) * (compare f1 f2)
         |(String s1, String s2) -> (-1) * (compare s1 s2)
         |_ -> 0) 
      (list_of_array_rows (get_data t)) in
  (get_labels t)::(arr_lst_to_data_lst sorted_vals)