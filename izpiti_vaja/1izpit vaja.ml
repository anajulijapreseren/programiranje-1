let odstej_trojici tr1 tr2 = 
  let (a,b,c) = tr1 in
  let (x,y,z) = tr2 in
  (a-x,b-y,c-z)


let rec max_rezultat_do_n f n = match n with
  |0 -> f(0) 
  |x -> max (f(x)) (max_rezultat_do_n f (x-1))
(*lepo se da narest z List.init*)

let max_rezultat_do_n' f n = 
  let rec aux trenutni_max trenutni_i =
    if trenutni_i < 0 then trenutni_max
    else (
      let v = f trenutni_i in
      aux (max v trenutni_max) (trenutni_i-1)
    )
  in
  aux (f n) (n-1)


let rec pocisti_seznam = function
  | []-> []
  | (Some x)::xs -> x::pocisti_seznam xs
  |_::xs -> pocisti_seznam xs
(*ta ni repno rekurzivna*)

let pocisti_2 l =
  let rec aux acc = function
  | [] -> List.rev acc
  |(Some x)::xs -> aux (x::acc) xs
  | _::xs -> aux acc xs
in aux [] l

let preveri_urejenost l = 
  let rec aux min_sodo max_liho = function
  | [] -> true
  | x::xs -> if x mod 2 = 0 then x > min_sodo && aux x max_liho xs else x < max_liho && aux min_sodo x xs in
  aux min_int max_int l

(*min in max int sta prav definirana v ocamlu:
min_int = The smallest representable integer*)

type 'a gnezdenje =
  | Element of 'a
  | Podseznam of 'a gnezdenje list

let gnezdenje_primer = 
  Podseznam [
    Element 1;
    Element 2;
    Podseznam (
      [Element 3; Podseznam [Element 4]; Podseznam []]
    );
    Podseznam [Element 5]
  ]

let rec najvecja_globina_gnezdenja g =
  match g with
  | Element _ -> 0
  | Podseznam xs -> 1 + (List.fold_left max 1 (List.map najvecja_globina_gnezdenja xs))

let najvecja_globina g_list = 
  1 + (List.fold_left max 1 (List.map najvecja_globina_gnezdenja g_list))

let rec preslikaj f g = match g with
  | Element x -> Element (f x)
  | Podseznam xs -> Podseznam ((List.map (preslikaj f)) xs)


let rec splosci = function
  |Element x -> [x]
  | Podseznam xs ->
    let splosceni = List.map splosci xs in
    List.fold_left (@) [] splosceni

let rec alternirajoci_konstrukorji = function
    |[] -> true
    |[x] -> true
    |Element _::Podseznam p::xs -> alternirajoci_konstrukorji ((Podseznam p)::xs)
    |Podseznam _:: Element p::xs -> alternirajoci_konstrukorji ((Element p)::xs)
    |_-> false

