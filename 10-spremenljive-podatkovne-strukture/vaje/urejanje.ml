(* ========== Vaje 5: Urejanje  ========== *)


(*----------------------------------------------------------------------------*]
 Funkcija [randlist len max] generira seznam dolžine [len] z naključnimi
 celimi števili med 0 in [max].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let l = randlist 10 10 ;;
 val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let rec randlist len max = 
  if len <= 0 then 
    []
  else 
    Random.int max :: randlist (len - 1) max
  

=======
let rec randlist len max =
  if len <= 0 then
    []
  else
    Random.int max :: (randlist (len-1) max)
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Sedaj lahko s pomočjo [randlist] primerjamo našo urejevalno funkcijo (imenovana
 [our_sort] v spodnjem primeru) z urejevalno funkcijo modula [List]. Prav tako
 lahko na manjšem seznamu preverimo v čem je problem.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 let test = (randlist 100 100) in (our_sort test = List.sort compare test);;
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Vstavljanjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert y xs] vstavi [y] v že urejen seznam [xs] in vrne urejen
 seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 9 [0; 2];;
 - : int list = [0; 2; 9]
 # insert 1 [4; 5];;
 - : int list = [1; 4; 5]
 # insert 7 [];;
 - : int list = [7]
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let min_and_rest list = 
  let rec odstrani_preostanek x l = match l with
    | [] -> [] (*to se ne sme zgodit, ker bomo uporabili na nepraznih*)
    | (y::ys) -> 
      if x=y then
        ys
      else
        (y:: odstrani_preostanek x ys)
    in 
  match  list with 
  | [] -> None
  | x::xs ->  (*Najdemo minimum, nato odstranimo preostanek*)
    let min_trenutnega = List.fold_left min x xs in
    Some (min_trenutnega, odstrani_preostanek min_trenutnega (x::xs))



let rec insert x l = match l with
  | [] -> [x]
  | y::ys ->
    if x <= y then 
      x::l
    else y :: (insert x ys)
  
(*da bo zadeva stabilna moraš nujno dat <= in ne le <*)
=======
let rec insert y = function
  | [] -> [y]
  | x :: xs when y > x -> x :: (insert y xs)
  | x :: xs -> y :: x :: xs
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f

(*----------------------------------------------------------------------------*]
 Prazen seznam je že urejen. Funkcija [insert_sort] uredi seznam tako da
 zaporedoma vstavlja vse elemente seznama v prazen seznam.
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let rec insert_sort l = match l with
  | [] -> []
  | x::xs -> (insert x) (insert_sort xs)
=======
let ins_sort list = List.fold_left (fun acc x -> insert x acc) [] list
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f

(*lahko uporabimo fold(neki ne dela cist)*)
(*let insert_sort' l = List.fold_left (fun ze_sorted x -> insert x ze_sorted) [] l*)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [min_and_rest list] vrne par [Some (z, list')] tako da je [z]
 najmanjši element v [list] in seznam [list'] enak [list] z odstranjeno prvo
 pojavitvijo elementa [z]. V primeru praznega seznama vrne [None].
[*----------------------------------------------------------------------------*)

let min_and_rest list =
  let rec remove_one z = function
    | [] -> failwith "not found"
    | x :: xs -> if x = z then xs else x :: remove_one z xs
  in
  let rec find_min current_min = function
    | [] -> current_min
    | x :: xs -> find_min (min x current_min) xs
  in
  match list with
  | [] -> None
  | x :: xs ->
     let z = find_min x xs in
     Some (z, remove_one z (x :: xs))

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri urejanju z izbiranjem na vsakem koraku ločimo dva podseznama, kjer je prvi
 že urejen, drugi pa vsebuje vse elemente, ki jih je še potrebno urediti. Nato
 zaporedoma prenašamo najmanjši element neurejenega podseznama v urejen
 podseznam, dokler ne uredimo vseh.

 Če pričnemo z praznim urejenim podseznamom, vemo, da so na vsakem koraku vsi
 elementi neurejenega podseznama večji ali enaki elementom urejenega podseznama,
 saj vedno prenesemo najmanjšega. Tako vemo, da moramo naslednji najmanjši člen
 dodati na konec urejenega podseznama.
 (Hitreje je obrniti vrstni red seznama kot na vsakem koraku uporabiti [@].)
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort] je implementacija zgoraj opisanega algoritma.
 Namig: Uporabi [min_and_rest] iz prejšnje naloge.
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let rec selection_sort l = 
  match min_and_rest l with
    | None -> []
    | Some (mini, tail) -> mini :: selection_sort tail
(*ni tail recursive, se pa da napisat*)
=======
let selection_sort list =
  let rec aux sorted unsorted =
    match min_and_rest unsorted with
    | None -> List.rev sorted
    | Some (x, unsorted') -> aux (x::sorted) unsorted'
  in aux [] list
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem na Tabelah
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri delu z tabelami (array) namesto seznami, lahko urejanje z izbiranjem
 naredimo "na mestu", t.j. brez uporabe vmesnih kopij (delov) vhoda. Kot prej
 tabelo ločujemo na že urejen del in še neurejen del, le da tokrat vse elemente
 hranimo v vhodni tabeli, mejo med deloma pa hranimo v spremenljivki
 [boundary_sorted]. Na vsakem koraku tako ne izvlečemo najmanjšega elementa
 neurejenga dela tabele temveč poiščemo njegov indeks in ga zamenjamo z
 elementom na meji med deloma (in s tem dodamo na konec urejenega dela).
 Postopek končamo, ko meja doseže konec tabele.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [swap a i j] zamenja elementa [a.(i)] and [a.(j)]. Zamenjavo naredi
 na mestu in vrne unit.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let test = [|0; 1; 2; 3; 4|];;
 val test : int array = [|0; 1; 2; 3; 4|]
 # swap test 1 4;;
 - : unit = ()
 # test;;
 - : int array = [|0; 4; 2; 3; 1|]
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let swap a i j = 
  let z = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- z
=======
let swap a i j =
  let v = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- v
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f

(*----------------------------------------------------------------------------*]
 Funkcija [index_min a lower upper] poišče indeks najmanjšega elementa tabele
 [a] med indeksoma [lower] and [upper] (oba indeksa sta vključena).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 index_min [|0; 2; 9; 3; 6|] 2 4 = 3
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let index_min a lower upper = 
  let trenutni_index_min = ref lower in
  let min_value = ref a.(!trenutni_index_min) in 
  for i = lower to upper do (*v ocamlu naredi tudi upper)*)
    if a.(i) < a.(!trenutni_index_min) then 
      trenutni_index_min := i(*do vrednosti reference pridemo s !*)
    else
      ()
  done;
  !trenutni_index_min
=======
let index_min a lower upper =
  let index_min = ref lower in
  for i = lower to upper do
    if a.(i) < a.(!index_min) then
      index_min := i
  done;
  !index_min

>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f
(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort_array] implementira urejanje z izbiranjem na mestu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Namig: Za testiranje uporabi funkciji [Array.of_list] in [Array.to_list]
 skupaj z [randlist].
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let selection_sort_array a = 
  let end_index = (Array.length a) - 1 in 
  for urejen_index = 0 to end_index do
    let in_min = index_min a urejen_index end_index in
    swap a urejen_index in_min
  done
=======
let selection_sort_array a =
  let index_end = Array.length a - 1 in
  (* Every step moves boundary_sorted one place to the right. *)
  for boundary_sorted = 0 to index_end do
    let i = index_min a boundary_sorted index_end in
    swap a i boundary_sorted
  done

let selection_sort_list list =
  (* For testing purposes. *)
  let a = Array.of_list list in
  selection_sort_array a;
  Array.to_list a
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f
