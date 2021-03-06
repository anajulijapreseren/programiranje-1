(* ========== Vaja 3: Definicije Tipov  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)
type euro = Euro of float
type dollar = Dollar of float

<<<<<<< HEAD
let en_evro = Euro 1.0 (* Evro(1.0) *)

let razmerje = 0.84

let dollar_to_euro dollar = match dollar with
       |Dollar d -> Euro (d *. 0.84)

let dollar_to_euro (Dollar d) = Euro (d *. razmerje)
(*to je smiselno le ko imamo eno moznost*)

let euro_to_dollar (Euro e) = Dollar (e /. razmerje)
=======
type euro = Euro of float
type dollar = Dollar of float
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f

let dollar_to_euro (Dollar x) = Euro (x *. 0.861)
let euro_to_dollar (Euro x) = Dollar (x *. 1.161)

(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en vsotni tip z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)
type currency = 
       | Yen of float
       | Pound of float
       | Krona of float
       | Chf of float

let to_pound c = match c with
       | Pound x -> Pound x
       | Yen x -> Pound(x *. 0.007)
       | Krona x -> Pound (x *. 0.09)
       | Chf x -> Pound(x *. 2.0)

type currency = Yen of float | Pound of float | Krona of float

let to_pound = function
  | Pound x -> Pound x
  | Yen x -> Pound 0.007
  | Krona x -> Pound 0.085

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
type intbool_list = 
       | Empty 
       | Int of int * intbool_list
       | Bool of bool * intbool_list

let testni = Int(5,Bool(true,Bool(false,(Int(7, Empty)))))
=======
type intbool_list =
  | Int of int * intbool_list
  | Bool of bool * intbool_list
  | Nil

let test = Int(5, Bool(true, Bool(false, Int(7, Nil))))
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let rec intbool_map f_int f_bool ib_list = match ib_list with  
       | Empty -> Empty
       | Int (x, rep) -> Int(f_int x, intbool_map f_int f_bool rep)
       | Bool (x, rep) -> Bool(f_bool x, intbool_map f_int f_bool rep)
=======
let rec intbool_map f_int f_bool = function
  | Int(x, xs) -> Int(f_int x, intbool_map f_int f_bool xs)
  | Bool(x, xs) -> Bool(f_bool x, intbool_map f_int f_bool xs)
  | Nil -> Nil
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let intbool_reverse l= 
       let rec reverse_pomozna gradimo podiramo = match podiramo with
              | Empty -> gradimo
              | Int(x, rep) ->reverse_pomozna (Int(x, gradimo)), rep 
              | Bool(x, rep) ->reverse_pomozna (Bool(x, gradimo)), rep
       in
       reverse_pomozna Empty l

=======
let rec intbool_reverse ib_list =
  let rec ib_reverse acc = function
  | Int(x, xs) -> ib_reverse (Int(x, acc)) xs
  | Bool(x, xs) -> ib_reverse (Bool(x, acc)) xs
  | Nil -> acc
  in
  ib_reverse Nil ib_list
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let rec intbool_separate l= 
       let rec separation_pomozna inti booli l = match l with
              | Empty -> (inti, booli)
              | Int(x, rep) -> separation_pomozna (inti @ [x]) booli rep (*ker moramo dodati z desne, drugace x::inti*)
              | Bool(x, rep) -> separation_pomozna inti (booli @ [x]) rep
       (*to stikanje na koncu je slabo, boljse je dodati na zacetku in na l poklicati reverse*)
       in
       separation_pomozna [] [] l (*tu lahko napisem (intbool_reverse l)*)
=======
let rec intbool_separate ib_list =
  let rec ib_separate iacc bacc = function
  | Int(x, xs) -> ib_separate (x :: iacc) bacc xs
  | Bool(x, xs) -> ib_separate iacc (x :: bacc) xs
  | Nil -> (iacc, bacc)
  in
  ib_separate [] [] (intbool_reverse ib_list)
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic = Fire | Frost | Arcane

<<<<<<< HEAD
type magic = Fire | Frost | Arcane

type specialisation = Historian | Teacher | Researcher

=======
type specialisation = Historian | Teacher | Researcher
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f

(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)
type status = 
       | Newbie 
       | Student of magic * float 
       | Employed of magic * specialisation

<<<<<<< HEAD
type wizard = { name: string; status: status}

let professor = {name = "Matija"; status = Employed(Fire, Teacher)}
(*tu lahko status zapisemo tudi pred name in bo ok*)
=======
type status =
  | Newbie
  | Student of magic * int
  | Employed of magic * specialisation

type wizard = {name : string; status : status}

let professor = {name = "Matija"; status = Employed(Fire, Teacher)}

>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f
(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
type magic_counter = {fire: int; frost: int; arcane: int}

let update magic_counter magic_type = match magic_type with
       | Fire -> { fire = magic_counter.fire + 1; frost = magic_counter.frost; arcane = magic_counter.arcane}
       | Frost -> { magic_counter with frost=magic_counter.frost + 1 }
       (*popravi le to stvar, ki mu jo napišemo*)
       (*lahko mu daš več za popravit in ločiš z ;*)
       | Arcane -> {  magic_counter with arcane=magic_counter.arcane + 1 }
=======
type magic_counter = {fire : int; frost : int; arcane : int}

let update counter = function
  | Fire -> {counter with fire = counter.fire + 1}
  | Frost -> {counter with frost = counter.frost + 1}
  | Arcane -> {counter with arcane = counter.arcane + 1}

>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f
(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let rec count_magic l = 
       let rec count_pomozna trenutno delovni = match delovni with 
              | [] -> trenutno
              | x::rest -> 
                     let trenutni' = match x.status with
                            | Newbie ->trenutno
                            | Student (magic, _) -> update trenutno magic
                            | Employed (magic, _) -> update trenutno magic
                     in 
                     count_pomozna trenutni' rest
       in count_pomozna {fire=0; frost=0, arcane=0} l
=======
let count_magic wizard_list =
  let rec count counter = function
    | [] -> counter
    | {name; status} :: wizards -> (
        match status with
        | Newbie -> count counter wizards
        | Student (magic, _) -> count (update counter magic) wizards
        | Employed (magic, _) -> count (update counter magic) wizards)
  in count {fire = 0; frost = 0; arcane = 0} wizard_list

>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f
(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

<<<<<<< HEAD
let zahtevana_leta = function
       | Historian -> 3
       | Researcher -> 4
       | Teacher -> 5

let rec find_candidate magic specialization wizard_list = match wizard_list with
       | [] -> None
       | wizard::rest -> (
              match wizard.status with
              | Student (student_magic, years) when student_magic=magic-> (
                     if (zahtevana_leta specialisation) <= years then 
                            (Some wizard.name) 
                     else find_candidate magic specialization rest
              )
              | _ -> find_candidate magic specialization rest
       )
=======
let find_candidate magic specialisation wizard_list =
  let year =
    match specialisation with
    | Historian -> 3
    | Researcher -> 4
    | Teacher -> 5
  in
  let rec search = function
    | [] -> None
    | {name; status} :: wizards ->
        match status with
        | Student (m, y) when m = magic && y >= year -> Some name
        | _ -> search wizards
  in
  search wizard_list
>>>>>>> c2d130e450ac62cf1d9467eb2e13d4f9db10938f
