(************************************)
(******* Zadanie: Przelewanka *******)
(****** autor: Marek Drozdowski *****)
(** Code reviewer : Marcin Wawerka **)
(************************************)
open Queue;;

let rec nwd a b = 
	if b = 0 then a
	else nwd b (a mod b)

(* jeśli nwd a1 a2 a3 ... an <> nwd b1 b2 b3 ... bn a1 a2 ... an to brak rozwiązań *)
let tab_nwd tab = 
	Array.fold_left (fun x a -> nwd x a) 0 tab 
(* iloczyn elementów plus jeden tablicy vmax to będzie liczba możliwych stanów *)		
let iloczyn tab = 
	Array.fold_left (fun a x -> a * (x+1)) 1 tab
		
let przelewanka taba =
	let tab = Array.of_list (List.filter (fun a -> a <> (0,0)) (Array.to_list taba)) in
	let n = Array.length tab in 
	let vmax = Array.init n (fun i -> fst tab.(i)) in 
	let used = Hashtbl.create 1
	and result_array = Array.init n (fun i -> snd tab.(i)) in
	(*let res = haszuj result_array vtab in*)
	let que = (create ()) and wynik = ref (-1) in
	if (tab_nwd vmax) = nwd (tab_nwd result_array) (tab_nwd vmax) then
	begin
		add (Array.make n 0) que;
		Hashtbl.add used (Array.make n 0) 0;
		while ( !wynik = -1 && is_empty que = false ) do
			let vobec = pop que in
			let resu = Hashtbl.find used vobec in
			if vobec = result_array then (wynik := resu;)
			else
			let akt = Array.copy vobec in
			begin
			for i = 0 to n-1 do
				akt.(i) <- vmax.(i);
				if Hashtbl.mem used akt = false then begin
					add (Array.copy akt) que; 
					Hashtbl.add used (Array.copy akt) (resu+1);
				end;
				akt.(i) <- 0;
				if Hashtbl.mem used akt = false then begin
					add (Array.copy akt) que; 
					Hashtbl.add used (Array.copy akt) (resu+1);
				end;
				akt.(i) <- vobec.(i);
			done; 
			for i = 0 to n-1 do
				for j = 0 to n-1 do
					if i <> j then begin
					akt.(j) <- min (vobec.(i) + vobec.(j)) vmax.(j);
					akt.(i) <- max 0 (vobec.(i) + vobec.(j) - vmax.(j));
					if Hashtbl.mem used akt = false then begin
						add (Array.copy akt) que; 
						Hashtbl.add used (Array.copy akt) (resu+1);
					end;
					akt.(j) <- vobec.(j);
					akt.(i) <- vobec.(i);
					end;
				done;
			done;
		end		 
		done;	
	end;  
	!wynik

(*******************)
(****** Testy ******)
(*******************)

(*Nie ma rozwiązania*)
let c = [|(10,2);(20,20);(10,0);(1000,1000)|];;
assert ( przelewanka c = -1 );;
let c = [|(3,2);(5,4);(5,2);(6,1)|];;
assert (przelewanka c = -1);;
let c = [|(40,1);(10,4);(23,2);(40,1)|];;
assert (przelewanka c = -1);;
let c = [|(12,2);(6,3);(4,4);(10,2)|];;
assert (przelewanka c = -1);;
let c = [|(14,3);(3,1)|];;
assert (przelewanka c = -1);;

(*Testy różne*)
let c = [|(3,2);(3,3);(1,0);(12,1)|];;
assert ( przelewanka c = 4 );;
let c = [|(1,1);(100,99)|];;
assert ( przelewanka c = 2 );;
let c = [|(3,3);(5,4);(5,2);(6,1)|];;
assert (przelewanka c = 6);;
let c = [|(100,3);(2,1);(1,0);(6,1)|];;
assert (przelewanka c = 7);;
let c = [|(3,3);(5,5);(5,5);(6,6)|];;
assert (przelewanka c = 4);;
let c = [|(40,20);(20,10);(10,5);(5,0)|];;
przelewanka c ;;
let c = [|(19,3);(1,1);(2,2)|];;
assert (przelewanka c = 6);;
let c = [|(14,3);(3,1);(3,0)|];;
assert (przelewanka c = 13);;
let c = [|(3,3);(4,0);(1,1);(6,6)|];;
assert (przelewanka c = 3);;
let c = [|(46,20);(23,10);(13,5);(5,0)|];;
assert (przelewanka c = 10);;
let c = [|(18,3);(3,1);(2,2)|];;
assert (przelewanka c = 4);;
let c = [|(14,3);(5,1)|];;
assert (przelewanka c = -1);;
let c = [|(14,3);(5,1);(5,0)|];;
assert (przelewanka c = 16);;

(* Przelewanie ciągle z jednego do drugiego*)
let c = [|(10000,5000);(1,0)|];;
assert (przelewanka c = 10000);;
let c = [|(50000,450);(3,1);(3,0)|];;
assert (przelewanka c = 33635);;
let c = [|(100000,25252);(2,2)|];;
assert (przelewanka c = 25253);;
(**open Przelewanka *)

let bad = ref 0
let total = ref 0

exception Repeated_id

let test =
  let ids = Hashtbl.create 0 in
  fun id input expected ->
    if Hashtbl.mem ids id then
      raise Repeated_id;
    Hashtbl.replace ids id ();
    Printf.printf "Test %d: ... %!" id;
    incr total;
    let start = Sys.time () in
    let ans = przelewanka input in
    let stop = Sys.time () in
    Printf.printf "%f " (stop -. start);
    if ans <> expected then begin
      incr bad;
      Printf.printf "ZLE: oczekiwano %d, otrzymano %d" expected ans
    end else
      Printf.printf "OK";
    Printf.printf "\n%!"
;;

let () =
  (* przypadki brzegowe, zera *)
  test 101 [||] 0;
  test 102 [|(1, 0)|] 0;
  test 103 [|(1, 1)|] 1;
  test 104 [|(0, 0)|] 0;
  test 105 [|(0, 0); (0, 0); (0, 0)|] 0;
  test 106 [|(10, 5)|] (~-1);

  (* testy na brak zalozen o wielkosci liczb *)
  test 201 [|(1000000, 999999); (1, 1)|] 2;
  test 202 [|(1, 1); (10000, 5000)|] 10000;
  
  (* niektore sytuacje z testow do "Mokrej roboty"
   * dla kazdej sytuacji mamy tutaj przynajmniej jeden test prawie maksymalny
   * (najdalsza konfiguracja przy danych pojemnosciach naczyn) *)
  test 301 [|(3, 0); (5, 0); (5, 5)|] 1;
  test 302 [|(3, 1); (5, 0); (5, 4)|] 8;
  test 303 [|(3, 0); (5, 1); (5, 1)|] 11;

  test 304 [|(5, 0); (7, 1); (7, 6)|] 15;
  test 305 [|(5, 5); (7, 1); (7, 1)|] 16;
  test 307 [|(5, 0); (7, 1); (7, 1)|] 17;

  test 308 [|(15, 1); (18, 0); (22, 21)|] 8;
  test 309 [|(15, 2); (18, 0); (22, 20)|] 17;

  test 310 [|(1, 1); (2, 1); (3, 2); (4, 3)|] 5;
  test 311 [|(1, 1); (2, 2); (3, 3); (4, 4)|] 4;

  test 312 [|(30, 0); (17, 15)|] 45;
  test 313 [|(30, 15); (17, 0)|] 46;

  test 314 [|(26, 26); (28, 4); (30, 28); (32, 2)|] 7;
  test 315 [|(26, 12); (28, 14); (30, 30); (32, 18)|] 21;

  test 316 [|(21, 10); (22, 22); (23, 10); (24, 13)|] 30;
  
  test 317 [|(31, 0); (33, 3); (35, 5); (37, 22)|] 41;
  test 318 [|(31, 0); (33, 32); (35, 34); (37, 3)|] 47;

  (* inne wymyslone *)
  test 401 [|(9973, 3); (9967, 0)|] 19938;
  (* (13, 6) - podzbior *)
  test 402 (Array.init 13 (fun i -> (1, i mod 2))) 6;
  (* duzo zer *)
  test 403 (Array.init 70 (fun i ->
    if i = 0 then
      (1, 0)
    else if (i mod 30) = 0 then
      (10, 5)
    else
      (0, 0)
  )) 11;
  (* ogromna przestrzen, ale maly wynik *)
  test 404 (Array.init 35 (fun i ->
    (i * i * i * i * i, if i = 25 then 20 * 20 * 20 * 20 * 20 else 0))
  ) 2;
  (* parami nwd nie 1, ale w sumie juz tak *)
  test 405 [|(6, 6); (10, 3); (15, 14)|] 12;

  (* przestrzeń stanów rośnie liniowo z rozmiarem zadania;
     rozmiar zadania rośnie wykładniczo w kolejnych testach *)
  let n = ref 1 in
  for i = 1 to 6 do
    n := !n * 10;
    Gc.compact ();
    let minor_words, promoted_words, major_words = Gc.counters () in
    test (i+420) [|(!n, !n/2); (!n-1, 0)|] (2 * !n - 2);
    let minor_words2, promoted_words2, major_words2 = Gc.counters () in
    Printf.printf "Zużycie pamięci: %.1f %.1f %.1f\n" 
      (minor_words2-.minor_words) (promoted_words2-.promoted_words) (major_words2-.major_words)
  done;
  
  (* przestrzeń stanów wynosi 2 ^ rozmiar zadania;
     rozmiar zadania liniowo w kolejnych testach,
     test ma na celu wykrywać programy, które optymalizują wykorzystanie pamięci
     np. stosujac algorytm iteracyjnego pogłębiania *)
  for n = 1 to 15 do
    Gc.compact ();
    let minor_words, promoted_words, major_words = Gc.counters () in
    test (n+450) (Array.make n (1,1)) n;
    let minor_words2, promoted_words2, major_words2 = Gc.counters () in
    Printf.printf "Zużycie pamięci: %.1f %.1f %.1f\n" 
      (minor_words2-.minor_words) (promoted_words2-.promoted_words) (major_words2-.major_words)
  done;  

  Printf.printf("Testy bonusowe\n");
  (* testy optymalizacji na -1, sprawdzanie nwd *)
  test 501 [|(20, 10); (18, 18); (16, 10); (14, 7)|] (~-1);
  test 502 (Array.init 1000 (fun i ->
    if i = 500 then
      (2 * i, 315)
    else
      (2 * i, 2 * i)
  )) (~-1);

  (* testy optymalizacji "choc jeden pelny/pusty" *)
  test 601 [|(20, 19); (18, 13); (15, 12); (10, 1)|] (~-1);
  test 602 (Array.init 1000 (fun i -> (i + 2, i / 2 + 1))) (~-1);

  Printf.printf "\n";
  if !bad = 0 then begin
    Printf.printf "Testy OK!\n"
  end else begin
    Printf.printf "Testy ZLE: %d/%d.\n" !bad !total;
    exit 1;
  end
;;
