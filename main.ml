open Curses


let noir = 0
let gris = noir+1
let blanc = gris+1
let rouge = blanc+1
let rouge_clair = rouge+1
let vert = rouge_clair+1
let vert_clair = vert+1
let bleu = vert_clair+1
let bleu_clair = bleu+1


let ncolors = bleu_clair + 1

let _ = Random.init (int_of_float (Unix.gettimeofday ()))

let cree_couleurs () =
    assert(init_color noir 0 0 0);
    assert(init_color gris 500 500 500);
    assert(init_color blanc 1000 1000 1000);
    assert(init_color rouge 1000 0 0);
    assert(init_color rouge_clair 1000 500 500);
    assert(init_color vert 0 1000 0);
    assert(init_color vert_clair 500 1000 500);
    assert(init_color bleu 0 0 1000);
    assert(init_color bleu_clair 500 500 1000)

let _ =
    let w = initscr () in
    assert(nodelay w true);
    assert(keypad w true);
    assert (start_color ());
    assert (cbreak ());
    assert (noecho ())

let cree_paires () =
    let paires = Array.make_matrix ncolors ncolors 0 in
    let p = ref 10 in
    for i = 0 to ncolors-1 do
        for j = 0 to ncolors-1 do
            assert(init_pair !p i j);
            paires.(i).(j) <- !p;
            incr p
        done
    done;
    paires

let paires = 
    (* cree des couleurs et toutes les paires *)
    cree_couleurs ();
    cree_paires ()

let couleur texte fond =
    attron (A.color_pair paires.(texte).(fond))

(* affiche un pixel *)
let putpixel col x y =
    couleur col col;
    ignore(mvaddch y x (int_of_char ' '))

(* on peut alors dessiner directement *)
let ligne_horiz col x1 x2 y =
    for x = x1 to x2 do
        putpixel col x y
    done

let ligne_vert col x y1 y2 =
    for y = y1 to y2 do
        putpixel col x y
    done

let boite col x1 y1 x2 y2 =
    ligne_horiz col x1 x2 y1;
    ligne_horiz col x1 x2 y2;
    ligne_vert col x1 y1 y2;
    ligne_vert col x2 y1 y2

let _ =
    let w = initscr () in
    assert(nodelay w true);
    assert(keypad w true);
    assert (start_color ());
    assert (cbreak ());
    assert (noecho ())

type joueur = {
    mutable hp : int;
    mutable hp_max : int;
    mutable score : int;
    mutable score_streak : int;
    mutable state : char;
}

let player hp score= {hp = hp; hp_max=hp; score = score; score_streak = 0; state = 'a'}

type particule = {
    mutable x : int;
    mutable y : int;
    mutable speed : int;
    mutable color : int;
    mutable compteur : int;
}

let particule x speed col = { x = x; y = 9999; speed = speed; color = col; compteur = 0}

let tick_particules (particules: particule array ref) =
    let h,w = get_size() in
    for i = 0 to Array.length !particules - 1 do
        !particules.(i).compteur <- !particules.(i).compteur +1;

        if !particules.(i).compteur >= !particules.(i).speed then begin
            !particules.(i).compteur <- 0;
            !particules.(i).y <- !particules.(i).y +1;

            if !particules.(i).y > h then begin
                !particules.(i).y <- 0;
                !particules.(i).x <- (Random.int 9999) mod w;
     (*couleur*)!particules.(i).color <- ((Random.int 9999) mod (ncolors-2)) +1;
                
            end;
        end;
        putpixel !particules.(i).color !particules.(i).x !particules.(i).y;
    done

let affichage_end_level result joueur = 
    let h, w = get_size () in
    if result = "lose" then begin
        couleur rouge noir;
        ignore (mvaddstr (h/2) (w/2-10) (Printf.sprintf "YOU LOSE, final score :"));
        ignore (mvaddstr (h/2+2) (w/2-25) (Printf.sprintf "Well, it doesn't really matter you lost anyway-"));
    end
    else begin
        couleur vert noir;
        ignore (mvaddstr (h/2) (w/2-8) (Printf.sprintf "YOU WON, final score : %d" joueur.score))
    end 

let load_level (level : int) : string list=
    let nom_dossier = ref "levels/level" in
    nom_dossier := !nom_dossier ^ (string_of_int level);
    nom_dossier := !nom_dossier ^ ".txt";
    
    let lignes = ref [] in
    let file = open_in !nom_dossier in

    try
        while true do
            lignes := input_line file :: !lignes
        done;
        !lignes
    with End_of_file -> close_in file;
        
    List.rev !lignes;;

let manual_tri tab =
    match tab with
    |t::q -> if t = "00000" then q else tab
    |[] -> []

let hit (joueur : joueur ref) = 
    (*La fonction qui agit quand le joueur marque un point*)
    let ajout = ref 0 in
    !joueur.score <- !joueur.score + 100 * !joueur.score_streak;
    !joueur.score_streak <- !joueur.score_streak + 1;
    if !joueur.hp >=200 then ajout := 0 else ajout := 50;
    !joueur.hp <- (!joueur.hp + !ajout)
    (*manual_tri tab*)

let fail (joueur : joueur ref) =
    (*La fonction qui agit quand le joueur fait une erreur*)
    begin
      if !joueur.hp>80 then 
      begin
        !joueur.hp <- (!joueur.hp - 80);
        !joueur.score_streak <- 0;
      end
      else
      !joueur.state <- 'm';
     end

let touche_valide touche : bool =
    match touche with
    | 102 -> true
    | 103 -> true
    | 104 -> true
    | 106 -> true
    | 107 -> true
    | _   -> false

let indice_of_touche touche : int =
    match touche with
    | 102 -> 0
    | 103 -> 1
    | 104 -> 2
    | 106 -> 3
    | 107 -> 4
    | _   -> 999

let verif_tile (tab : string list) (joueur : joueur ref) touche : string list= 
    
    let ligne_finie = ref false in
    let tete = ref (List.hd tab) in

    if touche > 0 then begin

        let tete_bytes = ref (String.to_bytes (List.hd tab)) in
        let valide = ref false in
        if touche_valide touche then begin
            let i = indice_of_touche touche in
                
            if !tete.[i] = '1' then begin

                valide := true;
                Bytes.set !tete_bytes i '0';

            end;
        end;
        if !valide then begin
            hit joueur;
            tete := Bytes.to_string !tete_bytes;
            if !tete = "00000" then ligne_finie := true;

        end else
            fail joueur;
        
    end;
    if !ligne_finie then List.tl tab
    else [!tete] @ List.tl tab

let affichage_ligne_level ligne x =
    let h,w = get_size () in
    let d_x = 5 in
    for i = 0 to String.length ligne - 1 do
        let c = ref 0 in
        if ligne.[i] = '1' then c := bleu
        else c := blanc;
        ligne_horiz !c (w/2+3*i-d_x) (1+w/2+3*i-d_x) (6+h/2-2*x);
    done

let affichage_tab tab selection =
    let size = List.length tab in
    couleur bleu noir;
    clear();
    let h, w = get_size () in
    for i= 0 to size-1 do
        let hd = List.nth tab i in
        couleur bleu noir;
        ignore (mvaddstr (h/2+16) (5+w/2-8) (Printf.sprintf "f g h j k" ));
        affichage_ligne_level hd i;
    done;
    couleur rouge noir;
    ignore (mvaddstr (h-2) (w-7) (Printf.sprintf "Level %d" selection))

    let affichage_hud joueur = 
        let h, w = get_size () in
        couleur rouge vert;
        ignore (mvaddstr (h/10) (w/10) (Printf.sprintf "HP : %d" joueur.hp));
        ignore (ligne_horiz vert (13) (13+(joueur.hp*w)/250) (h-2));

        couleur bleu blanc;
        ignore (mvaddstr (h/10) (w-20) (Printf.sprintf "Score : %d" joueur.score));
        couleur rouge blanc;
        ignore (mvaddstr (h-6) (w-20) (Printf.sprintf "ScoreStreak ! : X %d" joueur.score_streak))

let print_title () =
    (*  let ligne_horiz col x1 x2 y =
        let ligne_vert col x y1 y2 =   *)
    let h, w = get_size () in
    let xo = w/2-24 in
    let yo = h/2-13 in
    (* T *)
    ligne_horiz rouge (2+xo) (15+xo) (yo+7); ligne_vert rouge (xo+8) (yo+8) (yo+15); ligne_vert rouge (xo+9) (yo+8) (yo+15);
    (* I *) 
    ligne_vert rouge (xo+18) (yo+7) (yo+8); 
    ligne_vert rouge (xo+19) (yo+7) (yo+8);
    ligne_vert rouge (xo+18) (yo+10) (yo+15); 
    ligne_vert rouge (xo+19) (yo+10) (yo+15); 
    (* N *)
    ligne_vert rouge (xo+22) (yo+7) (yo+15); ligne_vert rouge (xo+23) (yo+7) (yo+15); ligne_vert rouge (xo+31) (yo+7) (yo+15); 
    putpixel rouge (xo+24) (yo+8); putpixel rouge (xo+25) (yo+9); putpixel rouge (xo+26) (yo+10); 
    putpixel rouge (xo+27) (yo+11); putpixel rouge (xo+28) (yo+12);putpixel rouge (xo+29) (yo+13); 
    putpixel rouge (xo+30) (yo+14); ligne_vert rouge (xo+32) (yo+7) (yo+15);
    (* C *)
    ligne_horiz rouge (xo+35) (xo+36) (yo+10); ligne_horiz rouge (xo+36) (xo+37) (yo+9); ligne_horiz rouge (xo+37) (xo+39) (yo+8); 
    ligne_horiz rouge (xo+39) (xo+44) (yo+7);ligne_horiz rouge (xo+35) (xo+36) (yo+11); ligne_horiz rouge (xo+35) (xo+36) (yo+12); 
    ligne_horiz rouge (xo+36) (xo+37) (yo+13); ligne_horiz rouge (xo+37) (xo+39) (yo+14); ligne_horiz rouge (xo+39) (xo+44) (yo+15)


(* ------------------------------------------ MAIN FUNCTION --------------------------------- *)

let _ =
    (* variables generales *)
    let h, w = get_size () in
    ignore (h);
    let nb_levels = 3 in
    let running = ref true in
    let state = ref 't' in
    let touche = ref 0 in

    let joueur = ref (player 199 0) in
        !joueur.hp <- !joueur.hp +1;

    let selection = ref 1 in
    let particules = ref [|(particule (Random.int w) (Random.int 7) ((Random.int (ncolors -1))+1))|] in

    (* variables temporaires *)
    let ch = ref 0 in
    
    (* variables level *)
    let in_game = ref false in

    let tab = ref [] in
    let result = ref "lose" in
    (*let osef = ref true in*)

    let com_particules = ref 0 in
    attroff(A.color);
    (* ----------- main loop ---------- *)
    while !running do
    try
        clear();
        let h, w = get_size () in

        if !com_particules < w/2 then begin
            particules := Array.append !particules [|(particule (Random.int w) (Random.int 7) ((Random.int (ncolors -1))+1))|];
            com_particules := !com_particules +1;
        end;
        if not !in_game then tick_particules particules;

        if !state = 't' then begin

            print_title ();
            couleur blanc noir;
            ignore (mvaddstr (h/2+8) (w/2-10) (Printf.sprintf "nombre : %d" !ch));
            ignore (mvaddstr (h/2+6) (w/2-10) (Printf.sprintf "Press space to continue"));
        

        end
        else if !state = 'l' then begin
            for i = 0 to h do
                ligne_horiz noir (w/2-16) (w/2+16) i
            done;
            couleur rouge noir;
            ignore (mvaddstr (h/2-4) (w/2-8) (Printf.sprintf "Choose your level"));

            for i = 1 to nb_levels do
                if !selection = i then couleur vert noir else couleur blanc noir;
                ignore (mvaddstr (h/2) (w/2-(!selection)*8 - (nb_levels-i*8) +1 ) (Printf.sprintf "level %d" i));
            done;


        end
        else if !state = 'g' then begin
            if not !in_game then begin

                tab := load_level !selection;
                in_game := true;

            end else begin
                (* ------------------- level loop --------------------- *)
                if !joueur.state = 'a' then begin
                    !joueur.hp <- !joueur.hp -1;
                    if !joueur.hp <= 0 then state := 'r';
                    try
                        tab := verif_tile !tab joueur !touche;

                        for i = 0 to h do
                            ligne_horiz gris (w/2-10) (w/2+10) i;
                        done;

                        affichage_tab !tab !selection;
                        
                        affichage_hud !joueur;
                        if List.length !tab = 0 then result := "win";
                    with Failure a -> begin (*fin level*)
                        affichage_end_level !result !joueur; 
                        ignore (mvaddstr (h-6) (w-55) (Printf.sprintf "Wanna retry ? Press Y"));
                        ignore a;
                        end
                end else begin
                    state := 'r'
                end;
                
            end;
        end
        else if !state = 'r' then begin
            couleur rouge noir;
            ignore (mvaddstr (h-6) (w-55) (Printf.sprintf "Wanna retry ? Press Y"));
            affichage_end_level "lose" !joueur;
        end;
        
        

        Unix.sleepf 0.05;
        ignore(refresh());

        touche := getch ();
        if !touche >= 0 then begin
            match !touche with
            | 27 -> running := false;
  (*espace*)| 32 -> if !state = 't' then state := 'l'
               else if !state = 'l' then state := 'g' 
    (*left*)| 260 -> if !state = 'l' && !selection > 1 then selection := !selection -1
   (*right*)| 261 -> if !state = 'l' && !selection < nb_levels then selection := !selection +1
   (*enter*)| 10 -> if !state = 'l' then state := 'g';
            | 121 -> if !state = 'r' || !result = "win" then begin
                state := 'g';
                !joueur.state <- 'a';
                !joueur.hp <- 200;
                !joueur.score <- 0;
                in_game:=false;
            end;
            | _ -> ()
            
        end;
        if !touche <> -1 then ch := !touche;


        
    with e -> begin 
        running := false;
        Printf.printf "%s\n" (Printexc.to_string e);
        end;
    done;
    
    
    endwin ()
    