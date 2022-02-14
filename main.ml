open Random

type joueur = {
    mutable hp : int;
    mutable hp_max : int;
    mutable score : int;
    mutable score_streak : float;
}

let actual = ref 0;

let create_player hp = 
    {hp = hp; hp_max=hp; score = 0; score_streak = 0};

let read = 
    ["1","0","0","0","0"];

(*let read_next =*) 

    (*Ici faudra mettre toute la partie du code qui lit la prochaine ligne et 
    modifie la liste map *)

let lose =
    Printf.sprintf "Et c'est perdu !";

let win =
    Printf.sprintf "Et c'est gagné !";

let hit = 
    (*La fonction qui agit quand le joueur marque un point, oui c'était assez évident *)
    joueur.score <- joueur.score + 100 * joueur.score_streak;
    joueur.score_streak <- joueur.score_streak +. 0.02;
    if joueur.hp < 500 then joueur.hp <- joueur.hp + 50 else joueur.hp <- joueur.hp + 3
    read_next;

let fail = 
    (*Soit le joueur a encore des hp et il en perd, soit il a perdu*)
    if joueur.hp>80 then joueur.hp <- joueur.hp-80 else lose;

let tab = read in

let verif_tile tab actual = 
    let touche = wait_next_event [Key_pressed] in
    let liste_act = ref tab.(actual);
    let yes = ref false;
    for i=0 to Array.length(tab.actual) - 1 do 
        if tab.(actual).(i) = touche then hit else fail
    done;

